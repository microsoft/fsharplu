/// F# helpers to the Azure Compute API
/// Copyright (c) Microsoft Corporation.
module Microsoft.FSharpLu.Azure.VM

open System
open Microsoft.Azure.Storage
open Microsoft.FSharpLu
open Microsoft.FSharpLu.Async
open Microsoft.Azure.Management.Compute
open Microsoft.Azure.Management.Compute.Models
open Microsoft.Azure.Management.ResourceManager
open Microsoft.Azure.Management.ResourceManager.Models
open Microsoft.Azure.KeyVault
open Microsoft.Rest.Azure
open Microsoft.FSharpLu.Azure
open Microsoft.FSharpLu.Azure.Context
open Microsoft.FSharpLu.Azure.AppInsights
open Microsoft.FSharpLu.Azure.Request
open Microsoft.FSharpLu.Azure.ResourceGroup
open Microsoft.FSharpLu.ErrorHandling

type ComputeSubResource = Microsoft.Azure.Management.Compute.Models.SubResource

type ComputeResource = Microsoft.Azure.Management.Compute.Models.Resource

module Constants =
    /// Time interval between two retry attempts of a blob copy
    let BlobCopyRetryPeriod = TimeSpan.FromSeconds(20.0)

    /// Maximum attempts at copying a blob
    let BlobCopyMaxiumAttempts = 3

    /// Directory where virutal machines' disk VHDS are stored under each storage account
    let VhdDisksContainer = "vhds"

    /// Vault key name prefix for the RDP certificate
    let CertificateKeyNamePrefix = "vmremote-certificate-"

    /// Azure storage container where captured images are stored
    let CapturedImagesContainer = "system"

    /// Name of the vault key containing the VM remote certificate
    let RemoteCertKey = "WinRMCertificateJsonBlob"

    /// Name of the vault key containing the VM remote SSH key
    let RemoteSshKey = "SshPublicKey"

    /// Maximum length of managed disk name
    let MaxManagedDiskNameLength = 80

    /// The managed disk ARM resource type ID
    let ManagedDiskResourceType = "Microsoft.Compute/disks"

    /// The managed image ARM resource type ID
    let ManagedImageResourceType = "Microsoft.Compute/images"

/// VM statuses
module Status =
    module PowerState =
        let Starting = "PowerState/starting"
        let Running = "PowerState/running"
        let Deallocating = "PowerState/deallocating"
        let Deallocated = "PowerState/deallocated"
        let Stopped = "PowerState/stopped"

    module Provision =
        let Succeeded = "ProvisioningState/succeeded"
        let Updating = "ProvisioningState/updating"
        let Failed = "ProvisioningState/failed"

        module Failure =
            let TargetDiskBlobAlreadyExists = "ProvisioningState/failed/TargetDiskBlobAlreadyExists"

/// Internal predicate expressions use to query states of virutal machines
module private InternalPredicates =
    let vmHasStatus (vm:VirtualMachine) status =
        vm.InstanceView.Statuses |> Seq.exists (fun s -> s.Code = status)

    let vmHasStatusWhere (vm:VirtualMachine) condition =
        vm.InstanceView.Statuses |> Seq.exists condition

    let vmIsReady (vm:VirtualMachine) =
        vmHasStatus vm Status.Provision.Succeeded
        && vmHasStatus vm Status.PowerState.Running

    let vmIsStopped (vm:VirtualMachine) =
        // VM is stopped either if
        // - it is marked as deallocated
        vmHasStatus vm Status.PowerState.Deallocated
        // - it has a stopped power state
        || vmHasStatus vm Status.PowerState.Stopped

module TraceTags =
    /// Track an exception in AppInsight and reraise it
    let inline failwithException e message tags =
        TraceTags.trackException e (("customMessage", message)::tags)
        Async.reraise e

type MachineNotFound(groupName: string, machineName:string, innerException:Exception) =
   inherit Exception(sprintf "Could not find VM instance %s under resource group %s" machineName groupName, innerException)
   new(groupName, machineName) = MachineNotFound(groupName, machineName, null)

let getAvailableOSImages (c:IComputeManagementClient) (location:string) =
    let location = location.Replace(" ", "") // strip spaces
    c.VirtualMachineImages.List(
                location=location,
                publisherName ="MicrosoftWindowsServer",
                offer="WindowsServer",
                skus ="2012-R2-Datacenter")

let getLatestAvailableOSImagesName (context:InfrastructureContext) location =
    getAvailableOSImages context.compute location |> Seq.last

/// Returns the URI to the most recent blob with the specified prefix and extesnion under the specified containter
let tryGetMostRecentBlobUnder (storage:BlobStorageContext) container prefix extension =
    async {
        let! blobs = Blob.listBlobsWithExtension (storage.client) container prefix extension
        return
            blobs
            |> Seq.cast<Microsoft.Azure.Storage.Blob.CloudBlob>
            // sort by decreasing modified date
            |> Seq.sortBy (fun b -> DateTimeOffset.MaxValue - b.Properties.LastModified.GetValueOrDefault(DateTimeOffset.MinValue))
            |> Seq.tryPick (fun s -> Some s.Uri) // Return the first element
    }

/// Get list of managed disks in the given resource group
let public getManagedDisksByResourceGroup (context:Context.InfrastructureContext) resourceGroupName =
    async {
        let! token = Async.CancellationToken
        try
            return!
                enumerateAllPagesAsync
                    context.tags
                    (sprintf "Managed disks under resource group %s" resourceGroupName)
                    {
                        getFirstPage =  fun () -> context.compute.Disks.ListByResourceGroupAsync(resourceGroupName, token)
                        getNextPage = fun link -> context.compute.Disks.ListByResourceGroupNextAsync(link, token)
                        pageAsList = fun page -> page |> Seq.toList
                        getLink = fun page -> page.NextPageLink
                    }

        with
        IsAggregateOf SomeResourceGroupNotFoundException e ->
            TraceTags.info "Resource group not found" (context.tags @ [ "resourceGroupName", resourceGroupName])
            return []
    }

/// Returns the most recent data disk with the specified prefix and under specified resource group and location
let getMostRecentDataDiskUnder (context: Context.InfrastructureContext) resourceGroupName prefix location =
    async {
        let! disks = getManagedDisksByResourceGroup context resourceGroupName
        let mostRecentDisk =
            disks
            |> Seq.filter  (fun d -> d.Name.StartsWith (sprintf "%s-%s" location prefix))
            |> Seq.sortBy  (fun d -> DateTime.MaxValue - d.TimeCreated.GetValueOrDefault(DateTime.MinValue))
            |> Seq.tryHead
        match mostRecentDisk with
        | Some disk ->
            return disk
        | None ->
            return TraceTags.failwith "Could not find a managed data disk under the specified resource group with specified prefix and region"
                            (context.tags @[ "prefix", prefix; "location", location; "resourceGroupName", resourceGroupName])
    }

/// Returns data disk with the specified name and version under specified resource group and location
let getDataDiskUnder (context:Context.InfrastructureContext) resourceGroupName name version (location:string) =
    async {
        // Managed disks max name length is 80 characters. The 80 characters contain several sub-sets of information, the region name being one of them.
        // These sub-sets of data are length restricted. This logic has to be the same as the one
        // naming managed disk. See function GenerateBlobVersion and function GenerateManagedDiskName in AzureHelpers.psm1 for the logic definition
        let LocationMaxLength = 10
        let location = Text.truncate LocationMaxLength location

        let diskName = location + version + name
        if diskName.Length > Constants.MaxManagedDiskNameLength  then
            TraceTags.failwith "Target managed disk name length exceeds maximum allowed length"
                               (context.tags @
                                [ "MaximumLength", Constants.MaxManagedDiskNameLength.ToString()
                                  "diskName" , diskName
                                  "diskNameLength", diskName.Length.ToString() ])


        let! disks = getManagedDisksByResourceGroup context resourceGroupName
        let disk = disks |> Seq.tryFind (fun d -> d.Name = diskName)

        match disk with
        | Some disk -> return disk
        | None -> return TraceTags.failwith "Could not find a managed data disk under specified resource group with given name and region"
                                            (context.tags @ [ "diskName", diskName; "location", location; "resourceGroupName", resourceGroupName ])
    }

/// Return true if an ARM resource is of the specified type
let isResourceOfType resourceType (genericResource:GenericResource) =
    String.Equals(genericResource.Type, resourceType, StringComparison.InvariantCultureIgnoreCase)

/// Get the disk details from a generic resource
/// If details cannot be fetched then log a warning and return None.
let tryGetDiskFromGenericResource (context:Context.InfrastructureContext) resourceGroupName (genericResource:GenericResource) =
    let logTags = context.tags @ [ "id", genericResource.Id;  "name", genericResource.Name;  "type", genericResource.Type ]
    if isResourceOfType Constants.ManagedDiskResourceType genericResource then
        try
            Some (context.compute.Disks.Get(resourceGroupName, genericResource.Name))
        with
        | :? Microsoft.Rest.Azure.CloudException ->
            TraceTags.warning "Skipping disk resource: could not get details of managed disk" logTags
            None
    else
        TraceTags.failwith "Resource is not of managed-disk type" logTags

/// Get the managed image details from a generic resource
/// If details cannot be fetched then log a warning and return None.
let tryGetImageFromGenericResource (context:Context.InfrastructureContext) resourceGroupName (genericResource:GenericResource) =
    let logTags = context.tags @ [ "id", genericResource.Id;  "name", genericResource.Name;  "type", genericResource.Type ]
    if isResourceOfType Constants.ManagedImageResourceType genericResource then
        try
            Some (context.compute.Images.Get(resourceGroupName, genericResource.Name))
        with
        | :? Microsoft.Rest.Azure.CloudException ->
            TraceTags.warning "Skipping image resource: could not get details of managed image" logTags
            None
    else
        TraceTags.failwith "Resource is not of managed-image type" logTags

/// A filter expression on tags supporting
/// wildcard expression '*' matchin any string,
/// and `~` matchin any expression contain the specified substring.
/// The first element is the tag key, the second is the search expression.
type TagFilterExpression = string * string

/// Returns true if the set of tags matches the specified tag  expression filter
let matchTagExpression
        (tags:System.Collections.Generic.IDictionary<string,string>)
        ((filterTagName,filterTagValue):TagFilterExpression) =
    match tags.TryGetValue filterTagName with
    | true, _ when filterTagValue = "*" ->
        // The tag exists for the disk resource e.g. ("version","*")
        true
    | true, diskTagValue when filterTagValue.Chars(0) = '~' ->
        // Test if the filter tag value is contained in the disk tag value e.g. ("release","~develop")
        // If the disk's tag value is "develop,dogfood", this should return true
        diskTagValue.Contains(filterTagValue.Substring(1))
    | true, diskTagValue ->
        //Test if the filter tag value is equal to the disk tag value
        diskTagValue = filterTagValue
    | false, _ ->
        //The tag does not exists for the disk resource
        false

/// Returns true if the set of tags matches the specified tag filter
let matchTagValue (tags:System.Collections.Generic.IDictionary<string,string>) ((tagName, tagValue):string*string) =
    match tags.TryGetValue tagName with
    | true, diskTagValue -> diskTagValue = tagValue
    | false, _ -> false

/// Find disks in a resource group filtered by the specified set of filter tags, which are name-value pairs
/// - The (tagName, tagValue) pair is used to query resource group. It should not contain any wildcard
/// expression (* or ~). (The characters themselves are allowed but won't be interpreted as wildcards)
/// - Additional tag filter expressions consisting of pairs (tagName, tagExpression) are used to further
///   filter the returned list of disks. The tagExpression can contain wildcards.
let listDisksInResourceGroupByTagExpressions
    (context:Context.InfrastructureContext)
    resourceGroupName
    (tagPair:string*string)
    (tagFilterExpressions:TagFilterExpression list) =
    async {
        let! disks = listDisksByResourceGroupAndTag context resourceGroupName tagPair
        TraceTags.info "Retrieved list of Azure disks." (context.tags @ ["tagPair", sprintf "%A" tagPair
                                                                         "tagFilterExpressions", sprintf "%A" tagFilterExpressions
                                                                         "resourceGroupName", resourceGroupName
                                                                         "disks.Length", disks.Length.ToString()])
        return
            disks
            |> List.filter (fun disk -> List.forall (matchTagExpression disk.Tags) tagFilterExpressions)
    }

/// List managed images in a resource group filtered by the specified set of filter name-value tag pairs
let listImagesInResourceGroupWithTags
    (context:Context.InfrastructureContext)
    resourceGroupName
    (tagPair:string*string)
    (additionalTagPairs:(string*string) list) =
    async {
        let! images = listImagesByResourceGroupAndTag context resourceGroupName tagPair
        TraceTags.info "Retrieved list of Azure images." (context.tags @ ["tagPair", sprintf "%A" tagPair
                                                                          "additionalTagPairs", sprintf "%A" additionalTagPairs
                                                                          "resourceGroupName", resourceGroupName
                                                                          "images.Length", images.Length.ToString()])
        return
            images
            |> List.filter (fun image -> List.forall (matchTagValue image.Tags) additionalTagPairs)
    }

/// Convert an absolute blob URI to a relative path under a given storage account and parent directory
let public makeRelativeUri parent (absoluteUri:System.Uri) =
    absoluteUri.LocalPath |> Text.skipPrefixCaseInsensitive ("/" + parent)

#if TEST
let x storage =
    tryGetMostRecentVhdUnder storage "vhds" "SFV-"

let y storage =
    getVhds storage "vhds" "S"
    |> Seq.map (fun x-> x.Uri.AbsolutePath, x.Properties.LastModified.ToString())
    |> Seq.sortBy snd
#endif

/// Azure VM sizes
type AzureVmSize =
    | ExtraSmall
    | Small
    | Medium
    | Large
    | ExtraLarge

/// User credentials on a VM
type VMUserCredentials =
    {
        Username : string
        Password : string
    }

/// Definition of a VM that can be used to provision a VM image from Azure marketplace.
type GalleryImageSource =
    {
        publisher : string
        offer : string
        sku : string
        version: string
    }

/// Various way of definining the disk layout when provisioning a new VM
type VMDiskSource =

    /// Provision OS disk by specializing a managed image specified by Resource Id
    | OSManagedImage of string

    /// Provision OS disk from gallery image.
    | OSGalleryImage of GalleryImageSource

exception AzureComputeException of System.Exception

/// Stop a VM in Azure
let stopVM (c:Context.InfrastructureContext) vmName =
    async {
        let tags = c.tags @ [ "vmName", vmName ]
        try
            do! c.compute.VirtualMachines.PowerOffAsync(c.groupName, vmName).AsAsync
            TraceTags.info "VM stopped." tags
        with e ->
            TraceTags.failwithException e "Failed to stop VM" tags
    }

let private getAllNetworkInterfaces (vm:VirtualMachine) =
    vm.NetworkProfile

/// Return the list of URI to all disk attached to the VM
let private getVMDisksUri context (vm:VirtualMachine) =
    if isNull vm.StorageProfile.OsDisk.ManagedDisk then
        vm.StorageProfile.OsDisk.Vhd.Uri
        ::(vm.StorageProfile.DataDisks |> Seq.map (fun d -> d.Vhd.Uri) |> Seq.toList)
    else
        TraceTags.info "This is a VM with managed disks, therefore we don't manage deletion of VHDs." context.tags
        []

/// Return URI to VHD blob of OS and data disks attached to the specified virtual machine
let getVMVhds (context:InfrastructureContext) vmName =
    async {
        let! vm = context.compute.VirtualMachines.GetAsync(context.groupName, vmName).AsAsync
        return getVMDisksUri context vm
    }

/// Returns the full URI to a given VHD blob file
/// (accepts both base name and filename with .vhd extension)
let private vhdUri s (file:string) =
    let filename =
        if file.EndsWith(".vhd", System.StringComparison.InvariantCultureIgnoreCase) then
            file
        else
            file + ".vhd"

    (blobUri s (sprintf "%s/%s" Constants.VhdDisksContainer filename)).AbsoluteUri

/// Copy a VHD blob on Azure.
/// Target blob name should not include the .vhd extension
let copyVhd (s:BlobStorageContext) (sourceUri:Uri) targetContainer targetBlobName =
    async {
        let container = s.client.GetContainerReference(targetContainer)
        let target = container.GetPageBlobReference(targetBlobName)
        let! r = target.StartCopyAsync(sourceUri).AsAsync
        return target.Uri
    }


/// Copy a VHD blob on Azure with resilience to "Server Busy" errors
let rec resilientcopyVhd tags (s:BlobStorageContext) (sourceUri:Uri) targetContainer targetBlobName =
    let rec aux retryCount =
        async {
            if retryCount > Constants.BlobCopyMaxiumAttempts then
                return TraceTags.failwith "Maximum attempt reached at copying blob." tags
            else
                try
                    return! copyVhd s sourceUri targetContainer targetBlobName
                with
                /// Catch System.AggregateException
                //    of Microsoft.Azure.Storage.StorageException
                //    of "The remote server returned an error: (503) Server Unavailable"
                | IsAggregateOf (SomeStorageException System.Net.HttpStatusCode.ServiceUnavailable) e ->
                    TraceTags.warning "WARNING: Failed to copy blob from specified URI. Retrying in %ds... "
                        (tags @ [ "sourceUri", sourceUri.AbsoluteUri
                                  "targetBlobName", targetBlobName
                                  "exception", e.ToString()
                                  "retryIn", (int Constants.BlobCopyRetryPeriod.TotalSeconds).ToString()
                               ])

                    do! Async.Sleep (int Constants.BlobCopyRetryPeriod.TotalMilliseconds)
                    return! aux (retryCount+1)

                | :? System.IO.IOException as e ->
                    return TraceTags.failwith "Error while copying blob from specified URI"
                        (tags @ [ "sourceUri", sourceUri.AbsoluteUri
                                  "targetBlobName", targetBlobName
                                  "retryIn", (int Constants.BlobCopyRetryPeriod.TotalSeconds).ToString()
                               ])
        }
    aux 0

/// Desired failure behviour when deleting a blob
type DeleteBlobFailureOptions =
    | Throw
    | TraceError
    | TraceWarning

/// Match web or Azure storage exceptions
let SomeWebOrStorageException : System.Exception -> System.Exception option = function
    | :? Microsoft.Azure.Storage.StorageException
    | :? System.Net.WebException as e -> Some e
    | _ -> None

/// Try delete a blob specified by its absolute URI
let tryDeleteBlob (storage:BlobStorageContext) (blobUri:Uri) =
  async {
        try
            let! blob = storage.client.GetBlobReferenceFromServerAsync(blobUri).AsAsync
            let! existed =  blob.DeleteIfExistsAsync().AsAsync
            return Choice1Of2 existed
        with
        IsAggregateOf SomeWebOrStorageException e ->
            return  Choice2Of2 e
    }

/// Delete a blob specified by its absolute URI
let deleteBlob tags (failureBehaviour:DeleteBlobFailureOptions) (storage:BlobStorageContext) (blobUri:Uri) =
  async {
        let fail message =
            let tags = tags @
                        [ "blobUri", blobUri.AbsoluteUri
                          "errorMessage", message
                          "severity", sprintf "%A" failureBehaviour ]
            match failureBehaviour with
            | Throw ->
                TraceTags.failwith "Could not delete blob (exception)" tags
            | TraceError ->
                TraceTags.error "Could not delete blob (error)" tags
            | TraceWarning ->
                TraceTags.warning "Could not delete blob (warning)" tags

        let! result = tryDeleteBlob storage blobUri
        match result with
        | Choice1Of2 true ->
            return ()
        | Choice1Of2 false ->
            return fail "The blob did not exist so it could not be deleted"
        | Choice2Of2 e ->
            return fail e.Message
    }

/// Extract the parent container and relative path from a blob URI
let getBlobContainerAndRelativePathFromUri (blobUri:Uri) =
    let container = blobUri.Segments.[1].Replace("/","")
    let relativePathUnderContainer = blobUri.Segments |> Seq.skip 2 |> Text.join ""
    container, relativePathUnderContainer

/// Delete a VHD captured from a VM that was deployed on the specified storage account.
/// The blob is specified by the path *relative to the parent container ("system") used by Azure to store captured images*.
let deleteCapturedVhdRelative tags (storage:BlobStorageContext) blobRelativePathInContainer =
    async {
        let container = storage.client.GetContainerReference(Constants.CapturedImagesContainer)
        let blob = container.GetBlobReference(blobRelativePathInContainer)
        let! r = blob.DeleteIfExistsAsync().AsAsync
        if not r then
            TraceTags.warning "Captured VHD blob does not exist or has already been deleted" (tags @ [ "BlobUri", blob.Uri.AbsoluteUri ])
    }

/// Delete blobs specified by Uri
let deleteBlobs context storage (blobUris:Uri list) disksExpectedToExist =
    async {
        let! results =
            blobUris
            |> Seq.map (tryDeleteBlob storage)
            |> Async.Parallel

        let blobAndResults =
            Seq.zip blobUris results

        let blobMissing (blobUri, deleteResult) =
            match deleteResult with
            | Choice1Of2 true -> None
            | Choice1Of2 false -> Some (sprintf "Blob did not exists: %O" blobUri)
            | Choice2Of2 _ -> None

        let hasError (blobUri, deleteResult) =
            match deleteResult with
            | Choice1Of2 _ -> None
            | Choice2Of2 e -> Some <| sprintf "Exception occurred when deleting blob %O: %O" blobUri e

        let errorBlobs =
            Seq.choose hasError blobAndResults |> Seq.toList

        let missingBlobs =
            Seq.choose blobMissing blobAndResults |> Seq.toList

        let tags =
            context.tags @
                ([
                  "blobsToDelete", sprintf "%A" blobUris
                  "missingBlobs", sprintf "%A" missingBlobs])

        if not <| List.isEmpty errorBlobs then
            TraceTags.error "Errors occured when trying to delete blobs" (tags @ [ "errors", sprintf "%A" errorBlobs ])
        else if disksExpectedToExist && not <| List.isEmpty missingBlobs then
            TraceTags.error "Some blobs were not found and could not be deleted" tags
    }

/// Get the list of URIs to all disk VHDs attached to a virtual machine
let private getAllVmDisks context (vm:VirtualMachine) =
    let allVhds = getVMDisksUri context vm
    let allDiskVhdsUrl =
        allVhds
        |> Seq.map Uri
        |> Seq.toList

    TraceTags.info "VHDs disks associated with a virtual machine"
        (context.tags @
            [  "diskCount", allDiskVhdsUrl.Length.ToString()
               "machineName", vm.Name
               "disks", (allDiskVhdsUrl |> Seq.map (fun v -> v.AbsoluteUri) |> Text.join "  \n")
            ])

    allDiskVhdsUrl

/// Determine if the status of the VM indicates that the attached disks are expected to exist
///  - If the machine has already started then we expect the disk vhd to necessarily exist
///  - If the machine has not started yet then the disk VHDs may or may not exist
/// This is used to determine whether errors need to be logged when cleaning up the VM and disks blobs are missing.
let areVmDisksExpectedToExist vm =
    let diskExpectedToExist = InternalPredicates.vmHasStatus vm Status.PowerState.Running
                            || InternalPredicates.vmHasStatus vm Status.PowerState.Starting
    diskExpectedToExist

/// Get the list with names of all managed data disks attached to a virtual machine
let private getNamesOfAllManagedDataDisks (vm:VirtualMachine) =
    if isNull vm.StorageProfile.OsDisk.ManagedDisk then
        Seq.empty
    else
        vm.StorageProfile.DataDisks |> Seq.map (fun d -> d.Name)

/// Get the name of OsDisk associated with the specified virtual machine
let private tryGetManagedOsDiskName (vm:VirtualMachine) =
    if isNull vm.StorageProfile.OsDisk.ManagedDisk then
        None
    else
        Some vm.StorageProfile.OsDisk.Name

/// Returns a managed disk by name and resource group, or None in case it wasn't possible to get the disk
let tryGetManagedDisk (context:Context.InfrastructureContext) resourceGroupName diskName =
    async {
        let tags = context.tags @
                        [ "diskName", diskName
                          "resourceGroupName", resourceGroupName ]
        try
            let! disk = context.compute.Disks.GetAsync(resourceGroupName, diskName).AsAsync
            if isNull disk then
                return None
            else
                TraceTags.info "Found a disk with matching name in the resource group." tags
                return Some disk
        with
        | e ->
            TraceTags.warning "Failed trying to get managed disk under specified resource group" (("exception", e.ToString())::tags)
            return None
    }

/// Get a managed disk by name and resource group
let getManagedDisk (context:Context.InfrastructureContext) resourceGroupName diskName =
    async {
        let tags = context.tags @
                        [ "diskName", diskName
                          "resourceGroupName", resourceGroupName ]
        try
            let! disk = context.compute.Disks.GetAsync(resourceGroupName, diskName).AsAsync
            if isNull disk then
                return TraceTags.failwith "Managed disk not found in resource group." tags
            else
                TraceTags.info "Found a disk with matching name in the resource group." tags
                return disk
        with
        | :? System.Runtime.Serialization.SerializationException as ex ->
            return TraceTags.failwithException ex "Unable to deserialize the response when trying to get a managed disk." tags
        | :? Microsoft.Rest.ValidationException as ex ->
            return TraceTags.failwithException ex "Validation failed when trying to get a managed disk." tags
        | :? System.ArgumentNullException as ex ->
            return TraceTags.failwithException ex "Failed trying to get a managed disk. Some of the passed parameters are null." tags
        | ex ->
            return TraceTags.failwithException ex "Unable to deserialize the response when trying to get a managed disk." tags
    }

/// Delete a manage disk by name and resource group
let deleteManagedDisk (context:Context.InfrastructureContext) resourceGroupName diskName =
    async {
        let tags = context.tags @
                     ([ "diskName", diskName
                        "resourceGroupName", resourceGroupName ])
        try
            let! disk = getManagedDisk context resourceGroupName diskName
            do! context.compute.Disks.DeleteAsync(resourceGroupName, diskName).AsAsync
            TraceTags.info "Deletion of managed disk completed." tags
        with
        | :? System.Runtime.Serialization.SerializationException as e ->
            return TraceTags.failwithException e "Unable to deserialize the response when trying to delete a managed image." tags
        | :? Microsoft.Rest.ValidationException as e ->
            return TraceTags.failwithException e "Validation failed when trying to delete a managed image." tags
        | :? System.ArgumentNullException as e ->
            return TraceTags.failwithException e "Failed trying to delete a managed image. Some of the passed parameters are null." tags
        | e ->
            return TraceTags.failwithException e "Failed deleting a managed disk from resource group." tags
    }

/// Create Azure portal url from a resource ID
let azurePortalUrlFromResourceId (resourceId: string) =
    sprintf "%s%s" Constants.portalResource resourceId

/// Retrieve VM Azure portal URL and VM IP configurations
let getVmIpConfigurations (context:Context.InfrastructureContext) vmName =
    async {
        let! vmRequest = context.compute.VirtualMachines.GetWithHttpMessagesAsync(context.groupName, vmName).AsAsync
        let vm = vmRequest.Body
        assertStatusCodeIs context.tags System.Net.HttpStatusCode.OK vmRequest.Response.StatusCode "Could not retrieve VM info."

        let networkInterfaceGroup, networkInterfaceName =
            match vm.NetworkProfile.NetworkInterfaces.[0].Id.Split([|'/'|]) |> Seq.toList |> List.rev with
            | n::"networkInterfaces"::"Microsoft.Network"::"providers"::g::"resourceGroups"::_ -> g, n
            | _ -> TraceTags.failwith "Incorrect network interface URI format" context.tags

        let token = Threading.CancellationToken()

        let! net = context.network.NetworkInterfaces.GetWithHttpMessagesAsync(networkInterfaceGroup, networkInterfaceName, cancellationToken = token).AsAsync
        assertStatusCodeIs context.tags  System.Net.HttpStatusCode.OK net.Response.StatusCode "Could not get network interface."

        // Get public IP address assigned to the VM
        let! publicIPs =
            net.Body.IpConfigurations
            |> Seq.map (fun ipConfig ->
                async {
                    if isNull ipConfig.PublicIPAddress then
                        return None
                    else
                        let publicIpResourceGroup, publicIpName =
                            match ipConfig.PublicIPAddress.Id.Split([|'/'|]) |> Seq.toList |> List.rev with
                            | n::"publicIPAddresses"::"Microsoft.Network"::"providers"::g::"resourceGroups"::_ -> g,n
                            | _ -> TraceTags.failwith "Incorrect public IP URI format" context.tags

                        let! publicIp = context.network.PublicIPAddresses.GetWithHttpMessagesAsync(publicIpResourceGroup, publicIpName, cancellationToken = token).AsAsync
                        assertStatusCodeIs context.tags System.Net.HttpStatusCode.OK publicIp.Response.StatusCode "Cannot find public IP for load balancer"
                        return Some publicIp.Body
                }
            ) |> Async.sequentialCombine
        return (azurePortalUrlFromResourceId vm.Id), net.Body.IpConfigurations, publicIPs
    }

/// Options accepted by deleteVM
[<Flags>]
type DeleteVmOptions =
/// Delete VHDs mounted as VM disks
| DeleteAttachedDisks = 0x01
/// Delete NICs attached to the VM
| DeleteAttachedNic = 0x02
/// Do nothing
| None = 0x04

/// Delete a VM and its associated VHDs. Returns once the deletion has completed.
let deleteVM (c:Context.InfrastructureContext) (storage:BlobStorageContext) (options:DeleteVmOptions) vmName =
    async {
        let tags = c.tags @ [ "name", vmName]
        TraceTags.info "Deleting VM synchronoulsy in ARM" tags

        let! vm = c.compute.VirtualMachines.GetAsync(c.groupName, vmName).AsAsync

        // Get the list of underlying VHDs disks: They are not automatically
        // deleted by ARM upon group deletion
        let allDiskVhdsUrl = getAllVmDisks c vm
        let allNics = getAllNetworkInterfaces vm
        let allManagedDataDisks = getNamesOfAllManagedDataDisks vm
        let managedOsDisk = tryGetManagedOsDiskName vm

        // Delete the VM synchronoulsy in ARM
        try
            do! c.compute.VirtualMachines.DeleteAsync(c.groupName, vmName).AsAsync
            TraceTags.info "VM successfully deleted." tags
        with e ->
            TraceTags.failwithException e "Failed to delete VM." tags

        if options.HasFlag DeleteVmOptions.DeleteAttachedDisks then
            // Azure does not properly cleanup the disks so we need to delete them manually
            match managedOsDisk with
            | None ->
                TraceTags.info "Delete VHDs that were attached to the deleted machine" tags
                do! deleteBlobs c storage allDiskVhdsUrl (areVmDisksExpectedToExist vm)
            | Some vmOsDisk ->
                TraceTags.info "Deleting managed data disks attached to the deleted machine, and its OS disk." tags
                allManagedDataDisks
                |> Seq.append [vmOsDisk]
                |> Seq.map (deleteManagedDisk c c.groupName)
                |> Async.Parallel |> ignore

        if options.HasFlag DeleteVmOptions.DeleteAttachedNic then
            TraceTags.info "Deleting NICs that were attached to deleted machine" tags
            for nic in allNics.NetworkInterfaces do
                let nicNameComponents = nic.Id.Split('/')
                if nicNameComponents.Length > 0 then
                    let nicName = Array.last nicNameComponents
                    let! r = Network.deleteNetworkInterface c c.groupName nicName
                    if not r.Response.IsSuccessStatusCode then
                        TraceTags.error "Could not delete network interface" [ "networkInterace", nic.Id; "statusCode", r.Response.StatusCode.ToString() ]
                else
                    TraceTags.error "Could not delete network interface: Invalid URI" [ "networkInterace", nic.Id ]
    }

/// Asynchronously delete a VM while keeping its underlying disk VHDs.
/// Returns the Azure request Id and the list of VHDs that were attached to the VM.
/// There is two level of async:
/// - asynchrony of the workflow using F# async {} => the function will not block thread when waiting for intermediate operations to terminate
/// - asynchrony of the deletion: the function will return to the caller before the VM is actually deleted. A `RequestId` is returned which can be used by the caller to check the deletion status.
/// Almost all our code is aync in the first sense, the suffic `Async` in the function name below indicates asynchrony in the second sense.
let deleteVMAsync (c:Context.InfrastructureContext) vmName =
    async {
        let tags = c.tags @ [ "name", vmName]
        TraceTags.info "Deleting VM asynchronoulsy in ARM" tags

        let! vm = c.compute.VirtualMachines.GetAsync(c.groupName, vmName).AsAsync

        let disksToDelete =
            if isNull vm.StorageProfile.OsDisk.ManagedDisk then
                // For a VM created from VHDs, save the list of VHDs disks for later cleanup
                // (Azure does not delete VHD files after deleting a VM)
                Choice1Of2(getAllVmDisks c vm)
            else
                // For a VM created form managed images, save the managed osdisk for later cleanup
                Choice2Of2(c.groupName, vm.StorageProfile.OsDisk.Name)

        // Request asynchronous deletion of the VM to ARM
        try
            let! azureAsyncOperation = c.compute.VirtualMachines.BeginDeleteWithHttpMessagesAsync(c.groupName, vmName).AsAsync
            TraceTags.info "Deletion of VM successfully requested."
                    (tags@["RequestId", azureAsyncOperation.RequestId])
            return azureAsyncOperation.GetAsyncOperationUrl(tags), disksToDelete
        with e ->
            return TraceTags.failwithException e "Failed to request deletion of VM." tags
    }

/// Status response of an asynchronous Azure VM operation
type AsyncResponse =
    /// Operation still in progress, need to check again in the specified amount of time
    | InProgressRetryIn of System.TimeSpan
    /// Operation completed with the specified status string
    | CompletedResult of string

/// Get result of an asynchronous request
/// Return `AsyncResponse.InProgressRetryIn` if operation is still in progress,
/// `AsyncResponse.CompletedResult` if it has completed and and throw if an error occurs.
let inline private getAsyncRequestResult< ^T when ^T : null>
    (c:Context.InfrastructureContext)
    (requestName:string)
    (asyncRequestUrl:AsyncOperationUrl) =
    async {
        let! completionResponse = getAsyncOperationStatusWithOutput< ^T> c.tags c.compute asyncRequestUrl
        return
            match completionResponse.Status.Status with
            | Microsoft.Rest.Azure.AzureAsyncOperation.FailedStatus->
                TraceTags.failwith
                    (sprintf "Async operation `%s` failed" requestName)
                    (c.tags @
                     [ "rawResponseContent", completionResponse.RawResponseContent
                       "error", sprintf "%A" completionResponse.Status.Error
                       "status", sprintf "%A" completionResponse.Status ])
            | Microsoft.Rest.Azure.AzureAsyncOperation.CanceledStatus ->
                TraceTags.failwith
                    (sprintf "Async operation `%s` was cancelled" requestName)
                    (c.tags @
                     [ "rawResponseContent", completionResponse.RawResponseContent
                       "error", sprintf "%A" completionResponse.Status.Error
                       "status", sprintf "%A" completionResponse.Status ])
            | Microsoft.Rest.Azure.AzureAsyncOperation.InProgressStatus ->
                AsyncResponse.InProgressRetryIn (System.TimeSpan.FromSeconds (float completionResponse.Status.RetryAfter))
            | Microsoft.Rest.Azure.AzureAsyncOperation.SuccessStatus ->
                TraceTags.info (sprintf "Async operation `%s` succeeded" requestName)
                     (c.tags @
                      [ "rawResponseContent", completionResponse.RawResponseContent
                        "response", (Microsoft.FSharpLu.Json.Compact.serialize completionResponse.Output)
                        "status", (Microsoft.FSharpLu.Json.Compact.serialize completionResponse.Status)
                      ])
                AsyncResponse.CompletedResult completionResponse.Status.Status
            | status ->
                TraceTags.failwith "Unknown operation status" (c.tags@ ["status", sprintf "%A" status])
    }

/// Get result of a VM deletion asynchronous request
/// Return `AsyncResponse.InProgressRetryIn` if the deletion operation is still in progress,
/// `AsyncResponse.CompletedResult` if it has completed and and throw if an error occurs.
let getDeleteVmAsyncResult (c:Context.InfrastructureContext) (deleteAsyncRequestUrl:AsyncOperationUrl) =
    getAsyncRequestResult<AzureAsyncOperation> c "VM deletion" deleteAsyncRequestUrl

/// Get result of a VM image capture asynchronous request
/// Return `AsyncResponse.InProgressRetryIn` if the capture operation is still in progress,
/// `AsyncResponse.CompletedResult` if it has completed and and throw if an error occurs.
let getCaptureAsyncResult (c:Context.InfrastructureContext) (captureAsyncRequestUrl:AsyncOperationUrl) =
    getAsyncRequestResult<AzureOperationResponse<Image>> c "Image capture" captureAsyncRequestUrl

/// Private section of the Azure diagnostic specification
/// See WAD files tutorial at http://blogs.msdn.com/b/davidhardin/archive/2011/03/29/configuring-wad-via-the-diagnostics-wadcfg-config-file.aspx
type AzureDiagnosticConfigPrivate =
    {
        storageAccountName : string
        [<Newtonsoft.Json.JsonIgnore>]
        getStorageAccountKey : unit -> string
        storageAccountEndPoint : string
    }
with
     member x.storageAccountKey with get () = x.getStorageAccountKey()

/// Public section of the Azure diagnostic specification
type AzureDiagnosticConfigPublic =
    {
        /// WadCfg XML config encoded in base 64
        xmlCfg : string
        /// Storage account name where to store the diagnostics
        StorageAccount : string
    }

/// Azure diagnostic specification
type AzureDiagnosticConfig = AzureDiagnosticConfigPublic * AzureDiagnosticConfigPrivate

/// Get list of virtual machines deployed in the given resource group
let private getVirtualMachine (context:Context.InfrastructureContext) machineName =
    async {
        try
            return! context.compute.VirtualMachines.GetAsync(context.groupName, machineName, expand = Nullable InstanceViewTypes.InstanceView).AsAsync
        with
        | IsAggregateOf SomeResourceGroupNotFoundException e ->
            return raise <| ResourceGroupNotFound(context.groupName, e.Message)
        | IsAggregateOf SomeResourceNotFoundException e ->
            return raise <| MachineNotFound(context.groupName, machineName)
    }

// Sets the state of the specified virtual machine to generalized.
let generalizeVm (c:Context.InfrastructureContext) vmName =
    async {
        let tags = c.tags @ [ "machineName", vmName ]
        TraceTags.info "Marking VM as generalized" tags
        try
            do! c.compute.VirtualMachines.GeneralizeAsync(c.groupName, vmName).AsAsync
            TraceTags.info "Generalization of VM completed." tags
        with
        | e ->
            return TraceTags.failwithException e "Failed to generalize VM." tags
    }

/// Capture a managed image from the OS disk of a virtual machine.
/// Return the capture request Id returned by Azure.
let captureOSImageAsync (c:Context.InfrastructureContext) vmName targetResourceGroup capturedImageName =
    async {
        let tags = c.tags @ [ "machineName", vmName ]
        do! generalizeVm c vmName
        try
            TraceTags.info "Capturing managed image from VM" tags
            let! vm = getVirtualMachine c vmName
            let image =
                Image (
                    SourceVirtualMachine = ComputeSubResource(vm.Id),
                    Location = vm.Location,
                    Tags = dict (tags @ ["creationTime", DateTime.UtcNow.ToString("u")])
                )
            let! captureRequestResponse = c.compute.Images.BeginCreateOrUpdateWithHttpMessagesAsync(targetResourceGroup, capturedImageName, image).AsAsync
            TraceTags.info "Request submitted to Managed Image API to capture virtual machine."
                (tags @
                    [
                      "machineId", vm.Id
                      "location", vm.Location
                      "targetResourceGroup", targetResourceGroup
                      "capturedImageName", capturedImageName
                      "requestId", captureRequestResponse.RequestId
                      "imageId", captureRequestResponse.Body.Id
                      "response", (Json.Compact.serialize captureRequestResponse.Body)
                      "responseInfo", (sprintf "%A" captureRequestResponse.Response)
                    ])

            return captureRequestResponse.Body.Id, captureRequestResponse.GetAsyncOperationUrl(tags)
        with
        | e ->
            return TraceTags.failwithException e "Failed to request image capture of VM." tags
    }

/// Wait for a VHD image capture to complete
let waitForCaptureToComplete (c:Context.InfrastructureContext) vmName (captureAsyncRequest:AsyncOperationRequest) (timeout:System.TimeSpan) =
    async {
        let! captureCompletion =
            waitForRequestToCompleteAsync
                captureAsyncRequest
                timeout
                (getAsyncOperationStatusWithOutput<Image> c.tags c.compute)

        let capturedImage = captureCompletion.Output.Id
        TraceTags.info "Successfully captured image from VM"
                (c.tags @
                 [ "vmName", vmName
                   "capturedImage", capturedImage])
        return capturedImage
    }

/// Capture an OS image from a VM, waits for capture to complete and then delete the VM.
let captureOSImageAndDeleteVM (c:Context.InfrastructureContext) storage vmName captureTimeout targetResourceGroup capturedImageName deleteVmOptions =
    async {
        let! capturedImageId, captureAsyncRequest = captureOSImageAsync c vmName targetResourceGroup capturedImageName
        let! operationResult = waitForCaptureToComplete c vmName captureAsyncRequest captureTimeout
        TraceTags.info "Deleting captured VM"
            (c.tags @ ["machineName", vmName; "asyncResult", operationResult])
        do! deleteVM c storage deleteVmOptions vmName
        return capturedImageId
    }

/// Get list of virtual machines deployed in the given resource group
let public getGroupVirtualMachine (context:Context.InfrastructureContext) =
    async {
        let! token = Async.CancellationToken
        try
            return!
                enumerateAllPagesAsync
                    context.tags
                    (sprintf "virtual machines under resource group %s" context.groupName)
                    {
                        getFirstPage =  fun () -> context.compute.VirtualMachines.ListAsync(context.groupName, token)
                        getNextPage = fun link -> context.compute.VirtualMachines.ListNextAsync(link, token)
                        pageAsList = fun page -> page |> Seq.map (fun vm -> vm.Name) |> Seq.toList
                        getLink = fun page -> page.NextPageLink
                    }

        with
        IsAggregateOf SomeResourceGroupNotFoundException e ->
            TraceTags.info "Resource group not found" (context.tags @ ["resourceGroup", context.groupName])
            return []
    }

/// Get list of managed images in the given resource group
let public getManagedImagesByResourceGroup (context:Context.InfrastructureContext) resourceGroupName =
    async {
        let! token = Async.CancellationToken
        try
            return!
                enumerateAllPagesAsync
                    context.tags
                    (sprintf "Managed images under resource group %s" resourceGroupName)
                    {
                        getFirstPage =  fun () -> context.compute.Images.ListByResourceGroupAsync(resourceGroupName, token)
                        getNextPage = fun link -> context.compute.Images.ListByResourceGroupNextAsync(link, token)
                        pageAsList = fun page -> page |> Seq.toList
                        getLink = fun page -> page.NextPageLink
                    }

        with
        IsAggregateOf SomeResourceGroupNotFoundException e ->
            TraceTags.info "Resource group not found" (context.tags @ ["resourceGroup", context.groupName])
            return []
    }

/// Get a managed image created from a specified VHD in given location and resource group
let getManagedImageByOsImage (context: Context.InfrastructureContext) resourceGroupName osImageVhdPath location =
    async {
        let tags = context.tags @
                     [ "osImageVhdPath", osImageVhdPath
                       "resourceGroupName", resourceGroupName
                       "location", location ]
        try
            let! images = getManagedImagesByResourceGroup context resourceGroupName
            let imagesFromOsImage = images |> Seq.tryFind (fun i -> i.StorageProfile.OsDisk.BlobUri.Contains(osImageVhdPath) && i.Location.ToLower() = location)
            match imagesFromOsImage with
            | Some targetImage ->
                TraceTags.info "Found image with matching VHD path, platform and location in the resource group."
                    (tags@[ "targetImage", targetImage.Name])
                return targetImage
            | None ->
                return TraceTags.failwith "Managed image could not be found for specified VHD path, platform and location under the specified resource group." tags
        with
        | :? System.Runtime.Serialization.SerializationException as e ->
            return TraceTags.failwithException e  "Unable to deserialize the response when trying to get a managed image." tags
        | :? Microsoft.Rest.ValidationException as e ->
            return TraceTags.failwithException e "Validation failed when trying to get a managed image." tags
        | :? System.ArgumentNullException as e ->
            return TraceTags.failwithException e "Failed trying to get a managed image. Some of the passed parameters are null." tags
        | e ->
            return TraceTags.failwithException e "Failed to get a managed image with from resource group for the specified OS image, VHD path and location" tags
    }

/// Get a managed image Resource Id by image name
let getManagedImageResourceIdByName subscriptionId resourceGroup imageName =
    sprintf "/subscriptions/%s/resourceGroups/%s/providers/Microsoft.Compute/images/%s" subscriptionId resourceGroup imageName

/// Get the name and resource group of a managed image by its Resource Id
let getManagedImageNameAndResourceGroupById tags (imageResourceId: string) =
    let imageUri = Uri (Uri Microsoft.FSharpLu.Azure.Constants.management, imageResourceId)
    match imageUri.Segments |> Seq.toList with
    | [_; _; _; "resourceGroups/"; resourceGroupName; _; _;"images/"; imageName] -> imageName, resourceGroupName.Replace("/","")
    | _ -> TraceTags.failwith "Format of managed image resource Id is incorrect"
                    (tags @ [ "imageResourceId", imageResourceId ])

/// Get a managed image with specified Resource Id
let tryGetManagedImageByResourceId (context: Context.InfrastructureContext) (imageResourceId:string) =
    async {
        let tags = context.tags @ [ "imageResourceId", imageResourceId ]
        try
            let imageName, resourceGroupName = getManagedImageNameAndResourceGroupById tags imageResourceId
            let! image = context.compute.Images.GetAsync(resourceGroupName, imageName).AsAsync
            if isNull image then
                return None
            else
                TraceTags.info "Found an image with matching name in the resource group." tags
                return Some image
        with
        | e ->
            TraceTags.warning "Failed to get a managed image" (["exception", e.ToString(); "imageResourceId", imageResourceId]@tags)
            return None
    }

/// Delete a manage image with a specified Resource Id
let tryDeleteManagedImage (context:Context.InfrastructureContext) (imageResourceId:string) =
    async {
        let tags = context.tags @ ["imageResourceId", imageResourceId]
        try
            let imageName, resourceGroupName = getManagedImageNameAndResourceGroupById tags imageResourceId
            let! image = tryGetManagedImageByResourceId context imageResourceId
            match image with
            | None ->
                TraceTags.warning "Managed image was not found, and thus could not be deleted." tags
            | Some _ ->
                do! context.compute.Images.DeleteAsync(resourceGroupName, imageName).AsAsync
                TraceTags.info "Deletion of managed image completed." tags
            return image
        with
        | _  as e ->
            return TraceTags.failwithException e "Failed deleting a managed image" tags
    }


/// Wait until a given condition is met for the specified VM role instance
let vmSatisfiesPredicate (predicate : VirtualMachine -> bool) (context:Context.InfrastructureContext) machineName =
  async {
    let! vm = getVirtualMachine context machineName
    return predicate vm
  }

/// Checks whether a virtual machine with specified name exists under given resource group
let vmExists (context:Context.InfrastructureContext) machineName =
    async {
        try
            let! vm = context.compute.VirtualMachines.GetAsync(context.groupName, machineName).AsAsync
            return true
        with
        | IsAggregateOf SomeResourceNotFoundException e
        | IsAggregateOf SomeNotFoundException e ->
            return false
    }

/// Polling functions
module WaitUntil =

    /// Wait until a VM exists if untilVmExist is true
    /// or until it disappear if untilVmExist is false
    let rec private waitUntilVmExistence untilVmExist (context:Context.InfrastructureContext) machineName =
      async {
            let! vmExists = vmExists context machineName
            if vmExists <> untilVmExist then
                do! Async.Sleep (int Request.Constants.PollingInterval.TotalMilliseconds)
                do! waitUntilVmExistence untilVmExist context machineName
      }

    let vmExists = waitUntilVmExistence true
    let vmRemoved = waitUntilVmExistence false

    /// Wait until a given condition is met for the specified VM role instance
    let rec private waitUntilVmCondition (condition:VirtualMachine -> bool) (context:Context.InfrastructureContext) machineName =
      async {
            let! predicateSatisfied = vmSatisfiesPredicate condition context machineName
            if predicateSatisfied then
                return ()
            else
                do! Async.Sleep (int Request.Constants.PollingInterval.TotalMilliseconds)
                return! waitUntilVmCondition condition context machineName
      }

    /// Wait until a given condition is met for the specified VM role instance or the specified timeout expires
    let private waitUntilVmConditionTimeout condition desiredStateDescription context machineName (timeout:System.TimeSpan) timeoutOption =
        async {
            let waitTask = waitUntilVmCondition condition context machineName
            let! waitHandle =
              if timeout <= System.TimeSpan.Zero then
                    Async.StartChild(waitTask)
                else
                    Async.StartChild(waitTask, (int timeout.TotalMilliseconds))
            try
                do! waitHandle
                return true
            with
                :? System.TimeoutException ->
                    match timeoutOption with
                    | ThrowOnTimeout ->
                        TraceTags.error "Timed-out while waiting for machine to reach desired state."
                                 (context.tags @
                                    [ "machineName", machineName
                                      "desiredStateDescription", desiredStateDescription
                                      "timeout", timeout.ToString()
                                    ])
                        return raise <| System.TimeoutException(sprintf "Machine %s was not %s after %O." machineName desiredStateDescription timeout)
                    | Return ->
                        TraceTags.info "Machine has not reached the desired state in the exepcted time."
                            (context.tags @
                               [ "machineName", machineName
                                 "desiredStateDescription", desiredStateDescription
                                 "timeout", timeout.ToString()
                               ])
                        return false
        }

    open InternalPredicates

    /// Asynchronously wait until the specified VM is ready or a timeout occurs
    let vmIsReadyTimeout context machineName (timeout:System.TimeSpan) timeoutOption =
        waitUntilVmConditionTimeout vmIsReady "ready" context machineName timeout timeoutOption

    /// Asynchronously wait until the specified VM is stopped or a timeout occurs
    let vmIsStoppedTimeout context machineName (timeout:System.TimeSpan) timeoutOption =
        waitUntilVmConditionTimeout vmIsStopped "stopped" context machineName timeout timeoutOption

    /// Asynchronously wait until the specified VM is stopped
    let vmIsStopped context machineName =
        waitUntilVmCondition vmIsStopped context machineName

/// Returns true if the specified machine is ready or is not found
/// Throws ResourceGroupNotFound exception if the VM was part of a ResourceGroup that was deleted
let public isVMReady context machineName =
    vmSatisfiesPredicate InternalPredicates.vmIsReady context machineName

let public isVMStoppedOrDeleted context machineName =
    async {
        try
            return! vmSatisfiesPredicate InternalPredicates.vmIsStopped context machineName
        with
            :? MachineNotFound ->
                TraceTags.warning "Machine not found exception."
                                  (context.tags @
                                    [ "Machine Name", machineName
                                      "Method", "isVMStoppedOrDeleted"])

                return true
    }

/// Download a VHD image from Azure
let downloadVhd tags (storage:BlobStorageContext) vhdUri toDirectory =
    async {
        let uri = Uri(vhdUri)
        let filename = uri.Segments |> Seq.last
        TraceTags.info "Downloading VHD from storage blob"
            (tags @ [ "filename", filename
                      "vhdUri", vhdUri])
        let! blob = storage.client.GetBlobReferenceFromServerAsync(uri).AsAsync

        // copy blob from cloud to local gallery
        let target = System.IO.Path.Combine(toDirectory, filename)
        do! blob.DownloadToFileAsync(target, System.IO.FileMode.CreateNew).AsAsync
        ()
    }

/// Stop a VM, capture the OS image as a managed image, wait for the capture to complete and delete the VM.
/// Returns the absolute URI to the captured image.
///
/// CAUTION: Capture will fail if the machine is in the middle of a shutdown that was initiated
/// from within the VM. (Even though the code below specifically stops the VM using the Azure API)
/// If this is the case then the caller should call waitUntilVmIsStopped before
/// calling this function.
let stopVMCaptureOSImageDeleteVM context storage vmName captureTimeout targetResourceGroup capturedImageName deleteVmOptions =
    async {
        let tags = context.tags @ ["vmName", vmName]
        TraceTags.info "Powering off the machine from Azure" tags
        do! stopVM context vmName
        TraceTags.info "Waiting until machine has stopped before capture" tags
        do! WaitUntil.vmIsStopped context vmName
        TraceTags.info "Capturing machine" tags
        let! capturedVhdUri = captureOSImageAndDeleteVM context storage vmName captureTimeout targetResourceGroup capturedImageName deleteVmOptions
        return capturedVhdUri
    }

/// Information required to remotely connect to a VM, returned by getVirtualMachineRemoteInfo
type VirtualMachineRemoteInfo =
    {
        DnsName: string
        IpAddress: string
        Port: int
    }

/// Retrieve frontend IP address, DNS name and port number mapping to the specified virtual machine and port number
let getVirtualMachineRemoteInfo(context:InfrastructureContext) vmName vmPort =
    async {
        let! vmRequest = context.compute.VirtualMachines.GetWithHttpMessagesAsync(context.groupName, vmName).AsAsync
        let vm = vmRequest.Body
        assertStatusCodeIs context.tags System.Net.HttpStatusCode.OK vmRequest.Response.StatusCode "Could not retrieve RDP connection file."

        let networkInterfaceGroup, networkInterfaceName =
            match vm.NetworkProfile.NetworkInterfaces.[0].Id.Split([|'/'|]) |> Seq.toList |> List.rev with
            | n::"networkInterfaces"::"Microsoft.Network"::"providers"::g::"resourceGroups"::_ -> g, n
            | _ -> TraceTags.failwith "Incorrect network interface URI format" context.tags

        let token = Threading.CancellationToken()

        let! net = context.network.NetworkInterfaces.GetWithHttpMessagesAsync(networkInterfaceGroup, networkInterfaceName, cancellationToken = token).AsAsync
        assertStatusCodeIs context.tags System.Net.HttpStatusCode.OK net.Response.StatusCode "Could not get network interface."

        let loadBalancerGroup, loadBalancerName, loadBalancerRule =
            // For simplicity we assume that all the rules associated with the network interface
            // belong to the same load balancer.
            match net.Body.IpConfigurations.[0].LoadBalancerInboundNatRules.[0].Id.Split([|'/'|]) |> Seq.toList |> List.rev with
            | z::"inboundNatRules"::y::"loadBalancers"::"Microsoft.Network"::"providers"::x::"resourceGroups"::_ -> x, y, z
            | _ -> TraceTags.failwith "Incorrect load balancer URI format" context.tags

        let vmIpConfigId = net.Body.IpConfigurations.[0].Id

        let! lb = context.network.LoadBalancers.GetWithHttpMessagesAsync(loadBalancerGroup, loadBalancerName, cancellationToken = token).AsAsync
        assertStatusCodeIs context.tags System.Net.HttpStatusCode.OK lb.Response.StatusCode "Cannot find specified load balancer"

        // Get the load balancer rule mapping to the requested port
        let rule =
            lb.Body.InboundNatRules
            |> Seq.tryFind (fun r -> r.BackendPort.HasValue && r.BackendPort.Value = vmPort
                                     && r.BackendIPConfiguration.Id = vmIpConfigId )
            |> Option.orDo (fun () -> TraceTags.failwith "Cannot find RDP rule on load balancer" context.tags)

        // Get public IP address of the load balancer
        let publicIpResourceGroup, publicIpName =
            match lb.Body.FrontendIPConfigurations |> Seq.toList with
            | [] -> TraceTags.failwith "Frontend IP configuration missing" context.tags
            | fip::_ ->
                match fip.PublicIPAddress.Id.Split([|'/'|]) |> Seq.toList |> List.rev with
                | n::"publicIPAddresses"::"Microsoft.Network"::"providers"::g::"resourceGroups"::_ -> g,n
                | _ -> TraceTags.failwith "Incorrect public IP URI format" context.tags

        let! publicIps = context.network.PublicIPAddresses.GetWithHttpMessagesAsync(publicIpResourceGroup, publicIpName, cancellationToken = token).AsAsync
        assertStatusCodeIs context.tags System.Net.HttpStatusCode.OK publicIps.Response.StatusCode "Cannot find public IP for load balancer"

        /// Retrieve the frontend port number that maps to the requested backend port
        let frontendPort = rule.FrontendPort.GetValueOrDefault vmPort
        let dnsname = publicIps.Body.DnsSettings.Fqdn
        let ipAddress = publicIps.Body.IpAddress

        return {
                    IpAddress = ipAddress
                    DnsName = dnsname
                    Port = frontendPort
                }
    }

/// Get RDP Remote Desktop connection file.
/// Generate RDP file according to Public IP and Load balancer rule configuration
let getRemoteDesktopFile (context:InfrastructureContext) vmName username =

    let VirtualMachineRdpPort = 3389

    // The "Download RDP File" API from RDFE (https://msdn.microsoft.com/library/azure/jj157183.aspx) does not exist in ARM
    // so we have to implement in ourselves by quering the port number and public DNS address from the load
    // balancer using Azure ARM API.
    // See question posted on Yammer: https://www.yammer.com/microsoft.com/#/threads/inGroup?type=in_group&feedId=4716407
    async {
        let! remoteInfo = getVirtualMachineRemoteInfo context vmName VirtualMachineRdpPort

        // Generate .RDP file content
        let content =
            sprintf """
full address:s:%s:%d
prompt for credentials:i:1
username:s:%s
"""                 remoteInfo.DnsName remoteInfo.Port username

        return System.Text.Encoding.ASCII.GetBytes(content)
    }

/// Create a new certificate and upload it to the vault associated with the specified resource group.
/// The vault is created if it does not already exist.
/// Return the URL to the certificate in the vault
/// NOTES: REQUIRES ADMIN RIGHTS AND ELEVATION. For this reason it cannot be called from an Azure Website
let createRdpCertificateInAzureVault
        (azure:Auth.Subscription)
        (resourceGroup:string)
        vaultName
        (certificate:System.Security.Cryptography.X509Certificates.X509Certificate2)
        (certificatePassword:string) =
    async {
        use c = Context.createVaultContext azure.Authentication
        let vaultUrl = sprintf "https://%s.vault.azure.net/" vaultName
        let secretName = Constants.CertificateKeyNamePrefix + resourceGroup.Replace("_","")

        try
            let! secret = c.GetSecretAsync(vaultUrl, secretName).AsAsync
            return secret.Id
        with
        | :? System.AggregateException as e when
               (e.InnerException :? Models.KeyVaultErrorException)
            && (e.InnerException :?> Models.KeyVaultErrorException).Response.StatusCode  = System.Net.HttpStatusCode.NotFound ->
            let vaultEntryEncoded =
                let contentType = Security.Cryptography.X509Certificates.X509ContentType.Pfx
                let privateKeyEncoded =
                    let privateKeyBytes : byte[] = certificate.Export(contentType, certificatePassword) : byte[]
                    System.Convert.ToBase64String(privateKeyBytes)

                sprintf """{
                 "data": "%s",
                 "dataType" :"pfx",
                 "password": "%s"}""" privateKeyEncoded certificatePassword
                 |> System.Text.Encoding.UTF8.GetBytes
                 |> System.Convert.ToBase64String

            // Load certificate in vault
            let! vaultEntry = c.SetSecretAsync(vaultUrl, secretName, vaultEntryEncoded).AsAsync
            return vaultEntry.Id
    }

/// Deletes the specified virtual machine extension
/// Returns true if the extension was found and deleted and false if not found
let deleteVmExtensionIfExists (context:InfrastructureContext) machineName customExtensionName =
    async{
        try
            let! token = Async.CancellationToken
            do! context.compute.VirtualMachineExtensions.DeleteAsync(context.groupName, machineName, customExtensionName, token)
                |> Async.AwaitTask
                |> Async.Ignore
            return true
        with
        | IsAggregateOf SomeResourceNotFoundException e ->
            TraceTags.info "Extension not found for specifed machine"
                (context.tags @
                    [ "customExtensionName", customExtensionName
                      "machineName", machineName
                      "resourceGroup", context.groupName
                    ])
            return false
    }

/// Create an Azure managed disk image from the OS disk of an existing OS image and a managed data disk
/// The source image and managed disk are assumed to be in the same resource group.
let createImageFromManagedOSImageAndDataManagedDisk (c:InfrastructureContext) vmName sourceResourceGroup sourceImageName dataManagedDiskName targetResourceGroup targetImageName =
    async {
        try
            TraceTags.info "Attaching data disk to managed image." (c.tags @ [ "image", vmName
                                                                               "sourceResourceGroup", sourceResourceGroup
                                                                               "sourceImageName", sourceImageName
                                                                               "targetResourceGroup", targetResourceGroup
                                                                               "targetImageName", targetImageName
                                                                               "dataManagedDiskName", dataManagedDiskName ])

            let! sourceImage = c.compute.Images.GetAsync(sourceResourceGroup, sourceImageName).AsAsync
            let! dataManagedDisk = c.compute.Disks.GetAsync(sourceResourceGroup, dataManagedDiskName).AsAsync

            let parameters =
                Image(
                        Location = sourceImage.Location,
                        StorageProfile =
                                ImageStorageProfile(
                                        OsDisk = ImageOSDisk(
                                                    OsState = OperatingSystemStateTypes.Generalized,
                                                    OsType = sourceImage.StorageProfile.OsDisk.OsType,
                                                    BlobUri = sourceImageName
                                                 ),
                                        DataDisks = [| ImageDataDisk(Lun=1, ManagedDisk = ComputeSubResource(Id = dataManagedDisk.Id)) |]
                                ),
                        Tags = dict ["creationTime", DateTime.UtcNow.ToString("u")]
                    )

            let! requestResponse = c.compute.Images.BeginCreateOrUpdateWithHttpMessagesAsync(targetResourceGroup, targetImageName, parameters).AsAsync
            TraceTags.info "Requested creation of a managed image from an existing disk image"
                (c.tags @
                    [ "sourceResourceGroup", sourceResourceGroup
                      "sourceImageName", sourceImageName
                      "targetResourceGroup", targetResourceGroup
                      "targetImageName", targetImageName
                      "dataManagedDiskName", dataManagedDiskName
                      "location", sourceImage.Location
                      "requestId", requestResponse.RequestId
                      "imageId", requestResponse.Body.Id
                      "response", (Json.Compact.serialize requestResponse.Body)
                      "responseInfo", (sprintf "%A" requestResponse.Response)
                    ])

            return requestResponse.Body.Id, requestResponse.GetAsyncOperationUrl(c.tags)
        with
        | e ->
            return TraceTags.failwithException e "Failed to request image creation."
                     (c.tags @ [ "sourceResourceGroup", sourceResourceGroup
                                 "sourceImageName", sourceImageName
                                 "targetResourceGroup", targetResourceGroup
                                 "targetImageName", targetImageName
                                 ])
    }

/// Update VM scaleset capacity
let setVmScaleSetCapacityAsync (c:Context.InfrastructureContext) resourceGroupName vmScaleSetName (capacity:int) =
    async {
        let tags = c.tags @ [ "resourceGroupName", resourceGroupName
                              "vmScaleSetName", vmScaleSetName
                              "capacity", capacity.ToString() ]
        TraceTags.info "Updating capacity for VM ScaleSet" tags
        try
            let! scaleSet = c.compute.VirtualMachineScaleSets.GetAsync(resourceGroupName, vmScaleSetName) |> Async.AwaitTask
            scaleSet.Sku.Capacity <- Nullable <| int64 capacity
            let! azureAsyncOperation = c.compute.VirtualMachineScaleSets.BeginCreateOrUpdateWithHttpMessagesAsync(resourceGroupName, vmScaleSetName, scaleSet).AsAsync
            TraceTags.info "Submitted request to update VM scale set capacity."
                (tags @ ["requestId", azureAsyncOperation.RequestId])
            return azureAsyncOperation.GetAsyncOperationUrl(tags)
        with
        | IsAggregateOf AnyCloudError error as e ->
            return TraceTags.failwithException e "Failed to update capacity for VM ScaleSet. (CloudException error)" (tags@["CloudErroCode", error.Code; "CloudErrorMessage", error.Message])
        | e ->
            return TraceTags.failwithException e "Failed to update capacity for VM ScaleSet. (Exception)" tags
    }

/// Gets the result of a VM scale set update request.
/// Return None if capture is still in progress, the URI path to the VHD if the capture has completed,
/// and throw if an error occurs.
let getVMScaleSetUpdateResult (context:Context.InfrastructureContext) (azureAsyncOperationUrl:AsyncOperationUrl) =
    async {
        // See https://github.com/Azure/azure-sdk-for-net/blob/master/src/SdkCommon/ClientRuntime.Azure/ClientRuntime.Azure/CommonModels/AzureAsyncOperation.cs
        // for details of the AzureAsyncOperation type.
        let! completionResponse = getAsyncOperationStatusWithOutput<AzureAsyncOperation> context.tags context.compute azureAsyncOperationUrl

        let returnValue =
            match completionResponse.Status.Status with
            | Microsoft.Rest.Azure.AzureAsyncOperation.SuccessStatus ->
                TraceTags.info "Successfully updated VM Scale Set."
                    (context.tags @ [ "output", completionResponse.Output.ToString() ])
                OperationStatus.Succeeded completionResponse.Status
            | Microsoft.Rest.Azure.AzureAsyncOperation.InProgressStatus ->
                OperationStatus.InProgress
            | Microsoft.Rest.Azure.AzureAsyncOperation.FailedStatus
            | Microsoft.Rest.Azure.AzureAsyncOperation.CanceledStatus ->
                let cloudError =
                    if not (isNull completionResponse.Output) && not (isNull completionResponse.Output.Error) then
                        printCloudError completionResponse.Output.Error 0
                    else
                        "null"
                TraceTags.error "Failed to update VM Scale Set."
                    (context.tags @ [ "status", completionResponse.Status.Status
                                      "RawResponseContent", completionResponse.RawResponseContent
                                      "cloudError", cloudError])
                OperationStatus.Failed (completionResponse.Status, cloudError)
            | status ->
                TraceTags.error "VM Scale Set update returned an unknown operation status."
                    (context.tags @[ "status", status ])
                OperationStatus.Undetermined (completionResponse.Status, azureAsyncOperationUrl)

        return returnValue
    }
