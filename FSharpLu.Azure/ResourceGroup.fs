/// F# helpers to the Azure Resource Group (ARM) API
/// Copyright (c) Microsoft Corporation.
module Microsoft.FSharpLu.Azure.ResourceGroup

open Microsoft.Azure.Management.ResourceManager
open Microsoft.Azure.Management.ResourceManager.Models
open Microsoft.FSharpLu.Azure.AppInsights.DiagnosticsAndAppInsights
open Microsoft.FSharpLu.Async
open Microsoft.FSharpLu.Azure.Context
open Microsoft.FSharpLu.Azure.Request
open Microsoft.FSharpLu
open System
open Microsoft.FSharpLu.ErrorHandling

/// Common provisioning states. (Used to be defined in Microsoft.Azure.Management.Resources.Models but was removed in 3.4.0-preview
type ProvisioningState =
    | NotSpecified
    | Accepted
    | Running
    | Registering
    | Creating
    | Created
    | Deleting
    | Deleted
    | Canceled
    | Failed
    | Succeeded

/// Constraint on the size of the name of a deployment in ARM
let MaxAzureDeploymentNameLength = 64

/// The specified resource group name was not found
type ResourceGroupNotFound(groupName:string, message:string, innerException:Exception) =
   inherit Exception((sprintf "Could not find resource group %s. %s" groupName message), innerException)
   new(groupName, message) = ResourceGroupNotFound(groupName, message, null)

let exists (c:Context.InfrastructureContext) groupName =
    async {
        let! groupExists = c.resource.ResourceGroups.CheckExistenceAsync(groupName).AsAsync
        return groupExists
    }

/// Check if the specified resource group already exists
let exists2 (azure:Auth.Subscription) groupName =
    async {
        let! token = Auth.getAuthorizationToken azure.Authentication
        use computeClient  =
            let token = Microsoft.Rest.TokenCredentials(token)
            new ResourceManagementClient(token, SubscriptionId = azure.SubscriptionId)

        return! computeClient.ResourceGroups.CheckExistenceAsync(groupName).AsAsync
    }

/// Create an Azure Resource Group (does not fail if it already exists)
let create (c:Context.InfrastructureContext) groupName location tagsMap =
    async {

        TraceTags.info "Creating the resource group" (c.tags @ ["groupName", groupName])
        let resourceGroup =
            ResourceGroup(
                Location = location,
                Tags = (tagsMap |> Map.ofSeq))

        return! c.resource.ResourceGroups.CreateOrUpdateAsync(groupName, resourceGroup).AsAsync
    }

/// List resource group matching the specified criteria
let list (c:Context.InfrastructureContext) (tagName:string) (tagValue:string) =
    async {
        TraceTags.info "Listing resource groups" c.tags
        let! token = Async.CancellationToken
        let filter = Microsoft.Rest.Azure.OData.ODataQuery<ResourceGroupFilter>( fun x -> x.TagName = tagName && x.TagValue = tagValue)
        return!
            enumerateAllPagesAsync
                c.tags
                "resource groups"
                {
                    getFirstPage = fun () -> c.resource.ResourceGroups.ListAsync(filter, token)
                    getNextPage = fun link -> c.resource.ResourceGroups.ListNextAsync(link, token)
                    pageAsList = fun page -> page |> Seq.toList
                    getLink = fun page -> page.NextPageLink
                }
    }

/// Try to delete an Azure Resource Group and its associated resources (virtual machines, ...)
/// If the resource group does not exist, or if a conflict occurs (locked resource group)
/// just log an error message without throwing.
let tryDelete (c:Context.InfrastructureContext) groupName =
    async {
        let tags = c.tags @ ["groupName", groupName]
        TraceTags.info "Deleting resource group." tags
        try
            do! c.resource.ResourceGroups.DeleteAsync(groupName).AsAsync
            TraceTags.info "Resource group deleted." tags
        with
        | NotFoundException e
        | IsAggregateOf SomeResourceGroupNotFoundException e ->
            TraceTags.error "Could not find resource group." (tags @ [ "exception", e.ToString() ])
        | IsAggregateOf InternalServerError e ->
            let rec cloudErrorToTags (errs: Microsoft.Rest.Azure.CloudError seq) (tags: (string*string) list) =
                match Seq.tryHead errs with
                | None -> tags
                | Some err ->
                    let additionalInfo = err.AdditionalInfo |> Seq.map (fun i -> i.Type, i.Info.ToString()) |> List.ofSeq
                    cloudErrorToTags (Seq.append (Seq.tail errs) err.Details) (tags @ ["Message", err.Message; "Code", err.Code; "Target", err.Target] @ additionalInfo)

            TraceTags.error "Internal server error in Azure attempting to delete resource group. This is likely an intermittent failure, and the resource group can be deleted manually."
                (tags @ (cloudErrorToTags [e] [] ))
        | IsAggregateOf (SomeCloudExceptionStatusCode System.Net.HttpStatusCode.Conflict) e ->
            TraceTags.error "Could not delete resource group due to conflict error. See response content for details."
                (tags @ [ "response", e.Content ])
        // Catching all other cloud errors to capture maximum information in Application Insights.
        | IsAggregateOf AnyCloudError e ->
            let additionalInfo =
                if not <| isNull e.AdditionalInfo then
                    let sb = System.Text.StringBuilder()
                    for info in e.AdditionalInfo do
                        sb.AppendLine(sprintf "Info: %A; Type: %s" info.Info info.Type) |> ignore
                    sb.ToString()
                else
                    "null"
            let details =
                if not <| isNull e.Details then
                    let sb = System.Text.StringBuilder()
                    for detail in e.Details do
                        sb.AppendLine(printCloudError detail 0) |> ignore
                    sb.ToString()
                else
                    "null"
            TraceTags.error "Exception while trying to delete a resource group."
                (tags @ ["e.Code", e.Code
                         "e.Message", e.Message
                         "e.Target", e.Target
                         "e.AdditionalInfo", additionalInfo
                         "e.Details", details])
    }

/// Initiate asynchronous deletion of an Azure Resource Group and its associated resources (virtual machines, ...)
/// Return the request Id of the operation.
/// Raise an exception if the group does not exist
let deleteAsync (c:Context.InfrastructureContext) groupName =
    async {
        let tags = c.tags @ ["groupName", groupName]
        try
            TraceTags.info "Initiating deletion of resource group" tags
            let! request = c.resource.ResourceGroups.BeginDeleteWithHttpMessagesAsync(groupName).AsAsync
            return request.RequestId
        with
        | NotFoundException e
        | IsAggregateOf SomeResourceGroupNotFoundException e ->
            TraceTags.error "Could not find resource group" (tags @ [ "exception", e.ToString() ])
            return raise <| ResourceGroupNotFound(groupName, e.Message)
        | IsAggregateOf AnyCloudError e ->
            return TraceTags.failwith "Could not initiate deletion of resource group" (tags @ [ "exception", e.ToString() ])
    }

/// Generates a random name to use as a deployment name in azure
let generateRandomAzureDeploymentName (prefix:string) =
    // one char for the underscode and another one for at least one random character
    let minimalSuffixLength = 2
    if prefix.Length + minimalSuffixLength > MaxAzureDeploymentNameLength then
        raise (System.ArgumentOutOfRangeException(sprintf "The prefix length should me be less than %d" MaxAzureDeploymentNameLength))

    let charList = "01234567891abcdefghijklmnopqrstuvwxyz"
    let generator = Random(int System.DateTime.UtcNow.Ticks)

    (fun _ -> charList.[(generator.Next() % charList.Length)] )
    |> Array.init (MaxAzureDeploymentNameLength - (prefix.Length + minimalSuffixLength))
    |> String
    |> sprintf "%s_%s" prefix

/// Get log messages list of operations for the deployment
let getDeploymentOperationsList (c:Context.InfrastructureContext) deploymentName =
    async{
        let! result =
            enumerateAllPagesAsync
                c.tags
                "Deployment Operations Log"
                {
                    getFirstPage = fun () -> c.resource.DeploymentOperations.ListAsync(c.groupName, deploymentName)

                    getNextPage = c.resource.DeploymentOperations.ListNextAsync

                    pageAsList =
                        fun operations ->
                                 operations
                                 |> Seq.map (fun s-> Option.ofObj s.Properties.StatusMessage,
                                                     s.Properties.ProvisioningState,
                                                     Option.ofObj s.Properties.TargetResource,
                                                     s.Properties.Timestamp,
                                                     match Enum.TryParse<Net.HttpStatusCode>(s.Properties.StatusCode) with
                                                     | true, c -> c
                                                     | false, _ ->
                                                        TraceTags.error "Could not parse HttpStatusCode returned when getting deployment operation list for specified deployment"
                                                            (c.tags @ [ "statusCode", s.Properties.StatusCode
                                                                        "deploymentName", deploymentName])
                                                        enum 0)
                                 |> List.ofSeq

                    getLink = fun r -> r.NextPageLink
                }
        return result
    }

/// Defines types used to report ARM deployment information in getArmDeploymentErrors
module DeploymentReport =
    open System.Collections.Generic
    type StatusCode = string
    type StatusProperties =
        {
            DisplayStatus : string
            Message : string
            Level : string
            Time : string
            Code : string
            Index : int
        }
    type OperationProperties = IDictionary<string, string>
    type VmExtensionProperties = IDictionary<string, string>
    type ResourceId = string
    type VmExtensions =
        {
            ExtensionName : string
            Properties : VmExtensionProperties
            Statuses : StatusProperties list
            SubStatuses : StatusProperties list
        }
    type Operation =
        {
            StatusMessage : string
            State : string
            TargetResource : string
            TimeStamp : string
            StatusCode : string
        }
    type ArmDeploymentFailureReport =
        {
            DeploymentName : string
            Operations : Operation list
            VmExtensions : VmExtensions list
            FailedResources : ResourceId list
        }

open DeploymentReport

/// Gather all ARM deployment errors relevant to the specified ARM deployment and return a structure
/// containing all the gathered error statuses and details
let getArmDeploymentErrors (c:Context.InfrastructureContext) deploymentName =
    async {
        let! operationList = getDeploymentOperationsList c deploymentName
        let failedOperationsAndResources =
            operationList
            |> Seq.filter (fun (_, _, _, _, statusCode) -> statusCode <> Net.HttpStatusCode.OK)
            |> Seq.map (fun (statusMessage, state, targetResource, timeStamp, statusCode) ->
                let parsedState =
                    Parsing.Union.tryParse state
                    |> Option.defaultValue ProvisioningState.NotSpecified

                let failedResource =
                    match parsedState with
                    | ProvisioningState.Succeeded
                    | ProvisioningState.Created
                    | ProvisioningState.Deleted ->
                        None
                    | ProvisioningState.Accepted
                    | ProvisioningState.Running
                    | ProvisioningState.Registering
                    | ProvisioningState.Deleting
                    | ProvisioningState.Creating ->
                        None
                    | ProvisioningState.Canceled
                    | ProvisioningState.Failed
                    | ProvisioningState.NotSpecified ->
                        targetResource

                {
                    StatusMessage = defaultArg (statusMessage |> Option.map Json.Compact.serialize) String.Empty
                    State = state
                    TargetResource =
                        targetResource
                        |> Option.map (fun (tr: TargetResource) -> tr.Id)
                        |> Option.defaultValue String.Empty
                    TimeStamp = timeStamp.ToString()
                    StatusCode = statusCode.ToString()
                }, parsedState, failedResource
            )

        let failedOperations =
            failedOperationsAndResources
            |> Seq.map (fun (operationProperties, _, _) -> operationProperties)
            |> Seq.toList

        let failedResources =
            failedOperationsAndResources
            |> Seq.choose (fun (_, _, resource) -> resource)
            |> Seq.map (fun r -> r.Id)
            |> Seq.toList

        let! vmExtensions =
            failedOperationsAndResources
            |> Seq.choose (fun (_, _, resource) -> resource)
            |> Seq.filter (fun resource ->  String.Compare(resource.ResourceType, "Microsoft.Compute/virtualMachines/extensions", true) = 0)
            |> Seq.choose (fun resource ->
                // ResourceName is of the form VMNAME/CustomScriptExtension
                let resourceComponents = resource.ResourceName.Split('/') |> Seq.toList
                match resourceComponents with
                | [] | [_] | _::(_::(_::_)) ->
                    None // Not a VM extension
                | [vmName; extensionName] ->
                    Some (vmName, extensionName))
            |> Seq.map (fun (vmName, extensionName) ->
                async {
                    let getStatusDetails index (status:Microsoft.Azure.Management.Compute.Models.InstanceViewStatus) =
                        {
                            DisplayStatus = status.DisplayStatus
                            Message = status.Message.Replace(@"\n", "\n")
                            Level = sprintf "%A" status.Level
                            Time = status.Time.ToString()
                            Code = status.Code
                            Index = index
                        }

                    // Gather extra logs if it's an Azure VM Extensions
                    try
                        let! vmExtension =
                            c.compute.VirtualMachineExtensions.GetWithHttpMessagesAsync(
                                    c.groupName,
                                    vmName,
                                    extensionName,
                                    expand = sprintf "%A" Microsoft.Azure.Management.Compute.Models.InstanceViewTypes.InstanceView,
                                    cancellationToken = Async.DefaultCancellationToken)
                            |> Async.AwaitTask
                        try
                            return
                                {
                                    ExtensionName = extensionName
                                    Statuses = vmExtension.Body.InstanceView.Statuses
                                               |> Seq.mapi getStatusDetails
                                               |> Seq.toList
                                    SubStatuses = vmExtension.Body.InstanceView.Substatuses
                                                    |> Seq.mapi getStatusDetails
                                                    |> Seq.toList
                                    Properties = dict
                                                    [
                                                        "Machine", vmName
                                                        "Extension", extensionName
                                                        "Name", vmExtension.Body.InstanceView.Name
                                                        "ProvisioningState", vmExtension.Body.ProvisioningState
                                                        "VirtualMachineExtensionType", vmExtension.Body.VirtualMachineExtensionType
                                                    ]
                                }
                        with
                        ex -> return
                                {
                                    ExtensionName = extensionName
                                    Statuses = []
                                    SubStatuses = []
                                    Properties =
                                        dict
                                            [
                                                "LogError", "Failed to retrieve ARM error statuses"
                                                "Exception", ex.ToString()
                                            ]
                                }
                    with
                    ex -> return
                            {
                                ExtensionName = extensionName
                                Statuses = []
                                SubStatuses = []
                                Properties =
                                    dict
                                        [
                                            "LogError", "Failed to retrieve VM machine extensions details"
                                            "Exception", ex.ToString()
                                        ]
                            }
                    })
                |> Async.sequential

        return
            {
                Operations = failedOperations
                FailedResources = failedResources
                VmExtensions = vmExtensions
                DeploymentName = deploymentName
            }
    }

/// Status of an asynchronous operation with success type 's,
/// error type 'e and undetermined error type 'u
type OperationStatus<'s, 'e, 'u> =
    /// Operation Succeeded
    | Succeeded of 's
    /// Operation  failed
    | Failed of 'e
    /// Operation still in progress
    | InProgress
    /// Could not retrive the operation status: e.g. a resource group or deployment could not be found
    | Undetermined of 'u

/// Log a report of all ARM deployment errors relevant to the specified ARM deployment
let logArmDeploymentErrors tags (errors:ArmDeploymentFailureReport) =
    async {
        if List.isEmpty errors.Operations then
            TraceTags.info "ARM deployment succeeded" tags
            return OperationStatus.Succeeded ()
        else
            TraceTags.error "ARM deployment encountered some errors"
                        (tags @ [
                                    "Operations", Json.Compact.serialize errors.Operations
                                    "FailedResources", Json.Compact.serialize errors.FailedResources
                                    "VmExtensions", Json.Compact.serialize errors.VmExtensions
                                ])
            return OperationStatus.Failed errors

    }

/// Retrieve and log to AppInsights the ARM deployment status and error report.
/// NOTE: If the deployment report cannot be retrieved then log the exception silently
/// and return success.
let getAndlogArmDeploymentErrors (c:Context.InfrastructureContext) deploymentName additionalTags
        : Async<OperationStatus<unit, ArmDeploymentFailureReport, Exception>> =
    async {
        let tags = c.tags@additionalTags@["deploymentName", deploymentName]
        try
            let! errors = getArmDeploymentErrors c deploymentName
            return! logArmDeploymentErrors tags errors
        with
        | ex ->
            TraceTags.error "Got exception when trying to retrieve ARM deployment errors"
                        (tags @ [ "exception", ex.ToString() ])
            return OperationStatus.Undetermined ex
    }

/// Returns all the deployments for the specified resource group
let getDeployments (context:InfrastructureContext) =
    async {
        TraceTags.info "Listing all deployments for resource group" (context.tags @ ["groupName", context.groupName ])
        let! token = Async.CancellationToken
        return!
            enumerateAllPagesAsync
                context.tags
                "resource group deployments"
                {
                    getFirstPage = fun () -> context.resource.Deployments.ListByResourceGroupAsync(context.groupName)
                    getNextPage = fun link -> context.resource.Deployments.ListByResourceGroupNextAsync(link, token)
                    pageAsList = fun page -> page |> Seq.toList
                    getLink = fun page -> page.NextPageLink
                }
    }

/// Create a new Azure Resource Deployment with a specific name
let createTemplateDeploymentWithName (c:Context.InfrastructureContext) groupName template templateParameters (deploymentName:string) =
    async {
        let name =
            if deploymentName.Length > MaxAzureDeploymentNameLength then
                TraceTags.warning "Truncating ARM deployment name"
                            (c.tags @ [
                                       "MaxAzureDeploymentNameLength", MaxAzureDeploymentNameLength.ToString()
                                       "deploymentName", deploymentName ])
                deploymentName.Remove(MaxAzureDeploymentNameLength)
            else
                deploymentName

        try
            let deployment =
                    Deployment(
                        Properties =
                            DeploymentProperties(
                                    Mode = DeploymentMode.Incremental,
                                    Template = template,
                                    Parameters = templateParameters))

            let! deploymentExtended = c.resource.Deployments.BeginCreateOrUpdateAsync(groupName, name, deployment).AsAsync
            TraceTags.info "ARM deployment completed."
                            (c.tags @
                                [ "Id", deploymentExtended.Id
                                  "ResourceGroup", groupName
                                  "Name", deploymentExtended.Name
                                  "ProvisioningState", deploymentExtended.Properties.ProvisioningState
                                  "Timestamp", deploymentExtended.Properties.Timestamp.ToString()
                                 ])
            return deploymentExtended.Name, deploymentExtended.Id
        with
        | IsAggregateOf AnyCloudError error
        | IsAggregateOf SomeLongRunningOperationFailedException error
        | IsAggregateOf SomeConflictErrorException error
        | (IsAggregateOf InvalidTemplateDeployment error) as ex ->
            try
                let rec formatDetails (subError:Microsoft.Rest.Azure.CloudError) indentLevel =
                    if isNull subError.Details then
                        ""
                    else
                        let indent = System.String(' ', indentLevel * 2)
                        subError.Details
                            |> Seq.map (fun d ->
                                            let entry = indent + sprintf "%s, %s" d.Code d.Message
                                            let nested = formatDetails d (indentLevel+1)
                                            if nested = String.Empty then
                                                entry
                                            else
                                                sprintf "%s [\n%s] " entry nested
                                        ) |> Text.join "\n"

                let! loggedErrors =
                    getAndlogArmDeploymentErrors c name
                       (c.tags @
                        [ "DeploymentName", name
                          "ResourceGroup", groupName
                          "CloudErrorCode", error.Code
                          "CloudErrorMessage", error.Message
                          "CloudErrorTarget", error.Target
                          "CloudErrorDetails", formatDetails error 0 ])
                ()
            with ex ->
               TraceTags.error "Deployment failed and an exception was thrown when trying to retrieve error details" ["DeploymentName", name; "Exception", ex.ToString()]
            return Async.reraise ex
    }

/// Create a new Azure Resource Deployment
let createTemplateDeployment (c:Context.InfrastructureContext) groupName template templateParameters =
    async {
        TraceTags.info "Creating a deployment for specified resource group" (c.tags @ [ "groupName", groupName ])
        let deploymentName = groupName + "_" + System.DateTime.UtcNow.ToString("yyyyMMdd-HHmmssUz").Replace("+","")
        return! createTemplateDeploymentWithName c groupName template templateParameters deploymentName
    }

/// Retrieve status of an ARM deployment,
/// if the ARM deployment failed, return the full ARM deployment report.
/// https://msdn.microsoft.com/en-us/library/azure/dn790519.aspx
let getDeploymentStatus (c:Context.InfrastructureContext) deploymentName =
    async {
        let! deployment = c.resource.Deployments.GetAsync(c.groupName, deploymentName) |> Async.AwaitTask

        let state = deployment.Properties.ProvisioningState
        match Parsing.Union.tryParse<ProvisioningState> state with
        | Some ProvisioningState.Succeeded
        | Some ProvisioningState.Created
        | Some ProvisioningState.Deleted ->
            return OperationStatus.Succeeded state

        | Some ProvisioningState.Canceled
        | Some ProvisioningState.Failed
        | Some ProvisioningState.NotSpecified ->
            let! errors =
                getArmDeploymentErrors c deploymentName
                |> Async.Catch

            match errors with
            | Choice2Of2 exn ->
                return OperationStatus.Undetermined (System.Exception("Got exception when trying to retrieve ARM deployment errors", exn))
            | Choice1Of2 errors ->
                if List.isEmpty errors.Operations then
                    return OperationStatus.Undetermined (System.Exception "Inconsistent states: deployment failed but deployment report is inconclusive")
                else
                    return OperationStatus.Failed (state, errors)

        | Some ProvisioningState.Accepted
        | Some ProvisioningState.Running
        | Some ProvisioningState.Registering
        | Some ProvisioningState.Deleting
        | Some ProvisioningState.Creating ->
            return OperationStatus.InProgress

        | None ->
            return TraceTags.failwith "Unreachable code. Provisioning state unkown."
                            (c.tags @ [ "provisioningState", sprintf "%A" deployment.Properties.ProvisioningState ])
    }

/// Retrieve status of an ARM deployment with full ARM deployment error report,
/// and log it to AppInsights.
let getAndLogDeploymentStatus (c:Context.InfrastructureContext) deploymentName =
    async {
        let! operationStatus = getDeploymentStatus c deploymentName
        let tags = c.tags@["deploymentName", deploymentName]
        match operationStatus with
        | OperationStatus.InProgress ->
            TraceTags.info "ARM deployment in progress" tags
        | OperationStatus.Succeeded state ->
            let tags = tags@[ "detectedState", sprintf "%A" state]
            TraceTags.info "ARM deployment succeeded" tags
        | OperationStatus.Failed (state, errors) ->
            let tags = tags@[ "detectedState", sprintf "%A" state]
            TraceTags.error "ARM deployment encountered some errors"
                (tags @ [
                            "Operations", Json.Compact.serialize errors.Operations
                            "FailedResources", Json.Compact.serialize errors.FailedResources
                            "VmExtensions", Json.Compact.serialize errors.VmExtensions
                        ])
        | OperationStatus.Undetermined ex ->
            TraceTags.error "Got exception when trying to retrieve ARM deployment errors"
                        (tags @ [ "exception", ex.ToString() ])
    }

/// Returns the deployment outputs if they exist (Some object) or None if null
let tryGetDeploymentOutputs (c:Context.InfrastructureContext) deploymentName =
    async {
        TraceTags.info "Querying deployment outputs for specified resource group deployment"
                            (c.tags @ [ "groupName", c.groupName
                                        "deploymentName", deploymentName ])
        let! deployment = c.resource.Deployments.GetAsync(c.groupName, deploymentName) |> Async.AwaitTask
        if isNull deployment.Properties || isNull deployment.Properties.Outputs then
            return None
        else
            return Some deployment.Properties.Outputs
    }

/// Returns true if the status code indicates a successful deployment
let isSuccessDeploymentState (state:string) =
    match Parsing.Union.tryParse<ProvisioningState> state with
    | Some ProvisioningState.Succeeded
    | Some ProvisioningState.Created
    | Some ProvisioningState.Deleted ->
        true
    | Some ProvisioningState.Canceled
    | Some ProvisioningState.Failed
    | Some ProvisioningState.NotSpecified
    | Some ProvisioningState.Accepted
    | Some ProvisioningState.Running
    | Some ProvisioningState.Registering
    | Some ProvisioningState.Deleting
    | Some ProvisioningState.Creating
    | None ->
        false

/// Is the deployment still in progress (returns true) or has it reached a final state (success or error) (returns false)
let inline isDeploymentInProgress tags state =
    match Parsing.Union.tryParse state with
    | Some ProvisioningState.Succeeded
    | Some ProvisioningState.Created
    | Some ProvisioningState.Deleted
    | Some ProvisioningState.Canceled
    | Some ProvisioningState.Failed
    | Some ProvisioningState.NotSpecified ->
        false
    | Some ProvisioningState.Accepted
    | Some ProvisioningState.Running
    | Some ProvisioningState.Registering
    | Some ProvisioningState.Deleting
    | Some ProvisioningState.Creating ->
        true
    | None ->
        TraceTags.failwith "Could not parse unknown state code" (tags @ ["state", sprintf "%A" state])

/// Returns true if the resource group does not have any deployment in progress
let allDeploymentCompleted (c:Context.InfrastructureContext) =
    async {
        let! deployments = getDeployments c
        return deployments |> Seq.forall (fun d -> not <| isDeploymentInProgress c.tags d.Properties.ProvisioningState)
    }

/// Poll until Azure Resource Group deployment completes. Returns the provisioning state
/// (https://msdn.microsoft.com/en-us/library/azure/dn790519.aspx)
/// Throws an exception if the specified timeout occurs
let waitUntilDeploymentCompletesOrTimeout (c:Context.InfrastructureContext) deploymentName timeout =
    let condition () =
        async {
            let! s = getDeploymentStatus c deploymentName
            return
                match s with
                | OperationStatus.Succeeded s -> Some (true, s)
                | OperationStatus.Failed (s, report) -> Some (false, s)
                | OperationStatus.InProgress ->
                    None
                | OperationStatus.Undetermined u ->
                    None // if deployment status is undetermined we assume
                         // that the deployment is still in progress.
                         // If the failure to retrive status is non-transient the retry
                         // operation will fail after the timeout.
        }

    Async.retryUntilSome timeout Microsoft.FSharpLu.Azure.Request.Constants.PollingInterval condition

/// Poll Azure until the specified deployment completes successfully,
/// throws an exception if the the specified timeout period expires
let waitUntilDeploymentSucceedsOrTimeout context deploymentName (timeout:System.TimeSpan) timeoutOption =
    async {
        let rec waitTask() = async {
            let! state = getDeploymentStatus context deploymentName
            match state with
            | OperationStatus.InProgress ->
                do! Async.Sleep (int Request.Constants.PollingInterval.TotalMilliseconds)
                return! waitTask()
            | OperationStatus.Succeeded state ->
                return ()
            | OperationStatus.Undetermined exn ->
                TraceTags.failwith "ARM deployment status could not be retrived"
                                    (context.tags @ [ "deploymentName", deploymentName
                                                      "exception", sprintf "%A" exn ])

            | OperationStatus.Failed (failureState, report) ->
                TraceTags.failwith "ARM deployment failed while polling for success"
                                    (context.tags @ [ "deploymentName", deploymentName
                                                      "failureState", sprintf "%A" failureState
                                                      "report", sprintf "%A" report])
        }
        let timeout =
            if timeout <= System.TimeSpan.Zero then
                -1
            else
                int <| timeout.TotalMilliseconds

        let! waitHandle =
            Async.StartChild(waitTask(), timeout)
        try
            do! waitHandle
            return true
        with
        :? System.TimeoutException ->
            match timeoutOption with
            | ThrowOnTimeout ->
                TraceTags.error "Deployment was still not ready after the timeout period."
                                    (context.tags @ [ "groupName", context.groupName
                                                      "timeout", timeout.ToString()])
                return raise <| System.TimeoutException(sprintf "Deployment %s was not ready after %O." context.groupName timeout)
            | Return ->
                TraceTags.info "Deployment was still not ready after the timeout period."
                                    (context.tags @ [ "groupName", context.groupName
                                                      "timeout", timeout.ToString()])

                return false
    }

let private ensureTagHasNoSingleQuoteCharacters (s:string) tags =
    if s.Contains("'") then
        TraceTags.failwith "Search criteria contains unsupported single quote characters." tags

/// List disks in a given resource group and with the specified tag name and tag value.
let listDisksByResourceGroupAndTag
        (context:Context.InfrastructureContext)
        resourceGroupName
        (tagName, tagValue) =
    async {
        let logTags = context.tags @
                        [ "resourceGroupName", resourceGroupName
                          "search_tagName", tagName
                          "search_tagValue", tagValue
                        ]
        let! token = Async.CancellationToken
        try
            ensureTagHasNoSingleQuoteCharacters tagName logTags
            ensureTagHasNoSingleQuoteCharacters tagValue logTags
            let! disks = enumerateAllPagesAsync
                                context.tags
                                (sprintf "Managed disks under resource group %s." resourceGroupName)
                                {
                                    getFirstPage = fun () -> context.compute.Disks.ListByResourceGroupWithHttpMessagesAsync(resourceGroupName)
                                    getNextPage = fun link -> context.compute.Disks.ListByResourceGroupNextWithHttpMessagesAsync(link, null, token)
                                    pageAsList = fun page -> page.Body |> Seq.toList
                                    getLink = fun page -> page.Body.NextPageLink
                                }

            // The API for getting disks does not allow for sending filters other than the resource group name, so we have to filter after getting all of the results.
            return disks
                |> List.filter (fun disk -> not (isNull disk.Tags) && disk.Tags.ContainsKey(tagName) && disk.Tags.[tagName] = tagValue)

        with
        | IsAggregateOf SomeResourceGroupNotFoundException _ ->
            TraceTags.info (sprintf "Resource group %s was not found." resourceGroupName) logTags
            return []
    }

/// List resources in a given resource group with the specified resource type
/// and with the specified tag name and tag value
let listImagesByResourceGroupAndTag
        (context:Context.InfrastructureContext)
        resourceGroupName
        (tagName, tagValue) =
    async {
        let logTags = context.tags @
                        [ "resourceGroupName", resourceGroupName
                          "search_tagName", tagName
                          "search_tagValue", tagValue
                        ]
        let! token = Async.CancellationToken
        try
            ensureTagHasNoSingleQuoteCharacters tagName logTags
            ensureTagHasNoSingleQuoteCharacters tagValue logTags
            let! images = enumerateAllPagesAsync
                                context.tags
                                (sprintf "Managed images under resource group %s." resourceGroupName)
                                {
                                    getFirstPage = fun () -> context.compute.Images.ListByResourceGroupWithHttpMessagesAsync(resourceGroupName)
                                    getNextPage = fun link -> context.compute.Images.ListByResourceGroupNextWithHttpMessagesAsync(link, null, token)
                                    pageAsList = fun page -> page.Body |> Seq.toList
                                    getLink = fun page -> page.Body.NextPageLink
                                }

            // The API for getting images does not allow for sending filters other than the resource group name, so we have to filter after getting all of the results.
            return images
                |> List.filter (fun image -> not (isNull image.Tags) && image.Tags.ContainsKey(tagName) && image.Tags.[tagName] = tagValue)

        with
        | IsAggregateOf SomeResourceGroupNotFoundException _ ->
            TraceTags.info (sprintf "Resource group %s was not found." resourceGroupName) logTags
            return []
    }

/// Update resource group tags
let updateResourceGroupTags
        (context:Context.InfrastructureContext)
        (tags: (string*string) list) =
    async {
        let logTags = context.tags @
                        tags @
                        [ "resourceGroupName", context.groupName
                        ]

        let! _ = context.resource.ResourceGroups.UpdateAsync(context.groupName, ResourceGroupPatchable(tags = dict tags)) |> Async.AwaitTask
        TraceTags.info "Updated resource group tags" logTags
        return ()
    }

// Keep the module for the type definition, otherwise ResourceGroupNotFound definition from above is masked
module ResourceGroupTagResult =
    type 'a ResourceGroupTag =
        | Tag of 'a
        | TagNotFound
        | ResourceGroupNotFound

/// Return tag value set for the specified tag name on the specified resource group, or None if the tag is not set
let tryGetResourceGroupTagValue
        (context:Context.InfrastructureContext)
        (tagName: string)
        (parse: string -> 'a)
        =
    async {
        try
            let! rg = context.resource.ResourceGroups.GetAsync(context.groupName) |> Async.AwaitTask
            match rg.Tags.TryGetValue tagName with
            | false, _ -> return ResourceGroupTagResult.TagNotFound
            | true, v -> return ResourceGroupTagResult.Tag (parse v)
        with
        | IsAggregateOf SomeResourceGroupNotFoundException _ ->
            let logTags = context.tags @
                            [
                                "tagName", tagName
                                "resourceGroupName", context.groupName
                            ]

            TraceTags.info "No resource found for specified criteria" logTags
            return ResourceGroupTagResult.ResourceGroupNotFound
    }
