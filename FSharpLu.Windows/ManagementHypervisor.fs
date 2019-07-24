/// Abstraction layer on top of the HyperV
/// WMI interface to managed VHD disk (mounting) 
/// and virtual machines
module Microsoft.FSharpLu.Management.Hypervisor

open System.Management
open Microsoft.FSharpLu.Parsing
open Microsoft.FSharpLu.Logger
open Microsoft.FSharpLu.Disposable
open Wmi

exception MsvmError of uint16 * string * Map<string,string> list

/// Job errror processing for Msvm_Error.
/// Adapted from the C# version at http://msdn.microsoft.com/en-us/library/hh850032(v=vs.85).aspx
let msvmErrorHandler (log:Logger<_,_>) (job:ManagementObject) errorCode errorDescription =
    use outParams = Wmi.invoke_internal job "GetErrorEx" []
    if returnValue outParams <> ReturnCode.Success then
        raise (new ManagementException("GetErrorEx() call on the job failed"))

    let errorList = getValue<string[]> "Errors" outParams
    if errorList = null then
        log.write "No error found."

    let icompare x y =  System.String.Compare(x, y, true) = 0

    log.write "Detailed WMI errors:"
    let subErrors =
        errorList
        |> Seq.map (fun error ->
            log.verbose "  XML: %s" error
            let reader = System.Xml.XmlReader.Create(new System.IO.StringReader(error))
            let tagIs name = icompare reader.Name name
            let mutable propertyId = None
            let mutable errorDetails = Map.empty
            while reader.Read() do
                if tagIs "PROPERTY" then
                    propertyId <- None
                    if reader.HasAttributes then
                        let firstAttribute = reader.GetAttribute 0
                        propertyId <- Some firstAttribute
                elif tagIs "VALUE" && propertyId.IsSome then
                    let value = reader.ReadElementContentAsString()
                    errorDetails <- Map.add propertyId.Value value errorDetails
                    propertyId <- None
                else
                    propertyId <- None
            done
            Map.iter (fun e v -> log.write "  %s = %s" e v) errorDetails
            log.write "-- END OF ERROR"
            errorDetails)
        |> Seq.toList

    raise <| (MsvmError (errorCode, errorDescription, subErrors))
    false

let VirtualizationScope computerName =
    if System.String.IsNullOrEmpty computerName then
        "root\virtualization\v2"
    else
        sprintf @"\\%s\root\virtualization\v2" computerName

/// HyperV specific message IDs
type MessageID =
    // Could not get the message id
    | Unknown = -1
    // "Not enough memory in the system to start the virtual machine"
    | OutOfMemory = 3122
    // 'RDD1681-2' could not initialize. (Virtual machine ID 0F9C697C-AB3B-4A77-BCF4-48AD575CB278)
    | CouldNotInitialize = 3040
    // 'RDD1672-2' could not initialize memory: There is not enough space on the disk. (0x80070070).
    | OutOfDiskSpace = 3050
    // 'RDD1672-2' could not create or access saved state file J:\Hyper-V\Virtual Machines\3248FF8A-9254-4EC4-B819-40C88355994B\3248FF8A-9254-4EC4-B819-40C88355994B.vsv
    | CouldNotSaveStateFile = 3080
    // The Virtual Machine 'RDD1672-2' failed to start because there is not enough disk space. The system was unable to create the memory contents file on 'J:\Hyper-V\Virtual Machines\3248FF8A-9254-4EC4-B819-40C88355994B\3248FF8A-9254-4EC4-B819-40C88355994B.bin' with the size of 2049 MB. Set the path to a disk with more storage space or delete unnecessary files from the disk and try again. (Virtual machine ID 3248FF8A-9254-4EC4-B819-40C88355994B)
    | CouldNotCreateMemoryContent = 3326
    // 'RDD1672-2' failed to start. (Virtual machine ID 3248FF8A-9254-4EC4-B819-40C88355994B)
    | CannotStartVM = 12030

/// Get the error type from a MsvmErrorType
let getMsvmErrorType =
    Map.tryFind "MessageID" >> Option.bind tryParseEnum<MessageID>

let isOutOfDiskSpaceError = function
    | MsvmError(code, description, details) ->
        description.Contains("0x80070070")
        || Seq.choose getMsvmErrorType details |> Seq.exists (fun e -> e = MessageID.OutOfDiskSpace || e = MessageID.CouldNotCreateMemoryContent || e = MessageID.CouldNotSaveStateFile )
    | _ -> false

let isOutOfMemoryerror = function
    | MsvmError(code, description, details) ->
        description.Contains("0x800705AA")
        || Seq.choose getMsvmErrorType details |> Seq.exists ((=) MessageID.OutOfMemory)
    | _ -> false

module Vhd =

    //// WMI constants
    module Constants =
        let MountedStorageImage = "Msvm_MountedStorageImage"
        let ImageManagementService = "Msvm_ImageManagementService"
        /// Initial retry period on WMI request in ms
        let RetryPeriod = 1000
        /// Get next retry period: double wait each time but limit to 32s
        let NextRetryPeriod current = max (2*current) (32*1000)

        module MethodName =
            let DetachVirtualHardDisk = "DetachVirtualHardDisk"
            let AttachVirtualHardDisk = "AttachVirtualHardDisk"

    open Constants

    /// Returns the WMI scope used to access the virtualization API
    let getScope hostname =
        new ManagementScope(VirtualizationScope hostname, defaultConnectionOptions())

    /// Gets the image management service using the specified WMI scope.
    let private getImageManagementService (scope:ManagementScope) : ManagementObject =
        getFirstInstance scope ImageManagementService

    /// Get Win32 disk drive object for the specified mounted virtual hard disk.
    let private getDriveFromFilePath (log:Logger<_,_>) computerName mountedFilePath =
        let scope = getScope computerName
        match tryQuerySingle scope (sprintf "SELECT * FROM %O WHERE Name='%s'" MountedStorageImage (escapePath mountedFilePath)) with
        | None -> log.write "Could not find mounted storage image for file %s" mountedFilePath; None
        | Some mountedStorageImage  ->
            log.write "PathID: %O, Lun: %O, PortNumber: %O, TargetId : %O"
                        (mountedStorageImage.GetPropertyValue("PathId"))
                        (mountedStorageImage.GetPropertyValue("Lun"))
                        (mountedStorageImage.GetPropertyValue("PortNumber"))
                        (mountedStorageImage.GetPropertyValue("TargetId"))
            use mountedStorageImage = mountedStorageImage
            let drive =
                Win32.tryFindDrive computerName
                                    (mountedStorageImage.GetPropertyValue("PathId"))
                                    (mountedStorageImage.GetPropertyValue("Lun"))
                                    (mountedStorageImage.GetPropertyValue("PortNumber"))
                                    (mountedStorageImage.GetPropertyValue("TargetId"))
            drive

    /// Detaches a VHD/VHDX that is currently mounted.
    let public detachVirtualHardDisk (log:Logger<_,_>) serverName virtualHardDiskPath =
        let scope = getScope serverName
        match tryQuerySingle scope (sprintf "SELECT * FROM %s WHERE Name='%s'" MountedStorageImage (escapePath virtualHardDiskPath)) with
        | None -> log.failWith "Failed to retrieve mounted virtual hard disk %s" virtualHardDiskPath
        | Some img ->
            use img = img
            use result = invokeSync log scope img msvmErrorHandler MethodName.DetachVirtualHardDisk []
            ()

    /// Attaches a VHD/VHDX/ISO to the system and optionally assign a driver letter.
    /// ReadOnly indicates if the mounted volumes are to be read-only. (Must be
    /// true when mounting ISO files.)
    let public attachVirtualHardDisk (log:Logger<_,_>) serverName virtualHardDiskPath (assignDriveLetters:bool) (readOnly:bool) =
        let scope = getScope serverName
        use imageManagementService = getImageManagementService scope
        let rec attemptMounting waitPeriod =
            async {
                try
                    log.write "Attaching VHD file %s to the system" virtualHardDiskPath
                    use result = invokeSync log scope imageManagementService msvmErrorHandler MethodName.AttachVirtualHardDisk
                                    [ "Path", virtualHardDiskPath :> obj
                                      "AssignDriveLetter", assignDriveLetters :> obj
                                      "ReadOnly", readOnly :> obj]
                    match getDriveFromFilePath log serverName virtualHardDiskPath with
                    | None ->
                        return log.failWith "Failed to retrieve the physical drive for attached VHD file %s" virtualHardDiskPath
                    | Some drive ->
                        log.write "Getting drive letter for mounted drive %O" drive
                        match Win32.getAllDriveLetters log serverName drive with
                        | [] ->
                            log.write "WARNING: Drive has no partition with a logical disk drive: %O" drive
                            return raise (Win32.NoPartitionFound <| drive.ToString())
                        | driveRoot::_ ->
                            log.write "VHD was mounted as drive %s" driveRoot
                            return driveRoot
                with
                Win32.NoPartitionFound drive ->
                    /// See http://blogs.technet.com/b/markrussinovich/archive/2011/11/08/3463572.aspx for more
                    /// details about disk signature collision
                    log.write "WARNING: Drive not properly mounted: probably a disk signature collision (copy of the VHD already mounted). Drive: %O. Retrying in %ds..." drive (waitPeriod/1000)
                    detachVirtualHardDisk log serverName virtualHardDiskPath
                    do! Async.Sleep waitPeriod
                    return! attemptMounting (NextRetryPeriod waitPeriod)
            }
        attemptMounting RetryPeriod |> Async.RunSynchronously


/// HyperV Machine virtualization WMI abstraction layer
module VM =

    /// VM states
    type VmState =
    | Other = 1
    | Running = 2
    | Off = 3
    | Stopping = 4
    | Saved = 6
    | Paused = 9
    | Starting = 10
    | Reset = 11
    | Saving = 32773
    | Pausing = 32776
    | Resuming = 32777
    | FastSaved = 32779
    | FastSaving = 32780

    /// WMI class names
    module Constants =
        let MsvmComputerSystem = "Msvm_ComputerSystem"
        let MsvmVirtualSystemManagementService = "Msvm_VirtualSystemManagementService"
        let MsvmExternalEthernetPort = "Msvm_ExternalEthernetPort"
        let MsvmWiFiPort = "Msvm_WiFiPort"
        let MsvmVirtualEthernetSwitch = "Msvm_VirtualEthernetSwitch"
        let MsvmVirtualEthernetSwitchManagementService = "Msvm_VirtualEthernetSwitchManagementService"
        let MsvmAllocationCapabilities = "Msvm_AllocationCapabilities"
        let MsvmSettingsDefineCapabilities = "Msvm_SettingsDefineCapabilities"
        let MsvmElementCapabilities = "Msvm_ElementCapabilities"
        let MsvmVirtualSystemSettingData = "Msvm_VirtualSystemSettingData"
        let MsvmSettingsDefineState = "Msvm_SettingsDefineState"
        let MsvmResourcePool = "Msvm_ResourcePool"
        let MsvmSyntheticEthernetPortSettingData = "Msvm_SyntheticEthernetPortSettingData"
        let MsvmMemorySettingData  = "Msvm_MemorySettingData"
        let MsvmProcessorSettingData = "Msvm_ProcessorSettingData"
        let MsvmVirtualEthernetSwitchSettingData = "Msvm_VirtualEthernetSwitchSettingData"
        let MsvmResourceAllocationSettingData = "Msvm_ResourceAllocationSettingData"
        let MsvmStorageAllocationSettingData = "Msvm_StorageAllocationSettingData"
        let MsvmSerialController = "Msvm_SerialController"
        let MsvmSerialPort = "Msvm_SerialPort"
        let MsvmVirtualSystemSettingDataComponent = "Msvm_VirtualSystemSettingDataComponent"
        let Msvm_VirtualSystemSnapshotService = "Msvm_VirtualSystemSnapshotService"
        let CimResourceAllocationSettingData = "Cim_ResourceAllocationSettingData"

        /// Resources
        module Resource =
            let EthernetPortSynthetic = "Microsoft:Hyper-V:Synthetic Ethernet Port"
            let DiskDriveSynthetic = "Microsoft:Hyper-V:Synthetic Disk Drive"
            let VirtualDiskSynthetic = "Microsoft:Hyper-V:Virtual Hard Disk"
            let IdeControllerResourceName = "Microsoft:Hyper-V:Emulated IDE Controller"
            let EthernetConnection = "Microsoft:Hyper-V:Ethernet Connection"
            let SerialController = "Microsoft:Hyper-V:Serial Controller"
            let SerialPort = "Microsoft:Hyper-V:Serial Port"

        // Resource subtypes
        module ResourceSubtype =
            let SerialController = "Microsoft Serial Controller"
            let SerialPort = "Microsoft Serial Port"

        /// Type of VM resources
        /// See http://msdn.microsoft.com/en-us/library/cc136903(v=vs.85).aspx for the full list of possible values
        type ResourceType =
        | Other = 1us
        | ComputerSystem = 2us
        | Processor = 3us
        | Memory = 4us
        | IDEController = 5us
        | SerialPort = 17us
        // ...

        module MethodName =
            let ModifyResourceSettings = "ModifyResourceSettings"
            let DefineSystem = "DefineSystem"
            let DestroySystem = "DestroySystem"
            let RequestStateChange = "RequestStateChange"
            let AddResourceSettings = "AddResourceSettings"
            let CreateSnapshot = "CreateSnapshot"

    open Constants

    let invokeSync log scope vm methodname parameters = Wmi.invokeSync log scope vm msvmErrorHandler methodname parameters

    /// Retrieves the first virtual machine with the given machine name.
    let tryGetVirtualMachine scope machineName =
        sprintf "SELECT * FROM %s WHERE ElementName=\"%s\"" MsvmComputerSystem machineName
        |> Wmi.tryQuerySingle scope

    /// Gets the Msvm_ComputerSystem instance that matches the requested virtual machine name.
    let getVirtualMachine scope machineName =
        match tryGetVirtualMachine scope machineName with
        | None -> raise (new ManagementException(sprintf "No virtual machine could be found with name \"%s\"" machineName))
        | Some v -> v

    /// Return the list of all virtual machines
    let getAllVirtualMachines (scope:ManagementScope) =
        sprintf "SELECT * FROM %s WHERE Caption='%s'" MsvmComputerSystem "Virtual Machine"
        |> query scope
        |> Seq.cast<ManagementObject>

    /// Return the name of a VM
    let getVmName (vm:ManagementObject) =
        getValue<string> "ElementName" vm

    /// Gets the virtual system management service for the specified WMI scope.
    let getVirtualMachineManagementService scope =
        getFirstInstance scope MsvmVirtualSystemManagementService

    /// Get the HyperV snapshot service
    let getSnapshotService scope =
        // https://msdn.microsoft.com/en-us/library/hh850259(v=vs.85).aspx
        getFirstInstance scope Msvm_VirtualSystemSnapshotService

    /// Delete a VM
    let deleteVm log scope machineName =
        use managementService = getVirtualMachineManagementService scope
        use vm = getVirtualMachine scope machineName
        use result = invokeSync log scope managementService MethodName.DestroySystem ["AffectedSystem", vm.Path.Path]
        ()

    /// Returns the state of a VM
    let getVmState scope name =
        use state = querySingle scope (sprintf "SELECT EnabledState FROM %s WHERE ElementName=\"%s\"" MsvmComputerSystem name)
        if state = null then
            VmState.Other
        else
            state
            |> getValue<uint16> "EnabledState"
            |> int32
            |> enum<VmState>

    /// Change the state of a VM
    let setVmState log scope desiredState machineName =
        if getVmState scope machineName <> desiredState then
            use vm = getVirtualMachine scope machineName
            use outputParameters = invokeSync log scope vm MethodName.RequestStateChange ["RequestedState", desiredState]
            use job = new ManagementObject(getValue<string> "Job" outputParameters)
            job.Scope <- scope
            match returnValue outputParameters with
            | ReturnCode.Started when isJobComplete job ->
                log.write "%s state was changed successfully." machineName
            | ReturnCode.Started ->
                log.write "Failed to change virtual system state for %s" machineName
            | ReturnCode.Success ->
                log.write "%s state was changed successfully." machineName
            | other ->
                log.write "Failed to change virtual system state of %s with error %d" machineName (other |> uint32)

    /// Start a VM
    let startVm log scope machineName = setVmState log scope VmState.Running machineName

    /// Stop a VM
    let stopVm log scope machineName = setVmState log scope VmState.Off machineName

    /// Return the list of all virtual machines
    let getAllVirtualSwitchesAsList (scope:ManagementScope) =
        getAllClassObjectsAsList scope MsvmVirtualEthernetSwitch

    /// Find the network switch matching a given name
    let tryGetHostNetworkSwitchByName scope switchName =
        sprintf "SELECT * FROM %s WHERE ElementName='%s'" MsvmVirtualEthernetSwitch switchName
        |> tryQuerySingle scope

    /// Return the first network switch
    let tryFirstHostNetworkSwitch scope =
        sprintf "SELECT * FROM %s" MsvmVirtualEthernetSwitch
        |> tryQuerySingle scope

    let getVirtualSystemSettingData =
        firstRelatedRelationshipRole MsvmVirtualSystemSettingData MsvmSettingsDefineState "SettingData" "ManagedElement"

    /// Create a VM with the specified name, CPU cores and RAM.
    let createVm (log:Logger<_,_>) scope machineName coreCount memoryInMegabytes =
        use managementService = getVirtualMachineManagementService scope
        use newVm =
            use cls = new ManagementClass(scope, ManagementPath(MsvmVirtualSystemSettingData), ObjectGetOptions())
            use ss = cls.CreateInstance()
            ss.["ElementName"] <- machineName
            use outParams =
                invokeSync log scope managementService Constants.MethodName.DefineSystem
                        [ "SystemSettings", ss.GetText(TextFormat.WmiDtd20) ]
            let vmPath = getValue "ResultingSystem" outParams : string
            log.write "Machine vmpath: %s" vmPath
            new ManagementObject(vmPath)

        do
            use virtualSystemSettingData = getVirtualSystemSettingData newVm
            use memSettingData = firstRelated MsvmMemorySettingData virtualSystemSettingData
            memSettingData.["VirtualQuantity"] <- memoryInMegabytes
            use procSettingData = firstRelated MsvmProcessorSettingData virtualSystemSettingData
            procSettingData.["VirtualQuantity"] <- coreCount
            use outParams =
                invokeSync log scope managementService MethodName.ModifyResourceSettings
                        [ "ResourceSettings", [| memSettingData.GetText(TextFormat.WmiDtd20)
                                                 procSettingData.GetText(TextFormat.WmiDtd20) |] :> obj ]
            ()
        newVm

    /// Working on this to take snapshots in Physical - Desktop.
    /// Testing it in HyperV.
    let public createVMSnapshot (log:Logger<_,_>) scope machineName =
        use snapshotService = getSnapshotService scope
        use vm = getVirtualMachine scope machineName

        log.write "Creating VM Snapshot."

        log.write "Setting source system to vm path."
        let vmPath = (vm.Path.Path :> obj)

        log.write "About to Invoke Method"
        use outParams =
            invokeSync log scope snapshotService Constants.MethodName.CreateSnapshot
                    [ "AffectedSystem", vmPath
                      "SnapshotSettings", ("" :>obj)
                      "SnapshotType", (2:>obj)]

        /// TODO: extract and return snapshot name
        //outParams.["ResultingSnapshot"]
        log.write "Snapshot was created successfully."
        ()

    /// Get the virtual ethernet switch management service
    let getVirtualEthernetSwitchManagementService scope =
        getFirstInstance scope MsvmVirtualEthernetSwitchManagementService

    /// Get default setting data template for the specified synthetic type
    let inline getDefaultSettingDataTemplate scope syntheticType =
        // Retrieve the default (primordial) resource pool for Synthetic Disk Drive’s
        use resourcePool = querySingle scope (sprintf "SELECT * FROM %s WHERE ResourceSubType = '%s' AND Primordial = True" MsvmResourcePool syntheticType)

        // Retrieve the AllocationCapabilities class for the Resource Pool
        use allocationCapabilities = firstRelatedRelationship MsvmAllocationCapabilities MsvmElementCapabilities resourcePool

        // Query the relationships on the AllocationCapabilities class and find the default class (ValueRole = 0)
        use settingsDefineCapabilities = findRelationship MsvmSettingsDefineCapabilities (fun c -> getValue "ValueRole" c = 0us) allocationCapabilities

        // The PartComponent is the Default Msvm_***AllocationSettingData class value
        let allocationSettingDataTemplatePath : string = getValue "PartComponent" settingsDefineCapabilities
        let allocationSettingDataTemplate = new ManagementObject(allocationSettingDataTemplatePath)
        allocationSettingDataTemplate

    /// Clone default setting data template for the specified synthetic type
    let cloneDefaultSettingDataTemplate scope syntheticType =
        use allocationSettingDataTemplate = getDefaultSettingDataTemplate scope syntheticType
        allocationSettingDataTemplate.Clone() :?> ManagementObject

    /// Connect a VM adapter to a host network switch
    let connectAdapterToSwitch log scope vmInstance virtualEthernetSwitch =
        use managementService = getVirtualMachineManagementService scope
        use virtualSystemSettingData = getVirtualSystemSettingData vmInstance

        // Retrieve the VirtualSwitch class the NIC will Connect to
        //use virtualEthernetSwitch = querySingle scope (sprintf "select * from %s where ElementName='%s'" MsvmVirtualEthernetSwitch switchName)

        // Retrieve the NetworkAdapterPortSettings Associated to the VM.
        use syntheticEthernetPortSettingData = findRelated MsvmSyntheticEthernetPortSettingData (fun e -> getValue "ElementName" e = "Network Adapter") virtualSystemSettingData

        // Specify the NIC's Port Setting and the Switch Path
        use ethernetPortAllocationSettingData = getDefaultSettingDataTemplate scope Resource.EthernetConnection
        ethernetPortAllocationSettingData.["Parent"] <- syntheticEthernetPortSettingData
        ethernetPortAllocationSettingData.["HostResource"] <- [| virtualEthernetSwitch |]

        // Add the connection object which connects the NIC
        use result =
            invokeSync log scope managementService MethodName.AddResourceSettings
                [ "AffectedConfiguration", virtualSystemSettingData.Path.Path :> obj
                  "ResourceSettings", [| ethernetPortAllocationSettingData.GetText(TextFormat.WmiDtd20) |] :> obj ]
        ()

    /// Create an external VMNetwork (switch)
    let createExternalSwitch log scope switchName (msvmExternalNic:ManagementObject) =
        use mgmt = getVirtualEthernetSwitchManagementService scope
        let switchSettingClass = new ManagementClass (scope, ManagementPath(MsvmVirtualEthernetSwitchSettingData), ObjectGetOptions())
        use switchSetting = switchSettingClass.CreateInstance()
        switchSetting.["ElementName"] <- switchName
        switchSetting.["Notes"] <- [| "" |]
        use outParams = invokeSync log scope mgmt MethodName.DefineSystem [ "SystemSettings", switchSetting.GetText(TextFormat.WmiDtd20) ]
        let vmSwitchPath = getValue<string> "ResultingSystem" outParams
        assert (vmSwitchPath <> null)
        let vmSwitch = new ManagementObject(vmSwitchPath)
        assert (vmSwitch <> null)

        log.write "Switch configuration through WMI is not currently working. Please go to HyperV settings and manually set switch '%s' as External." switchName
        invalidOp "HyperV network switch needs to be configured as External before continuing."

        try
            // Constructs a External Port Allocation setting data.
            let switchPortName = sprintf "%s_External" switchName
            let externalSwitchPort = getDefaultSettingDataTemplate scope Resource.EthernetPortSynthetic
            externalSwitchPort.["ElementName"] <- switchPortName
            externalSwitchPort.["HostResource"] <- [| msvmExternalNic.Path.Path |]

            // Use the switch name as the internal switch port name.
            use hostComputer = querySingle scope (sprintf "SELECT * FROM %s Where Name = __SERVER" MsvmComputerSystem)

            let internalSwitchPort = getDefaultSettingDataTemplate scope Resource.EthernetPortSynthetic
            internalSwitchPort.["ElementName"] <- switchPortName
            internalSwitchPort.["HostResource"] <- [| hostComputer.Path |]
            internalSwitchPort.["Address"] <- msvmExternalNic.GetPropertyValue("PermanentAddress") :?> string

            /// Add an external connection and an internal connection at the same time,
            /// with the MAC address mapped from internal to external.
            use result = invokeSync log scope mgmt MethodName.AddResourceSettings
                            [ "AffectedConfiguration" , switchSetting.Path.Path :> obj
                              "ResourceSettings" , [| externalSwitchPort.GetText(TextFormat.CimDtd20)
                                                      internalSwitchPort.GetText(TextFormat.CimDtd20) |] :> obj ]
            vmSwitch
        with
            _  as e ->
            use result = invokeSync log scope vmSwitch MethodName.DestroySystem []
            reraise()

    /// Returns the first external network NIC found
    let tryFirstExternalEthernetPort scope =
        let externalPortClasses = [MsvmExternalEthernetPort; MsvmWiFiPort]
        let tryFindPort portClass = tryQuerySingle scope (sprintf "Select * From %s" portClass)
        Seq.tryPick tryFindPort externalPortClasses

    /// Returns the path to the VHD of the first mounted drive
    let tryGetFirstVhdFilePath vmInstance =
        use virtualSystemSettingData = getVirtualSystemSettingData vmInstance
        match tryFirstRelated MsvmStorageAllocationSettingData virtualSystemSettingData with
        | None -> None
        | Some v ->
            use virtualHardDisk = v
            Array.get (getValue<string[]> "HostResource" virtualHardDisk)  0 |> Some

    /// Attache a VHD disk to the virtual machine
    let attachVhdDisk log scope (vm:ManagementObject) vhdFilePath =
        use managementService = getVirtualMachineManagementService scope

        use virtualSystemSettingData = getVirtualSystemSettingData vm

        // Find the IDE Controller To Connect The Disk To
        use ideController = findRelated MsvmResourceAllocationSettingData
                                (fun c ->   getValue "ResourceType" c = uint16 ResourceType.IDEController
                                            && getValue "ResourceSubType" c = Resource.IdeControllerResourceName
                                            && getValue "Address" c = "0")
                                virtualSystemSettingData

        // Specify the disk drives connection point and address (location on the controller)
        use storageAllocationSettingData = cloneDefaultSettingDataTemplate scope Resource.DiskDriveSynthetic
        storageAllocationSettingData.["Parent"] <- ideController
        storageAllocationSettingData.["AddressOnParent"] <- 0

        // Add the drive to the VM and save the resulting disk drive path
        use diskDriveResource = invokeSync log scope managementService
                                    Constants.MethodName.AddResourceSettings
                                    [ "AffectedConfiguration", virtualSystemSettingData :> obj
                                      "ResourceSettings", [| storageAllocationSettingData.GetText(TextFormat.WmiDtd20) |] :> obj]

        let diskDriveResource = (getValue<string[]> "ResultingResourceSettings" diskDriveResource).[0]

        // Specify the VHD's Disk Drive and Path (location on the controller)
        use storageAllocationSettingData = cloneDefaultSettingDataTemplate scope Resource.VirtualDiskSynthetic
        storageAllocationSettingData.["Parent"] <- diskDriveResource
        storageAllocationSettingData.["HostResource"] <- [| vhdFilePath |]

        // Add the VHD to the disk drive
        use result = invokeSync log scope managementService
                        Constants.MethodName.AddResourceSettings
                        [ "AffectedConfiguration", virtualSystemSettingData :> obj
                          "ResourceSettings", [| storageAllocationSettingData.GetText(TextFormat.WmiDtd20) |] :> obj]
        ()

    /// Add network adapter to the VM
    let addNetworkAdapter log scope (vm:ManagementObject) adapterName =
        use managementService = getVirtualMachineManagementService scope

        /// Get the VM setting data
        use virtualSystemSettingData = getVirtualSystemSettingData vm

        // Specify a unique identifier, a friendly name and specify dynamic mac addresses
        use syntheticEthernetPortSettingData = getDefaultSettingDataTemplate scope Resource.EthernetPortSynthetic
        syntheticEthernetPortSettingData.["VirtualSystemIdentifiers"] <- [| System.Guid.NewGuid().ToString("B") |]
        syntheticEthernetPortSettingData.["ElementName"] <- "Network Adapter"
        syntheticEthernetPortSettingData.["StaticMacAddress"] <- false

        // Add the network adapter to the VM
        use result = invokeSync log scope managementService
                            Constants.MethodName.AddResourceSettings
                            [ "AffectedConfiguration", virtualSystemSettingData :> obj
                              "ResourceSettings", [| syntheticEthernetPortSettingData.GetText(TextFormat.WmiDtd20) |] :> obj]
        ()

    /// Create a pipe on the host redirecting to the specified guest VM port
    let setupHostPipeToGuestComPort log scope vm pipeName portNumber =
        let portName = sprintf "COM %d" portNumber
        use managementService = getVirtualMachineManagementService scope
        use virtualSystemSettingData = getVirtualSystemSettingData vm
        use serialPort =
            getRelatedRelationships CimResourceAllocationSettingData MsvmVirtualSystemSettingDataComponent null null virtualSystemSettingData
            |> Seq.findDispose (fun x -> getValue "ResourceSubType" x = Constants.Resource.SerialPort
                                            && getValue "ElementName" x = portName)

        serialPort.["Connection"] <- [| pipeName |]
        use outParams = invokeSync log scope managementService
                            MethodName.ModifyResourceSettings
                            [ "ResourceSettings", [| serialPort.GetText(TextFormat.CimDtd20) |] :> obj ]
        ()