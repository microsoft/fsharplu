/// VM provisioning based on local installation of HyperV
module Microsoft.FSharpLu.HyperV

open Microsoft.FSharpLu
open Microsoft.FSharpLu.File
open Microsoft.FSharpLu.Logger
open Microsoft.FSharpLu.Management.Wmi
open Microsoft.FSharpLu.Management.Hypervisor.Vhd
open Microsoft.FSharpLu.Management.Hypervisor.VM
open Microsoft.FSharpLu.Management
open Microsoft.FSharpLu.Disposable

module private Constants =
    /// Period of time to wait in seconds between two checks for available resources
    let WaitForResourcesPeriod = 5 * 1000 // 10 seconds

    /// Name of the system-wide mutex used to prevent simulataneous VHD mounting
    /// causing "disk signature collisions" in Windows.
    let MountingMutexName = "VHDMOUNTING_MUTEX_HYPERV"

    /// Estimated average amount of memory required on the host to create a new VM (expressed in KB)
    let HostMemoryRequiredPerVM = 4096UL * 1024UL // 4 GB

    /// Estimated average amount of disk space on the host to be able to create a new VM (in bytes)
    let HostDiskSpaceRequiredPerVM = 10000000000UL  // ~10 GB
    
open Constants

/// State of an HyperV machine
type MachineState =
| Running
| DoesNotExist
| Stopped

/// Prepare a VHD file to be consumed by a VM.
/// Return the path to the generated VHD file
let private prepareWindowsVhd (log:Logger<_,_>) hostName hostVhdFilePath injectFiles =
    async {
        let mutexContext = System.Threading.SynchronizationContext()
        use mutex = new System.Threading.Mutex(false, MountingMutexName)

        log.write "Acquiring VHD mounting mutex %s" MountingMutexName
        mutex.WaitOne() |> ignore

        try
            try
                let mountedDrive = Hypervisor.Vhd.attachVirtualHardDisk log hostName hostVhdFilePath true false

                let mountedPath = mountedDrive + @"\"
                let sanityCheckPath = mountedPath ++ @"windows\system32\kernel32.dll"
                if not (System.IO.File.Exists sanityCheckPath) then
                    invalidOp (sprintf "Unexpected VHD type: kernel32.dll not present in %s: %s" hostVhdFilePath sanityCheckPath)
                log.write "VHD mounted on %s" mountedDrive

                log.write "Custom file injection..."
                injectFiles mountedPath

                log.write "VHD file mounted on %s was successfully patched" mountedDrive
                return hostVhdFilePath
            with  _ as e ->
                log.write "[WARNING] Exception: %O" e
                return Async.reraise e
        finally
            log.write "Dismounting VHD %s" hostVhdFilePath
            Hypervisor.Vhd.detachVirtualHardDisk log hostName hostVhdFilePath
            //async {
            log.write "Releasing VHD mounting mutex %s" MountingMutexName
            //do! Async.SwitchToContext(mutexContext)
            mutex.ReleaseMutex()
            //} |> Async.RunSynchronously
    }

let scope (log:Logger<_,_>) (hostname:string) =
    log.write "Creating WMI scope for HyperV machine %s" hostname
    getScope hostname

let getMachineState (log:Logger<_,_>) (hostname:string) vmMachineName =
    log.verbose "Querying state of machine %s at %s using WMI" vmMachineName hostname
    let scope = scope log hostname
    match tryGetVirtualMachine scope vmMachineName with
    | None -> MachineState.DoesNotExist
    | Some vm ->
        vm.Dispose()
        match getVmState scope vmMachineName with
        | VmState.Running -> MachineState.Running
        | VmState.Other| VmState.Off | VmState.Stopping
        | VmState.Saved | VmState.Paused
        | VmState.Starting | VmState.Reset
        | VmState.Saving | VmState.Pausing
        | VmState.Resuming | VmState.FastSaved
        | VmState.FastSaving
        | _ -> MachineState.Stopped

/// Wait until a VM is in any of a specified desired states
let waitUntilMachineStateIn (log:Logger<_,_>) hostname (vmName:string) (desiredStates:MachineState list) =
    async {
        let isNotDesired x = List.tryFind ((=) x) desiredStates |> Option.isNone
        log.write "Waiting for %s to be in state %A..." vmName desiredStates
        while getMachineState log hostname vmName |> isNotDesired do
            do! Async.Sleep WaitForResourcesPeriod
        log.write "Machine %s reached one of desired state %A." vmName desiredStates
        return (getMachineState log hostname vmName)
    }

/// Delete a VM and its associated VHD file
let deleteVmAndVhd (log:Logger<_,_>) (hostname:string) vmMachineName =
    async {
        let scope = scope log hostname
        match tryGetVirtualMachine scope vmMachineName with
        | None -> return false
        | Some vm ->
            use vm = vm
            let vhdFilePath = tryGetFirstVhdFilePath vm

            stopVm log scope vmMachineName
            // Race condition here: the cleanup process running in parallel
            // may delete the VM just after it is stopped so to avoid waiting indefinitely
            // we wait on both Stopped and DoesNotExist states
            let! state = waitUntilMachineStateIn log hostname vmMachineName [ MachineState.Stopped; MachineState.DoesNotExist ]

            if state = MachineState.DoesNotExist then
                log.write "The VM %s (%s) has just been deleted from a concurrent process" vmMachineName hostname
            else
                log.write "Deleting VM %s from %s" vmMachineName hostname
                Hypervisor.VM.deleteVm log scope vmMachineName

            /// delete associated VHD file
            match vhdFilePath with
            | Some vhdFilePath when Win32.isLocalMachine hostname ->
                log.write "Removing VHD file '%s'" vhdFilePath
                if System.IO.File.Exists vhdFilePath then
                    System.IO.File.Delete vhdFilePath
            | _ -> ()
            return true
        }

/// Get the list of all machines matching specified filter
let getMachines scope namefilter =
    getAllVirtualMachines scope
    |> Seq.mapDispose getVmName
    |> Seq.filter namefilter
    |> Seq.toList
    |> Seq.ofList

/// Find existing or create new network switch
let getHostNetworkSwitch (log:Logger<_,_>) scope hostNetworkSwitchName =
    match tryGetHostNetworkSwitchByName scope hostNetworkSwitchName with
    | Some switch -> switch
    | None ->
        // Fallback on first available switch if it exists
        match tryFirstHostNetworkSwitch scope with
        | Some switch -> switch
        | None ->
            // Fallback: create a new network switch
            use firstExternalPhysicalAdapter =
                tryFirstExternalEthernetPort scope
                |> Option.orDo (fun () -> invalidOp "Could not find a net card to set the VM Switch with")
            log.write "Installing Hyper-V VM switch named \"%s\" on adapter \"%s\"" hostNetworkSwitchName (getValue "ElementName" firstExternalPhysicalAdapter)
            createExternalSwitch log scope hostNetworkSwitchName firstExternalPhysicalAdapter

/// Delete multiple VMs
let deleteVms log machineNames hostname =
    async {
        let! r =
            machineNames
            |> Seq.map (deleteVmAndVhd log hostname)
            |> Async.Parallel
        return Array.forall id r
    }

/// Log currently available resources (disk and memory)
let logCurrentResources (log:Logger<_,_>) (hostname:string) hostVmDir =
    let drive = System.IO.Directory.GetDirectoryRoot(hostVmDir).Substring(0,2)
    let diskSpace = Win32.getFreeDiskSpace hostname drive
    let freeMem = Win32.getFreePhysicalMemory hostname
    log.write "VM directory: %s" hostVmDir

    log.write "%d available disk space on drive %s at %s" diskSpace drive hostname
    log.write "%d available memory on %s" freeMem hostname

/// Keep running an async task until it completes without any resource related failure
let resilientAsync (log:Logger<_,_>) (hostname:string) hostVmDir waitPeriod description (job:Async<'a>) =
    let rec aux () : Async<'a> =
        async {
            log.write "%s" description
            let! result = job |> Async.Catch
            match result with
            | Choice1Of2(r) -> return r
            | Choice2Of2(exn) ->
                return!
                    match exn with
                    | :? System.OutOfMemoryException ->
                        insufficientResources "memory" waitPeriod description job
                    | :? System.IO.IOException as exn when Win32.isOutOfDiskSpaceException exn  ->
                        insufficientResources "disk space" waitPeriod description job
                    | Hypervisor.MsvmError(_) as exn when Hypervisor.isOutOfDiskSpaceError exn ->
                        insufficientResources "disk space" waitPeriod description job
                    | Hypervisor.MsvmError(_) as exn when Hypervisor.isOutOfMemoryerror exn ->
                        insufficientResources "memory" waitPeriod description job
                    | _ as exn -> raise exn
        }
    and insufficientResources resourceType waitPeriod description job =
        async {
            logCurrentResources log hostname
            log.write "Insufficient %s, retrying in %d seconds..." resourceType (waitPeriod/1000)
            do! Async.Sleep waitPeriod
            return! aux ()
        }
    aux ()


type VmRequirements =
    {
        /// Amount of memory allocated to each VM (in MB)
        vmMemoryInMegabytes : int
        /// Name of the network adapter created for each VM e.g. ExternalNetworkCard
        vmNetAdapterName : string 
        /// A port number for kernel debugging if required
        kernelDebuggerPort : int option
        hostNetworkSwitchName: string
        /// Number of CPU cores
        cores : int
        sourceVhdFileName : string
    }

let hasCapacityForNewVM hostVmDir hostname =
    let drive = System.IO.Directory.GetDirectoryRoot(hostVmDir).Substring(0,2)
    let diskSpace = Win32.getFreeDiskSpace hostname drive
    let freeMem = Win32.getFreePhysicalMemory hostname 
    if diskSpace < HostDiskSpaceRequiredPerVM then
        Some "disk space"
    else if freeMem < HostMemoryRequiredPerVM then
        Some "memory"
    else
        None
  
/// Wait until there is enough resources to create a VM on the HyperV host
/// with the provided parameters
let waitForFactorySystemResources (log:Logger<_,_>) hostVmDir hostname =
    let rec wait waitPeriod =
        async {
            match hasCapacityForNewVM hostVmDir hostname with
            | None -> ()
            | Some resource ->
                log.write "WARNING: Insufficient %s to provision VM, retrying in %d seconds..." resource (waitPeriod/1000)
                logCurrentResources log hostname hostVmDir
                do! Async.Sleep waitPeriod
                return! wait waitPeriod
        }
    wait Constants.WaitForResourcesPeriod

/// fileInjectionCallBack call back tak
let createVM (log:Logger<_,_>) (hostname:string) vmMachineName vmParameters (fileInjectionCallBack:string->unit) =
    async {
        if not (Diagnostics.Elevation.isElevated()) then

            failwith "Creating HyperV VM requires elevated privileges (to mount the VHD on the system and inject files)."

        // Delete the VM if it already exists
        let! existedBefore = deleteVmAndVhd log hostname vmMachineName
        
        let scope = scope log hostname

        // Get the path to the directory where to store the VM files
        let hostVmDir =
            let vhdDir = System.Environment.GetEnvironmentVariable("PROGRAMDATA") ++ @"HyperV\VHDS"
            log.write "WARNING: HyperV VM directory not configured. Falling back on %s" vhdDir
            vhdDir

        log.write "Prepping VM %s with %s" vmMachineName vmParameters.sourceVhdFileName
        let! vhdFilePath =
             prepareWindowsVhd log hostname vmParameters.sourceVhdFileName fileInjectionCallBack
             |> resilientAsync log hostname hostVmDir WaitForResourcesPeriod "Getting VHD"

        // Wait for system resources
        do! waitForFactorySystemResources log hostVmDir hostname

        log.write "Creating VM %s on %s" vmMachineName hostname
        use vmInstance = createVm log scope vmMachineName vmParameters.cores vmParameters.vmMemoryInMegabytes

        match vmParameters.kernelDebuggerPort with
        | None -> ()
        | Some port ->
            let pipeName = sprintf @"\\.\pipe\%s" vmMachineName
            log.verbose "Attaching pipe %s on host machine to kernel debugger COM port %d on VM" pipeName port
            Hypervisor.VM.setupHostPipeToGuestComPort log scope vmInstance pipeName port

        // Attach VHD drive
        attachVhdDisk log scope vmInstance vhdFilePath

        // Create network adapter for the VM
        let adapter = addNetworkAdapter log scope vmInstance vmParameters.vmNetAdapterName
        use switch = getHostNetworkSwitch log scope vmParameters.hostNetworkSwitchName
        connectAdapterToSwitch log scope vmInstance switch

        // Start the VM
        do! async { startVm log scope vmMachineName }
            |> resilientAsync log hostname hostVmDir WaitForResourcesPeriod "Starting VM"

        // Wait until it's ready
        let! state = waitUntilMachineStateIn log hostname vmMachineName [ MachineState.Running ]
        ()
    }
