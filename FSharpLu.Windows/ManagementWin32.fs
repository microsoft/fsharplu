/// Helper function to consume some key components of the 
/// the Win32 WMI interface (disk, memory)
module Microsoft.FSharpLu.Management.Win32

open System.Management
open Microsoft.FSharpLu.Logger
open Microsoft.FSharpLu.Disposable
open Wmi


let Win32Scope computerName = sprintf @"\\%s\root\cimv2" computerName
let Win32DiskDrive = "Win32_DiskDrive"
let Win32LogicalDisk = "Win32_LogicalDisk"
let Win32OperatingSystem = "Win32_OperatingSystem"
let Win32DiskPartition = "Win32_DiskPartition"

/// No partition found for the specified drive
exception NoPartitionFound of string

/// Returns true if the given hostName  corresponds to the local machine
let public isLocalMachine (hostname:string) =
    hostname = "."
    || System.String.Compare(hostname, "localhost", true) = 0
    || System.String.Compare(hostname, System.Net.Dns.GetHostName(), true) = 0
    || System.String.Compare(hostname, System.Net.Dns.GetHostEntry("LocalHost").HostName, true) = 0

/// Return amount of free disk space on the specified machine
let public getFreeDiskSpace hostname drive : uint64 =
    let scope = new ManagementScope(Win32Scope hostname, defaultConnectionOptions())
    use freespace = querySingle scope (sprintf "SELECT FreeSpace FROM %s WHERE Caption='%s'" Win32LogicalDisk drive)
    if freespace = null then
        raise (System.IO.DriveNotFoundException(sprintf "Drive %s could not be found" drive))
    getValue "FreeSpace" freespace

/// Return amount of free physical memory on the specified machine
let public getFreePhysicalMemory hostname : uint64 =
    let scope = new ManagementScope(Win32Scope hostname, defaultConnectionOptions())
    use freemem = querySingle scope (sprintf "SELECT FreePhysicalMemory FROM %s" Win32OperatingSystem)
    getValue "FreePhysicalMemory" freemem

/// Return the list of all drive letters associated with partitions of the given drive
let public getAllDriveLetters (log:Logger<_,_>) hostname drive =
    use partitions = getRelated Win32DiskPartition drive
    if Seq.isEmpty partitions then
        raise <| NoPartitionFound (drive.ToString())

    let drives =
        partitions
        |> Seq.cast<ManagementObject>
        |> Seq.mapDispose (fun partition ->
                            use logicalDisks = getRelated<ManagementBaseObject> Win32LogicalDisk partition
                            log.write "logical disks: %d (drive: %O)" (Seq.length logicalDisks) drive
                            logicalDisks
                            |> Seq.mapDispose (getValue<string> "Name")
                            |> Seq.toList
                            )
        |> Seq.concat
        |> Seq.toList
    drives

/// Return the list of all drive letters associated with partitions of the given deviceId
let public getDriveLettersByDeviceId (log:Logger<_,_>) hostname deviceId =
    let scope = new ManagementScope(Win32Scope hostname, defaultConnectionOptions())
    let query = sprintf "SELECT * FROM %s WHERE DeviceId='%s'" Win32DiskDrive (escapePath deviceId)
    match tryQuerySingle scope query with
    | None -> invalidOp (sprintf "Could not find the requested device %s" deviceId)
    | Some mo ->
        getAllDriveLetters log mo

// Lookup a Win32_DiskDrive instance from disk parameters
let public tryFindDrive computerName pathId lun portNumber targetId =
    let scope = new ManagementScope(Win32Scope computerName, defaultConnectionOptions())
    let command = sprintf "SELECT * FROM %s WHERE SCSIBus = %O and SCSILogicalUnit = %O and SCSIPort = %O and SCSITargetId = %O"
                        Win32DiskDrive pathId lun portNumber targetId
    tryQuerySingle scope command

/// Determine if a .Net exception originates from an OutOfDiskSpace error
let isOutOfDiskSpaceException (exn:System.Exception) =
    let ERROR_HANDLE_DISK_FULL = 0x27
    let ERROR_DISK_FULL = 0x70
    let win32ErrorCode = System.Runtime.InteropServices.Marshal.GetHRForException(exn) &&& 0xFFFF
    win32ErrorCode = ERROR_HANDLE_DISK_FULL || win32ErrorCode = ERROR_DISK_FULL
