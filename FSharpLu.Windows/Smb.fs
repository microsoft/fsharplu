/// User creation and SMB network sharing
module Microsoft.FSharpLu.Windows.Smb

open Microsoft.FSharpLu.Logging
open System.Management
open System.Security.Principal
open System.DirectoryServices

/// Result codes returned by WMI APIs.
/// See https://msdn.microsoft.com/en-us/library/aa393598(v=vs.85).aspx
module ResultCode =
    let [<Literal>] Success = 0u
    let [<Literal>] AlreadyExists = 22u

/// See https://msdn.microsoft.com/en-us/library/aa394063(v=vs.85).aspx
type AccessMasks =
| FullControl = 2032127
| Change = 1245631
| Read = 1179817

/// See https://msdn.microsoft.com/en-us/library/aa394063(v=vs.85).aspx
type AceType =
| Allow = 0
| Deny = 1

/// See https://msdn.microsoft.com/en-us/library/aa394063(v=vs.85).aspx
type AceFlags =
| ObjectInherit = 0x1
| ContainerInherit = 0x2

/// See https://msdn.microsoft.com/en-us/library/windows/desktop/aa379566(v=vs.85).aspx
type SeDacl =
    Present = 0x4

/// Create a new SMB share named `sharename` mapping to local directory `directory`
let createSmbShare sharename directory =
    if not <| System.IO.Directory.Exists directory then
        System.IO.Directory.CreateDirectory directory |> ignore

    let path = new ManagementPath("Win32_Share")
    path.Server <- "." // localhost
    let parameters = [|
            directory   :> obj  // Path to shared folder
            sharename   :> obj  // Share name
            0x0         :> obj  // Share type (disk drive)
            0           :> obj  // Maximum Allowed concurrent users
            null        :> obj  // Password (optional)
            null        :> obj  // Security Access (optional)
        |]

    use share = new ManagementClass(path)
    let result = share.InvokeMethod("Create", parameters) :?> uint32

    match result with
    | ResultCode.Success
    | ResultCode.AlreadyExists ->
        Trace.info "Share '%s' successfully created" sharename
    | returnValue ->
        Trace.failwith "Unable to share directory. Error Code: %u" returnValue

/// Grant a local user access to a network share
let grantAccess shareName userName =
    /// See https://msdn.microsoft.com/en-us/library/aa393598(v=vs.85).aspx
    /// for WMI API documentation

    // Create the security descriptor
    let ntAccount = System.Security.Principal.NTAccount(userName)

    // SID
    let userSID = ntAccount.Translate(typeof<SecurityIdentifier>) :?> SecurityIdentifier
    let utenteSIDArray = Array.zeroCreate userSID.BinaryLength : byte[]
    userSID.GetBinaryForm(utenteSIDArray, 0)

    // Trustee
    use trustee = new ManagementClass(ManagementPath("Win32_Trustee"), null)
    trustee.["Name"] <- userName
    trustee.["SID"] <- utenteSIDArray

    // ACE
    use ace = new ManagementClass(ManagementPath("Win32_Ace"), null)
    ace.["AccessMask"] <- AccessMasks.FullControl
    ace.["AceFlags"] <- AceFlags.ObjectInherit ||| AceFlags.ContainerInherit
    ace.["AceType"] <- AceType.Allow
    ace.["Trustee"] <- trustee

    use securityDescriptor = new ManagementClass(ManagementPath("Win32_SecurityDescriptor"), null)
    securityDescriptor.["ControlFlags"] <- SeDacl.Present
    securityDescriptor.["DACL"] <- [| ace |]

    let path = new ManagementPath("Win32_Share.Name='" + shareName + "'")
    use share = new ManagementObject(path)
    let r = share.InvokeMethod("SetShareInfo", [| System.Int32.MaxValue :> obj; null :> obj; securityDescriptor |]) :?> uint32
    if r <> ResultCode.Success then
        Trace.failwith "Unable to grant user access to file share. Error Code: %u" r

/// Windows local user management
module User =
    open System.DirectoryServices

    /// Get the list of local users on the machine
    let getAllFromUsersGroup () =
        seq {
            use machine = new DirectoryEntry("WinNT://" + System.Environment.MachineName)
            let group = machine.Children.Find("users","group")
            let groupMembers = group.Invoke("members", null) :?> System.Collections.IEnumerable |> Seq.cast<obj>
            for groupMember in groupMembers do
                use entry = new DirectoryEntry(groupMember)
                yield entry.Name
        }

/// Flags used for user creation
module Flags =
    [<Literal>]
    let PasswordNeverExpires = 0x10000

module ErrorCodes =
    [<Literal>]
    let AccountAlreadyExists = 0x800708B0

/// Create a new local user on the machine
let createLocalUser username (fullName:string) (password:string) throwIfAlreadyExists =
    try
        use hostMachineDirectory = new DirectoryEntry("WinNT://" + System.Environment.MachineName)
        use userEntry = hostMachineDirectory.Children.Add(username, "User")
        userEntry.Properties.["FullName"].Add(fullName) |> ignore
        userEntry.Invoke("SetPassword", password) |> ignore
        userEntry.Invoke("Put", [|"UserFlags" :>obj; Flags.PasswordNeverExpires:>obj |]) |> ignore
        userEntry.CommitChanges()
        true
    with
    | :? System.Runtime.InteropServices.COMException as c
        when c.ErrorCode = ErrorCodes.AccountAlreadyExists ->
            if throwIfAlreadyExists then
                Trace.failwith "Local user account already exists: %s" username
            else
                Trace.warning "Local user account already exists: %s" username
                false

    | :? System.UnauthorizedAccessException ->
            Trace.failwith "Access denied while attempting to create a local user. Please run again in elevated mode."


module Test =
    let test () =
        let created = createLocalUser "testUser" "User full name" "*******" false
        createSmbShare "testShare" @"C:\TestDir"
        grantAccess "testShare" "testUser"