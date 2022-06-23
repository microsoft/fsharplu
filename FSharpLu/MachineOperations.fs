module Microsoft.FSharpLu.Platform.MachineOperations

open System
open System.IO
open System.Net.NetworkInformation
open Microsoft.FSharpLu.Logging
open Microsoft.FSharpLu.ErrorHandling
open Microsoft.FSharpLu
open Microsoft.FSharpLu.Diagnostics

module Constants =

    /// Timeouts and intervals used when mounting an SMB drive
    module SmbMounting =
        /// Timeout after which we assume that the `mount` command has failed
        let MountProcessTimeout = TimeSpan.FromSeconds 60.0

    /// Timeouts and intervals used when pinging a VM
    module Ping =
        let IntervalsBetweenAttempts = System.TimeSpan.FromMinutes(1.0)
        let Timeout = System.TimeSpan.FromHours(1.0)
        let IndividualPingTimeoutInMilliseconds = 120

    /// Timeouts and constants for network initialization on a VM
    module MachineNameInitialization =
        let IntervalsBetweenAttempts = System.TimeSpan.FromMinutes(1.0)
        let Timeout = System.TimeSpan.FromMinutes(15.0)
        let QueryHostnameTimeoutInMilliseconds = 120

        let [<Literal>]LinuxUninitializedName = "localhost.localdomain"

/// Match on a PingException
let somePingException = someExceptionOfType<System.Net.NetworkInformation.PingException>

/// Ping the specified machine until it responds
let pingUntilResponse (serverName:string) =
    let data = "Are you up and running?"
    let buffer = System.Text.Encoding.ASCII.GetBytes (data)

    let ping () =
        async {
            use pingSender = new Ping ()
            Trace.info "Pinging %s" serverName
            try
                let! reply = pingSender.SendPingAsync (serverName,
                                                       Constants.Ping.IndividualPingTimeoutInMilliseconds,
                                                       buffer,
                                                       PingOptions (DontFragment = true)) |> Async.AwaitTask

                if reply.Status = IPStatus.Success then
                    Trace.info "Address: %O" reply.Address
                    Trace.info "RoundTrip time: %d" reply.RoundtripTime
                    if not (isNull reply.Options) then
                        Trace.info "Time to live: %d" reply.Options.Ttl
                        Trace.info "Don't fragment: %b" reply.Options.DontFragment
                    Trace.info "Buffer size: %d" reply.Buffer.Length
                    return Some true
                else
                    Trace.info "Response: %O" reply.Status
                    return None
            with
            | IsAggregateOf somePingException e ->
                Trace.info "Ping error: %O" e
                return None
        }
    Async.retryUntilSomeOrTimeout Constants.Ping.Timeout Constants.Ping.IntervalsBetweenAttempts ping

/// Run cmdkey with provided arguments in order to make changes in Windows Credential manager
let private startCredentialManager arguments (expectedStringInStdOut:string) =
    async {
        let command = "cmdkey"
        let workingDirectory = System.Environment.CurrentDirectory
        let! result = Process.startProcessAsync command arguments workingDirectory (Process.ProcessStartFlags.RedirectStandardError ||| Process.ProcessStartFlags.RedirectStandardOutput ||| Process.ProcessStartFlags.SensitiveArguments) Process.ProcessTimeoutAction.NoTimeout None

        // Even though cmdkey succeeds we may expect a specific success outcome based on the standard output
        if result.ExitCode = 0 && result.StandardOutput.Contains(expectedStringInStdOut) then
            return Result.Ok()
        else
            return Result.Error (sprintf "Failed to cmdkey with arguments [%s] domain stored credentials with exit code: %d, standard out: %s, standard error: %s" arguments result.ExitCode result.StandardOutput result.StandardError)

    }

module WindowsOnly =
    /// Create domain stored credential. On Windows the credential is stored in Credential Manager.
    let createDomainStoredCredential domain user password =
        startCredentialManager (sprintf "/add:\"%s\" /user:\"%s\" /pass:\"%s\"" domain user password) "CMDKEY: Credential added successfully."

    /// Delete domain stored credential. On Windows the credential is deleted from Credential Manager.
    let deleteDomainStoredCredential domain =
        startCredentialManager (sprintf "/delete:\"%s\"" domain) "CMDKEY: Credential deleted successfully."

    /// List currently stored credentials for the domain
    let listDomainStoredCredential domain =
        startCredentialManager (sprintf "/list:\"%s\"" domain) (sprintf "Currently stored credentials for %s:" domain)

// test for share name of the form "d$/"
// E.g isDriveShareName "d$/" returns true
// while isDriveShareName "sharename/" returns false
let isDriveShareName (s:string) =
    s.Length >= 3
    && System.Char.IsLetter s.[0]
    && s.[1] = '$'
    && s.[2] = '/'

/// Test if a URL refers to localhost
let isLocalHost (u:System.Uri) =
    String.Compare(u.Host, "localhost", true) = 0

/// Test if a UNC path is referring to a local drive
///   isLocalUncPath "\\localhost\c$\foo" returns true
///   isLocalUncPath "\\?\c:\\foo" returns true
///   isLocalUncPath "\\localhost\sharename" returns false
///   isLocalUncPath "\\foo.domain.com\sharename" returns false
let isLocalUncPath (uncPath:System.String) =
    if uncPath.StartsWith(@"\\?\") then
        true
    else
        let uncAsUri = System.Uri(uncPath)
        match uncAsUri.Segments with
        | [| "/"; s ; _|] when isDriveShareName s && isLocalHost uncAsUri ->
            true
        | _ -> false

/// Check if a file share from a given domain is already mounted
let isFileShareMounted (domain: string) =
    async {
        match Platform.Current with
        | Platform.Windows ->
            let command, arguments, workingDirectory =
                "net.exe",
                "use",
                System.Environment.CurrentDirectory
            let! result =
                    Process.startProcessAsync command arguments workingDirectory (Process.ProcessStartFlags.RedirectStandardError ||| Process.ProcessStartFlags.RedirectStandardOutput) Process.ProcessTimeoutAction.NoTimeout None
            if result.ExitCode = 0 && result.StandardOutput.Contains domain then
                return Result.Ok()
            elif result.ExitCode <> 0 then
                return Result.Error(sprintf "Failed to execute net use for domain: %s with error code: %d, standard out: %s, standard error: %s" domain result.ExitCode result.StandardOutput result.StandardError)
            else
                return Result.Error(sprintf "File share for domain: %s is not mounted. Standard out: %s, standard error: %s" domain result.StandardOutput result.StandardError)
        | Platform.Linux ->
            return Result.Ok()

    }

/// Unmount an SMB share that was previously mounted under the specified local drive location
//// (e.g. Y:)
/// Returns `Some true` on success, `Some false` on failure, or
/// `None` if unmounting is not yet implemented for the current platform.
let unmountFileShare localDrive =
    async {
        match Platform.Current with
        | Platform.Windows ->
            let command, arguments, workingDirectory =
                "net.exe",
                (sprintf @"use /DELETE %s" localDrive),
                System.Environment.CurrentDirectory

            Trace.info "Unmounting drive %s %s" command arguments
            let! exitCode =
                Process.startProcessAsyncAndWait command arguments workingDirectory Process.ProcessStartFlags.None
            return
                if exitCode = 0 then
                    Result.Ok ()
                else
                    Result.Error exitCode

        | Platform.Linux ->
            return Trace.failwith "Linux unmount is not implemented."
    }

/// Mount an SMB File share as local drive.
/// If retryUntilShareReady is true then in case of failure retry to mount the drive until the SMB share is online or a timeout expires.
let mountFileShare localDirPath (uncPath:string) username (password:string) timeout intervalsBetweenAttempts =
    async {
        let tryMount () =
            async {
                // Because we have seen that the mount can fail, retry mounting after waiting
                // a period of time.
                // At this point it's unclear why the mount is failing, it's an 'unspecified error' (0x80004005).
                try
                    Trace.info "Mounting SMB share %s at %s" uncPath localDirPath
                    let! processResult =
                        match Platform.Current with
                        | Platform.Windows ->
                            Process.startProcessAsync
                                "net.exe" (sprintf @"use %s %s /y" localDirPath uncPath)
                                System.Environment.CurrentDirectory
                                Process.ProcessStartFlags.None
                                (Process.AttemptToKillProcessAfterTimeout Constants.SmbMounting.MountProcessTimeout)
                                None
                        | Platform.Linux ->
                            // On Linux we pass credentials through command-line arguments to avoid
                            // chartacter escape issues. See https://linux.die.net/man/8/mount.cifs
                            Process.startProcessAsync
                                "/usr/bin/mount" (sprintf "-t cifs %s %s -o vers=3.0,dir_mode=0777,file_mode=0777" uncPath localDirPath)
                                Environment.OSRootPath.Linux
                                Process.ProcessStartFlags.None
                                (Process.AttemptToKillProcessAfterTimeout Constants.SmbMounting.MountProcessTimeout)
                                (Some [ "USER", username
                                        "PASSWD", password ])
                    if processResult.ExitCode = 0 then
                        return Some true
                    else
                        return None
                with ex ->
                    Trace.error "Exception when attempting to mount a drive. %A" ex
                    return None
            }

        let onError f x =
            x |> Result.mapError f |> ignore

        let domain = Uri(uncPath).Host

        // Special workaround for Azure Files SMB share: Azure Storage Account keys allow "/" as part of the key.
        // But "net.exe use" gets confused when "/"  is a first character of the Azure Storage key since it treats
        // the password as the command line flag.
        // There is no way to escape the "/", and double quotes around password do not help ether.
        // Current work around on Windows is to use Credential Manager (its command line tool
        // properly handles quotes around the password).
        match Platform.Current with
        | Platform.Windows ->
            let! initMountError = WindowsOnly.createDomainStoredCredential domain username password
            initMountError |> onError (Trace.failwith "%s")

            let! listCredentialsError = WindowsOnly.listDomainStoredCredential domain
            listCredentialsError |> onError (Trace.error "%s")
        | Platform.Linux ->
            return ()

        let! mountingResponse =
            Async.retryUntilSomeOrTimeout timeout intervalsBetweenAttempts tryMount

        let! isMountedError = isFileShareMounted domain
        isMountedError |> onError (Trace.failwith "%s")

        match Platform.Current with
        | Platform.Windows ->
            let! cleanupMountError = WindowsOnly.deleteDomainStoredCredential domain
            cleanupMountError |> onError (Trace.error "%s")
        | Platform.Linux ->
            return ()

        return
            match mountingResponse with
            | None ->
                Trace.failwith "SMB share %s could not be mounted in the expected amount of time (%O)." uncPath timeout
            | Some success ->
                Trace.info "Drive successfully mounted to %s" uncPath
                success

    }

/// Mounting options for `mountFilesShareIfNecessary`
type MountOptions =
| None = 0x00
| WarningIfShareExists = 0x02

/// Mount a remote SMB file share locally as a drive letter and returned the mounted location.
/// The desired drive letter location is specified by mountLocation.
/// No-op if a remote drive is already mounted at the target path.
///
/// If the remote UNC path happens to be of the form `\\localhost\x$` where `x` is a local drive letter
/// then skip drive mounting altogether and return the original UNC path.
/// (This can be useful for testing purpose when running without elevated privileges)
///
/// If retryUntilShareReady is true then in case of failure retry to mount the drive until the SMB share is online or a timeout expires.
let mountFilesShareIfNecessary (mountLocation:PlatformSpecific<string>) uncPath username password (option:MountOptions) timeout intervalsBetweenAttempts =
    async {
        if isLocalUncPath uncPath then
            return Some uncPath
        else
            let handleNetworkDriveExists mountingPath =
                async {
                    if option.HasFlag MountOptions.WarningIfShareExists then
                        Trace.warning "Network drive %s already exists. Assuming it's mounted as %s" mountingPath uncPath
                    return true
                }

            let mountingPath = mountLocation.ForCurrentPlatform

            let! shareMounted, mountingPath =
                match Platform.Current with
                | Platform.Windows ->
                    async {
                        let mountingPathExists =
                            System.IO.DriveInfo.GetDrives ()
                            |> Seq.tryFind (fun t -> System.String.Compare(t.Name, mountingPath + @"\", true) = 0)
                        let! shareMounted =
                            match mountingPathExists with
                            | None ->
                                Trace.info "Mounting drive %s as %s" mountingPath uncPath
                                mountFileShare mountingPath uncPath username password timeout intervalsBetweenAttempts
                            | Some v when v.DriveType = System.IO.DriveType.Network && v.IsReady ->
                                handleNetworkDriveExists mountingPath
                            | Some v when v.DriveType = System.IO.DriveType.Network && not v.IsReady ->
                                // This approach assumes only 1 failure (which may be right).
                                // We might consider recursively calling ourselves, but we would have to add a count so we don't retry forever.
                                async {
                                    match Platform.Current with
                                    | Platform.Windows ->
                                        Trace.warning "Drive exists %s as %s but it is not ready!" mountingPath uncPath
                                        Trace.info "Unmounting drive: %s" mountingPath
                                        let! unmount = unmountFileShare mountingPath
                                        let! mount =
                                            match unmount with
                                            | Result.Ok _ ->
                                                Trace.info "Mounting drive %s as %s" mountingPath uncPath
                                                mountFileShare mountingPath uncPath username password timeout intervalsBetweenAttempts
                                            | Result.Error exitCode ->
                                                Trace.warning "Failed to unmount drive: %s with exit code %d, skipped mounting call" mountingPath exitCode
                                                async.Return false
                                        return mount
                                    | Platform.Linux ->
                                        Trace.warning "Drive exists %s as %s but it is not ready. We cannot fix mounting disconnection on linux yet as the SMB unmouting feature is not implementd yet." mountingPath mountingPath
                                        return false
                                }
                            | Some v ->
                                invalidOp (sprintf "A drive named %s already exists, cannot mount a network drive with this letter." mountingPath)
                        return shareMounted, mountingPath
                    }
                | Platform.Linux ->
                    async {
                        let! isNetworkShareMountedToDirectory = async {
                            // The following command obtains all shares and searches for the specified directory
                            // mounted as a network share (file system type cifs) in the output.
                            // When the share is mounted, the output of the command will be non-empty.
                            let! result =  Process.startProcessAsync
                                                "/usr/bin/findmnt"
                                                "--df --output fstype,source,target"
                                                (Environment.OSRootPath.Linux)
                                                Process.ProcessStartFlags.RedirectStandardOutput
                                                Process.ProcessTimeoutAction.NoTimeout
                                                None
                            let outputLines = result.StandardOutput.Split [|'\n'|]
                            if outputLines.Length < 2 then
                                Trace.failwith "Incorrect findmnt output or parsing.  Command output: <%s>, split output (expected at least 2 lines): %A"
                                                result.StandardOutput
                                                outputLines
                            let foundShare =
                                Array.tryFind (fun (s:string) -> s.StartsWith "cifs" && s.EndsWith mountingPath) outputLines
                            return foundShare.IsSome
                        }

                        let! shareMounted =
                            if isNetworkShareMountedToDirectory then
                                handleNetworkDriveExists mountingPath
                            else if Directory.Exists mountingPath && (Directory.GetFiles(mountingPath).Length > 0) then
                                invalidOp (sprintf "A non-empty directory named %s already exists, will not mount a network drive here." mountingPath)
                            else
                                Trace.info "Mounting drive %s as %s" mountingPath uncPath
                                // If the share does not exist, create it.
                                Microsoft.FSharpLu.File.createDirIfNotExists mountingPath
                                mountFileShare mountingPath uncPath username password timeout intervalsBetweenAttempts
                        return shareMounted, mountingPath
                    }
            return if shareMounted then Some mountingPath else None
    }

/// Type representing a mounted share
type MountedNetworkShare =
    {
        /// The local directory the share is mounted on
        mountedDirectoryPath : File.FilePath

        /// The UNC path to the share root that is mounted at mountedDirectoryPath
        remoteUncPath : File.FilePath
    }

/// Mount a UNC share. If already mounted, return the path to it.
/// If retryUntilShareReady is true then in case of failure retry to mount the drive until the SMB share is online or a timeout expires.
let getMountedShare mountLocation uncPath username password options timeout intervalsBetweenAttempts =
    async {
        let! mountedPath = mountFilesShareIfNecessary
                            mountLocation
                            uncPath
                            username
                            password
                            options
                            timeout
                            intervalsBetweenAttempts
        match mountedPath with
        | None ->
            return invalidOp (sprintf "Could not mount share %s" uncPath)
        | Some path ->
            return
                {
                    MountedNetworkShare.mountedDirectoryPath = path
                    MountedNetworkShare.remoteUncPath = uncPath
                }
    }

/// Returns the native system directory on a Windows system
let getWindowsNativeSystemDirectory () =
    if Environment.Is64BitOperatingSystem && not Environment.Is64BitProcess then
        System.Environment.GetEnvironmentVariable("windir") + @"\Sysnative"
    else
        System.Environment.SystemDirectory

/// Shut down the local machine
let shutdown n =
    // We don't shutdown immediately to give current process a chance to terminate
    // (this is to prevent EndToEnd remote automation to get stuck.)
    match Platform.Current with
    | Platform.Windows ->
        Process.startProcessAsyncAndWait
            (sprintf @"%s\shutdown.exe" (getWindowsNativeSystemDirectory()))
            (sprintf "/s /t %A" n)
            (System.IO.Directory.GetCurrentDirectory())
            Process.ProcessStartFlags.Elevated
    | Platform.Linux ->
        Process.startProcessAsyncAndWait
            "/usr/sbin/shutdown"
            (sprintf "-t %A" n)
            // Use a path that is guaranteed to be on the system drive - this path must
            // not be in the home directory, since sysprep, which executes prior to
            // shutdown, removes the home directory.
            Environment.OSRootPath.Linux
            Process.ProcessStartFlags.None

type DisableAntivirusBeforeSysprep = DisableAntivirusBeforeSysprep of bool

type ShutdownAfterSysprep = ShutdownAfterSysprep of bool

/// Sysprep the local machine
let sysprep (ShutdownAfterSysprep shutdownAfterSysprep) (DisableAntivirusBeforeSysprep disableAntivirusBeforeSysprep) =
    async {
        match Platform.Current with
        | Platform.Windows ->
            Trace.info "Starting sysprep"

            // On Windows, disable antivirus (Defender) in group policy.
            // This is done here because it requires a reboot, to avoid
            // an extra reboot when rehydrating the VM from the captured image.
            let arguments = sprintf "%s %s"
                                    (if shutdownAfterSysprep then
                                        "-ShutdownAfterSysprep" else "")
                                    (if disableAntivirusBeforeSysprep then
                                        "-DisableAntivirusBeforeSysprep" else "")
            let currentAssemblyDirectory =
                let assembly = System.Reflection.Assembly.GetExecutingAssembly()
                System.IO.Path.GetDirectoryName(assembly.Location)

            let runAsNative = true
            let! result = Process.startScriptAsyncAux
                            (sprintf "%s\\Sysprep.ps1" currentAssemblyDirectory)
                            arguments
                            currentAssemblyDirectory
                            (Process.ProcessStartFlags.Elevated ||| Process.ProcessStartFlags.RedirectStandardOutput)
                            runAsNative
                            Process.NoTimeout
                            None
            Trace.info "Sysprep.ps1 exit code: %d, stdout: %s" result.ExitCode result.StandardOutput

            return result.ExitCode

        | Platform.Linux ->
            Trace.info "Removing extension directories prior to sysprep"

            let customScriptDirectories =
                Directory.GetDirectories("/var/lib/waagent")
                |> Array.filter (fun s -> s.Contains("CustomScriptForLinux"))

            if customScriptDirectories.Length < 1 then
                Trace.warning "Warning: extension could not be deleted.  Giving you time to delete it manually..."
                do! Async.Sleep(TimeSpan.FromMinutes(2.0).TotalMilliseconds |> int)

            customScriptDirectories |> Array.iter Microsoft.FSharpLu.File.deleteDirIfExists

            Trace.info "Starting waagent"
            let! sysprepResult =
                Process.startProcessAsyncAndWait
                    "/usr/sbin/waagent"
                    "-deprovision+user -force"
                    Environment.OSRootPath.Linux
                    Process.ProcessStartFlags.None
            Trace.info "Sysprep with waagent completed with exit code: %d" sysprepResult

            if shutdownAfterSysprep then
                let shutdownDelayInSeconds = 3
                Trace.info "Scheduling machine shut down in %ds..." shutdownDelayInSeconds
                let! shutdownExitCode = shutdown shutdownDelayInSeconds
                Trace.info "Shutdown returned code %d" shutdownExitCode

            return sysprepResult
    }

/// Waits for the machine name to be assigned to the VM
/// This is needed for Linux virtual machines where the machine host name gets assigned
/// only after the machine is started.
let waitForMachineNameInitialization() =
    let checkMachineName() =
        async {
            Trace.info "Trying to get the machine name..."
            return
                try
                    match Platform.Environment.MachineName with
                    | null
                    | Constants.MachineNameInitialization.LinuxUninitializedName ->
                        Trace.info "Machine name not yet assigned. Retrying in a moment."
                        None
                    | machineName ->
                        Trace.info "Machine name: %s" machineName
                        Some machineName
                with
                | :? System.Net.Sockets.SocketException as e ->
                    Trace.info "Socket exception (%s) indicates that network is still configuring. Retrying in a moment." e.Message
                    None
        }
    Async.retryUntilSomeOrTimeout
            Constants.MachineNameInitialization.Timeout
            Constants.MachineNameInitialization.IntervalsBetweenAttempts
            checkMachineName
