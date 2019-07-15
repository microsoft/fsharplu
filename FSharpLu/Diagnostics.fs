/// System diagnotics, process and logging helpers
namespace Microsoft.FSharpLu.Diagnostics

open System
open System.Security.Principal
open Microsoft.FSharpLu.Platform

module Elevation =
    /// Determine if the process runs elevated
    let isElevated () =
        use user = WindowsIdentity.GetCurrent()
        let principal = WindowsPrincipal(user)
        principal.IsInRole(WindowsBuiltInRole.Administrator)

    /// ensure that the process runs with elevated permissions
    let ensureElevated () =
        if not (isElevated ()) then
            invalidOp "Elevated privileges required."

/// Process execution helpers
module Process =
    open System.Diagnostics
    open Microsoft.FSharpLu.Logging

    /// Process information returned by startProcessAsync
    type ProcessResult =
        {
            ProcessExited : bool
            ExitCode : int
            StandardOutput : string
            StandardError : string
            ExecutionTime : TimeSpan
            ProcessId : int
        }

    /// Process start flag parameters for startProcessAsync
    [<FlagsAttribute>]
    type ProcessStartFlags =
        | None = 0x0
        | Elevated = 0x1
        | Minimized = 0x2
        | RedirectStandardOutput = 0x4
        | RedirectStandardError = 0x8
        /// Arguments contain sensitive information like passwords that should not be printed out to logs
        | SensitiveArguments = 0x10
        /// Environment variables contain sensitive information like passwords that should not be printed out to logs
        | SensitiveEnvironmentVariables = 0x20
        /// Run command elevated if environment supports elevation, otherwise run as regular user
        | ElevatedIfSupported = 0x40

    /// Process timeout option
    type ProcessTimeoutAction =
        | AttemptToKillProcessAfterTimeout of TimeSpan
        | KeepTheProcessRunningAfterTimeout of TimeSpan
        | NoTimeout

    /// Kills the process by process unique identifier - pid.
    let killProcess (pid:uint32) =
        try
            let ps = Process.GetProcessById (int pid)
            if ps <> null then
                ps.Kill ()
        with
        | :? System.ArgumentException ->
            Trace.warning "Could not kill process %d" pid

    /// Kills the process by process name.
    let killProcessByName (name: string) =
        if name.EndsWith(".exe") then
            Trace.warning "Killing a process by name expects a friendly name without .exe to be used"

        let ps = System.Diagnostics.Process.GetProcessesByName(name)
        Trace.info "Trying to find process %s, found processes to kill: %A" name ps
        ps |> Array.iter(fun p ->
            try
                p.Kill()
                let processExited = p.WaitForExit(int (TimeSpan.FromSeconds(2.0).TotalMilliseconds))
                if processExited then
                    // From MSDN: When standard output has been redirected to asynchronous event handlers,
                    // it is possible that output processing will not have completed when p.WaitForExit() returns.
                    // To ensure that asynchronous event handling has been completed, call the WaitForExit() overload
                    // that takes no parameter after receiving a true from this overload.
                    p.WaitForExit() |> ignore
                else
                    Trace.error "Process %s, pid: %d was not terminated after attempting to kill it." name p.Id
            with
            | :? System.ComponentModel.Win32Exception as ex ->
                // This is a warning, because it may be expected, e.g. when the process is terminating when killed.
                Trace.warning "Failed to kill %s, pid: %d with exception: %A" name p.Id ex
            | :? System.InvalidOperationException as ex ->
                Trace.info "Did not kill %s, pid: %d because it has already exited. Exception: %A" name p.Id ex
            | :? System.SystemException as ex ->
                // May be thrown by WaitForExit if the process no longer exists.
                Trace.info "Caught exception %A while waiting for the process to exit. Process %s, pid: %d" ex name p.Id
        )

    /// Create a Process instance based on the specified parameters
    let createProcessInstance command arguments workingDir (flags:ProcessStartFlags) =
        if flags.HasFlag(ProcessStartFlags.Minimized)
        && (flags.HasFlag(ProcessStartFlags.RedirectStandardOutput)|| flags.HasFlag(ProcessStartFlags.RedirectStandardError)) then
            Trace.failwith "Incompatible switches: running minimized requires starting the process through ShellExecute while RedirectStandardOutput or RedirectStandardError requires starting the process directly."

        let useShellExecute =
            if flags.HasFlag ProcessStartFlags.Minimized then
                Trace.warning "Important: Shell execute may pop-up the IE security zone window which synchronously blocks the call to Start()! To run minimized shell execute has to be used"
                true
            else
                false

        new Process(
            StartInfo =
                ProcessStartInfo
                    (
                        FileName = command,
                        WorkingDirectory = workingDir,
                        Arguments = arguments,
                        CreateNoWindow = false,
                        // Important: Shell execute may pop-up the
                        // IE security zone window which synchronously blocks the call to Start()!
                        // To run minimized shell execute has to be used
                        UseShellExecute = useShellExecute,
                        RedirectStandardOutput = flags.HasFlag ProcessStartFlags.RedirectStandardOutput,
                        RedirectStandardError = flags.HasFlag ProcessStartFlags.RedirectStandardError,
                        WindowStyle = (
                            if flags.HasFlag ProcessStartFlags.Minimized then
                                ProcessWindowStyle.Minimized
                            else
                                ProcessWindowStyle.Normal
                            ),

                        Verb = (
                            if flags.HasFlag(ProcessStartFlags.Elevated) then
                                match Platform.Current with
                                | Platform.Linux -> invalidOp "option elevated not supported on Linux"
                                | Platform.Windows -> "runas"
                            else if flags.HasFlag(ProcessStartFlags.ElevatedIfSupported) then
                                match Platform.Current with
                                | Platform.Linux -> String.Empty
                                | Platform.Windows -> "runas"
                            else
                                String.Empty)
                    ),
            EnableRaisingEvents = true)

    /// Starts a process and returns an asynchronous workflow that waits
    /// for it to terminate
    let startProcessAsync command arguments workingDir (flags:ProcessStartFlags) (timeout:ProcessTimeoutAction) (environmentVariables:List<string*string> option) =
        async {
            let maskedArguments = if flags.HasFlag(ProcessStartFlags.SensitiveArguments) then "***MASKED***" else arguments

            use instance = createProcessInstance command arguments workingDir flags

            environmentVariables
            |> Option.iter
                (fun d -> // UseShellExecute must be false if environment variables are set
                    instance.StartInfo.UseShellExecute <- false
                    let sensitiveVariables = flags.HasFlag ProcessStartFlags.SensitiveEnvironmentVariables
                    d |> List.iter(fun (k,v) ->
                                    if instance.StartInfo.EnvironmentVariables.ContainsKey(k) then
                                        if sensitiveVariables then
                                            Trace.info "Remove environment variable '%s' with sensitive content" k
                                        else
                                            Trace.info "Remove environment variable '%s' with value <%s>" k instance.StartInfo.EnvironmentVariables.[k]

                                        instance.StartInfo.EnvironmentVariables.Remove(k)

                                    instance.StartInfo.EnvironmentVariables.Add(k,v)
                                    if sensitiveVariables then
                                        Trace.info "Set environment variable '%s' with sensitive content" k
                                    else
                                        Trace.info "Set environment variable '%s' to <%s>" k v))

            Trace.info "Launching '%s %s'" command maskedArguments
            let timer = System.Diagnostics.Stopwatch()

            timer.Start()

            // Note: it's important to register this event before calling instance.Start()
            // to avoid a deadlock if the process terminates too quickly...
            instance.Exited.Add
                (fun _ ->
                    timer.Stop()
                    /// ... but then this handler still gets called if the process instance gets killed
                    /// with .Kill() before the underlying OS process gets actually started with .Start()!
                    /// which then causes the below evaluation of `.ExitCode` to throw
                    //    `System.InvalidOperationException: No process is associated with this object`
                    try
                        // we still need to check if HasExited otherwise the call to .ExitCode can throw
                        //  `System.InvalidOperationException: No process is associated with this object`
                        // on some .NET runtimes (e.g. unit tests in AppVeyor) if the process was forced terminated.
                        Trace.info "Process execution terminated in %O with exit code 0x%X: '%O %O'" timer.Elapsed (int32 instance.ExitCode) command arguments
                        ()
                    with :? System.InvalidOperationException ->
                        Trace.info "Process execution terminated abruptly in %O with no exit code: '%O %O'" timer.Elapsed command arguments
                        ())

            let waitEvent = Async.AwaitEvent(instance.Exited)
            let! waitAsync =
               match timeout with
                | NoTimeout ->
                    Async.StartChild(waitEvent)
                | AttemptToKillProcessAfterTimeout t
                | KeepTheProcessRunningAfterTimeout t ->
                    Async.StartChild(waitEvent, int t.TotalMilliseconds)

            let! t = Async.Sleep(2000) /// attempt to trigger bug

            // Standard output must be read prior to waiting on the instance to exit.
            // Otherwise, a deadlock is created when the child process has filled its output
            // buffer and waits for the parent to consume it, and the parent waits for the
            // child process to exit first.
            // Reference: https://stackoverflow.com/questions/139593/processstartinfo-hanging-on-waitforexit-why?lq=1
            let standardOutput = System.Text.StringBuilder()
            let standardError = System.Text.StringBuilder()
            use noMoreOutput = new System.Threading.AutoResetEvent(false)
            use noMoreError = new System.Threading.AutoResetEvent(false)
            let appendHandler
                    (endOfStreamEvent:System.Threading.AutoResetEvent)
                    (aggregator:System.Text.StringBuilder)
                    (args:DataReceivedEventArgs) =
                if isNull args.Data then
                    if not endOfStreamEvent.SafeWaitHandle.IsClosed then
                        endOfStreamEvent.Set() |> ignore
                else
                    aggregator.AppendLine(args.Data) |> ignore

            if flags.HasFlag ProcessStartFlags.RedirectStandardOutput then
                instance.OutputDataReceived.Add(appendHandler noMoreOutput standardOutput)

            if flags.HasFlag ProcessStartFlags.RedirectStandardError then
                instance.ErrorDataReceived.Add(appendHandler noMoreError standardError)

            if not (instance.Start()) then
                let message = sprintf "Could not start command: '%s' with parameters '%s'" command arguments
                return raise <| System.InvalidOperationException(message)
            else
                try
                    if flags.HasFlag ProcessStartFlags.RedirectStandardOutput then
                        instance.BeginOutputReadLine()
                    if flags.HasFlag ProcessStartFlags.RedirectStandardError then
                        instance.BeginErrorReadLine()

                    let! _ = waitAsync
                    Trace.info "%s %s exited with code: %d" command arguments instance.ExitCode
                    return
                        {
                            ProcessResult.ProcessExited = true
                            ProcessId = instance.Id
                            ExitCode = instance.ExitCode
                            ExecutionTime = timer.Elapsed
                            StandardOutput = standardOutput.ToString()
                            StandardError = standardError.ToString()
                        }
                with
                | :? System.TimeoutException as e ->
                    match timeout with
                    | NoTimeout -> ()
                    | AttemptToKillProcessAfterTimeout t ->
                        Trace.info "%s %s did not exit within allocated time out of %f seconds. Killing process." command arguments t.TotalSeconds
                        try instance.Kill() with _ -> ()
                    | KeepTheProcessRunningAfterTimeout t ->
                        Trace.info "%s %s did not exit within allocated time out of %f seconds." command arguments t.TotalSeconds
                    return
                        {
                            ProcessResult.ProcessExited = false
                            ProcessId = instance.Id
                            ExitCode = instance.ExitCode
                            ExecutionTime = timer.Elapsed
                            StandardOutput = standardOutput.ToString()
                            StandardError = standardError.ToString()
                        }
        }

    // Start a process and returns an asynchronous workflow that waits
    // for it to terminate and return the process exit code
    let startProcessAsyncAndWait command arguments workingDir flags =
        async {
            let! processResult = startProcessAsync command arguments workingDir flags NoTimeout None
            return processResult.ExitCode
        }

    /// Start a process and asynchronously wait for it to terminate
    /// redirect stdout and stderr to Trace.info and Trace.error
    let startProcessWithStdTracingAsync command arguments workingDir (flags:ProcessStartFlags) =
        async {
            use resetEvent = new System.Threading.ManualResetEvent false
            let maskedArguments = if flags.HasFlag(ProcessStartFlags.SensitiveArguments) then "***MASKED***" else arguments

            use p = createProcessInstance
                        command
                        arguments
                        workingDir
                        (ProcessStartFlags.RedirectStandardError
                         ||| ProcessStartFlags.RedirectStandardOutput
                         ||| flags)

            p.ErrorDataReceived
                |> Event.add (fun dataReceived ->
                    if isNull dataReceived.Data then
                        () // There is no more standard error
                    else
                        Trace.error "%s" dataReceived.Data)

            p.OutputDataReceived
                |> Event.add(fun dataReceived ->
                    if isNull dataReceived.Data then
                        // There is no more standard output
                        ()
                    else
                        Trace.info "%s" dataReceived.Data)

            p.Exited.Add (fun _ -> resetEvent.Set() |> ignore)
            if not (p.Start()) then
                let message = sprintf "Could not start command: '%s' with parameters '%s'" command maskedArguments
                raise <| System.InvalidOperationException(message)

            p.BeginOutputReadLine()
            p.BeginErrorReadLine()
            let! _ = resetEvent |> Async.AwaitWaitHandle
            return p.ExitCode
        }

    /// Start an external command, Powershell script or batch file and asynchronously wait for it to terminate.
    let startScriptAsyncAux (script:string) additionalParameters workingDir flags runAsNative timeout environmentVariables =
        let batchScriptCommand scriptFileName =
            "cmd.exe", sprintf "/c %s %s" scriptFileName additionalParameters

        let powershellScriptCommand scriptFileName =
            let systemDir =
                if runAsNative && Environment.Is64BitOperatingSystem && not Environment.Is64BitProcess then
                    System.Environment.GetEnvironmentVariable("windir") + "\\sysnative"
                else
                    System.Environment.GetEnvironmentVariable("SystemRoot") + "\\system32"
            let powershell = sprintf @"%s\WindowsPowerShell\v1.0\powershell.exe" systemDir
            powershell, sprintf "-nologo -NoProfile -executionpolicy bypass -Command \"& { %s %s } ; exit $LASTEXITCODE\"" scriptFileName additionalParameters

        let bashScriptCommand scriptFileName =
            "/bin/bash", (sprintf "-c \"chmod +x %s; %s %s\"" scriptFileName scriptFileName additionalParameters)

        let command, parameters =
            if script.EndsWith(".cmd", System.StringComparison.InvariantCultureIgnoreCase) then
                batchScriptCommand script
            else if script.EndsWith(".ps1", System.StringComparison.InvariantCultureIgnoreCase) then
                powershellScriptCommand script
            else if script.EndsWith(".exe", System.StringComparison.InvariantCultureIgnoreCase) then
                script, ""
            else if script.EndsWith(".sh", System.StringComparison.InvariantCulture) then
                bashScriptCommand script
            else
                invalidArg "script" "Unsupported script file"

        startProcessAsync command parameters workingDir flags timeout environmentVariables

    /// Start an external command, Powershell script or batch file, asynchronously wait for it to terminate.
    /// Return the process exit code.
    let startScriptAsync script additionalParameters workingDir flags runAsNative =
        async {
            let! processResult = startScriptAsyncAux script additionalParameters workingDir flags runAsNative NoTimeout None
            return processResult.ExitCode
        }


module Assembly =
    open System.Reflection
    open System.Runtime.CompilerServices

    /// Get path of the currently executing assembly
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let getCurrentAssemblyPath() =
        let callingAssembly = Assembly.GetCallingAssembly()
        callingAssembly.Location


module Extensions =
    /// Extension to use Stopwatch to measure performance of async computations
    type System.Diagnostics.Stopwatch with
        member x.Measure(task:Async<'t>) =
            async {
                x.Restart()
                let! r = task
                x.Stop()
                return r
            }
