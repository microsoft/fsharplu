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
            if not <| isNull ps then
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

    /// Starts a process and asynchronously wait
    /// for it to terminate
    let startProcessAsync command arguments workingDir (flags:ProcessStartFlags) (timeout:ProcessTimeoutAction) (environmentVariables:List<string*string> option) =
        async {
            let maskedArguments = if flags.HasFlag(ProcessStartFlags.SensitiveArguments) then "***MASKED***" else arguments
            let redirectOutput = flags.HasFlag ProcessStartFlags.RedirectStandardOutput
            let redirectErrors = flags.HasFlag ProcessStartFlags.RedirectStandardError
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
            use instanceExit = new System.Threading.AutoResetEvent(false)

            // Note: it's important to register this event __before__ calling instance.Start()
            // to avoid a deadlock if the process terminates too quickly...
            instance.Exited.Add
                (fun _ ->
                    timer.Stop()
                    // ... but this handler still gets called if the process instance gets killed
                    // (e.g. using .Kill() function) before the underlying OS process gets actually
                    /// started with .Start()!
                    /// This then causes below  evaluation of property `.ExitCode` to throw with:
                    //    `System.InvalidOperationException: No process is associated with this object`
                    // we thus wrap the handler within a try .. catch block.
                    try
                        Trace.info "Process execution terminated in %O with exit code 0x%X: '%O %O'" timer.Elapsed (int32 instance.ExitCode) command arguments
                    with :? System.InvalidOperationException ->
                        Trace.info "Process execution terminated abruptly in %O with no exit code: '%O %O'" timer.Elapsed command arguments
                    if not instanceExit.SafeWaitHandle.IsClosed then
                        instanceExit.Set() |> ignore)

            // IMPORTANT NOTE:
            // It is tempting here to use 
            //      Async.AwaitEvent(instance.Exited) 
            // to detect when the process ends, instead of relying on 
            // an extra System.Threading.AutoResetEvent.
            //
            // However this can hang when stars don't align...
            // (See unit test `NoHangInStartProcessLogic` for details.)
            // Also, awaiting with process.Wait also leads to hang when
            // attempting to capture stdout/stderr.
            let waitAsync =
               match timeout with
                | NoTimeout ->
                    Async.AwaitWaitHandle(instanceExit)
                | AttemptToKillProcessAfterTimeout t
                | KeepTheProcessRunningAfterTimeout t ->
                    Async.AwaitWaitHandle(instanceExit, int <| t.TotalMilliseconds)

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

            if redirectOutput then
                instance.OutputDataReceived.Add(appendHandler noMoreOutput standardOutput)

            if redirectErrors then
                instance.ErrorDataReceived.Add(appendHandler noMoreError standardError)

            if not (instance.Start()) then
                let message = sprintf "Could not start command: '%s' with parameters '%s'" command arguments
                return raise <| System.InvalidOperationException(message)
            else
                if redirectOutput then
                    instance.BeginOutputReadLine()
                if redirectErrors then
                    instance.BeginErrorReadLine()

                let! exitedBeforeTimeout = waitAsync

                let exitCode = 
                    if exitedBeforeTimeout then
                        Trace.info "(%d) %s %s exited with code: %d" instance.Id command arguments instance.ExitCode
                        instance.ExitCode
                    else
                        match timeout with
                        | NoTimeout ->
                            failwith "Impossible case: waitAsync timed out with an infinite timeout value!"
                        | AttemptToKillProcessAfterTimeout t
                        | KeepTheProcessRunningAfterTimeout t ->
                            Trace.info "Process (%d) [%s %s] did not exit within allocated time out of %f seconds." instance.Id command arguments t.TotalSeconds
                            // Note: calling instance.ExitCode would throw:
                            //  System.InvalidOperationException: Process must exit before requested information can be determined.
                            -1

                if exitedBeforeTimeout then
                    // Read the stdout and stderr
                    if redirectOutput then
                        let! _ = Async.AwaitWaitHandle noMoreOutput
                        Trace.verbose "Standard output captured (%d) [%s %s]" instance.Id command arguments

                    if redirectErrors then
                        let! _ = Async.AwaitWaitHandle noMoreError
                        Trace.verbose "Standard error captured (%d) [%s %s]" instance.Id command arguments
                else
                    // We should not read stdoud/stderr since the time out period is already exceeded,
                    // and reading the standard outputerror would indirectly wait for the process to terminate!
                    match timeout with
                    | KeepTheProcessRunningAfterTimeout _
                    | NoTimeout -> ()
                    | AttemptToKillProcessAfterTimeout t ->
                        Trace.info "Killing timed-out process (%d) [%s %s]" instance.Id command arguments
                        try 
                            instance.Kill()
                            Trace.info "Process killed (%d) [%s %s]" instance.Id command arguments
                        with _ ->
                            Trace.warning "Failed to kill process (%d) [%s %s]" instance.Id command arguments

                return
                    {
                        ProcessResult.ProcessExited = exitedBeforeTimeout
                        ProcessId = instance.Id
                        ExitCode = exitCode
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
