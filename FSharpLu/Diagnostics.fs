/// System diagnotics, process and logging helpers
module Microsoft.FSharpLu.Diagnostics

open System
open System.Security.Principal

/// Determine if the process runs elevated
let isElevated () =
    use user = WindowsIdentity.GetCurrent()
    let principal = new WindowsPrincipal(user)
    principal.IsInRole(WindowsBuiltInRole.Administrator)

/// ensure that the process runs with elevated permissions
let ensureAdmin () = 
    if not (isElevated ()) then
        invalidOp "Elevated privileges required."

/// Ask for user confirmation before completing a task
let confirm message =
    printf "%s  Press CTRL+C to abort, ENTER to continue." message
    System.Console.ReadLine() |> ignore

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
            ExecutionTime : TimeSpan
        }

    /// Process start flag parameters for startProcessAsync
    [<FlagsAttribute>]
    type ProcessStartFlags =
        | None = 0x0
        | Elevated = 0x1
        | Minimized = 0x2
        | RedirectStandardOutput = 0x4

    /// Process timeout option
    type ProcessTimeoutAction =
        | AttemptToKillProcessAfterTimeout of TimeSpan
        | KeepTheProcessRunningAfterTimeout of TimeSpan
        | NoTimeout

    /// Starts a process and returns an asynchronous workflow that waits
    /// for it to terminate
    let startProcessAsync command arguments workingDir (flags:ProcessStartFlags) (timeout:ProcessTimeoutAction) =
        
        let elevated, minimized, redirectStdOut =
            flags.HasFlag ProcessStartFlags.Elevated,
            flags.HasFlag ProcessStartFlags.Minimized,
            flags.HasFlag ProcessStartFlags.RedirectStandardOutput
        
        let useShellExecute =
            if minimized then
                Trace.warning "Important: Shell execute may pop-up the IE security zone window which synchronously blocks the call to Start()! To run minimized shell execute has to be used"
                true
            else
                false
        
        let instance =
            new Process(
                StartInfo =
                    ProcessStartInfo
                        (
                            FileName = command,
                            WorkingDirectory = workingDir,
                            Arguments = arguments,
                            CreateNoWindow = false,
                            UseShellExecute = useShellExecute,
                            RedirectStandardOutput = redirectStdOut,
                            WindowStyle = (
                                if minimized then
                                    ProcessWindowStyle.Minimized
                                else
                                    ProcessWindowStyle.Normal
                                ),

                            Verb = (
                                if elevated then 
                                     "runas"
                                else
                                    String.Empty)
                        ),
                EnableRaisingEvents = true)

        Trace.info "Launching '%s %s'" command arguments
        let startTime = System.DateTime.UtcNow
        instance.Exited.Add (fun a -> let endTime = System.DateTime.UtcNow
                                      if instance.ExitCode = 0 then
                                          Trace.info "Process executed successfully in %O: '%O %O'" (endTime-startTime) command arguments
                                      else
                                          Trace.info "Process execution failed in %O with error code 0x%X: '%O %O'" (endTime-startTime) (int32 instance.ExitCode) command arguments)

        if not (instance.Start()) then
            let message = sprintf "Could not start command: '%s' with parameters '%s'" command arguments
            raise <| System.InvalidOperationException(message)
        else
            let processExitedResult() =
                {
                    ProcessResult.ProcessExited = true
                    ExitCode = instance.ExitCode
                    ExecutionTime = System.DateTime.UtcNow - startTime
                    StandardOutput =
                        if redirectStdOut then 
                            instance.StandardOutput.ReadToEnd() 
                        else 
                            String.Empty
                }

            let runProcessWithTimeout (t:TimeSpan) (kill: bool) =
                async {
                    try
                        let! tsk = Async.StartChild(Async.AwaitEvent(instance.Exited), int t.TotalMilliseconds)
                        let! _ = tsk
                        Trace.info "%s %s exited with code: %d" command arguments instance.ExitCode
                        return processExitedResult()
                    with
                    | :? System.TimeoutException ->
                        Trace.info "%s %s did not exit within allocated time out of %f seconds. Should process be killed: %A" command arguments t.TotalSeconds kill
                    
                        if kill then
                            try instance.Kill() with _ -> ()

                        return
                            {
                                ProcessResult.ProcessExited = false
                                ExitCode = -1
                                ExecutionTime = System.DateTime.UtcNow - startTime
                                StandardOutput = String.Empty
                            }
                }

            async {
                match timeout with
                | NoTimeout ->
                    let! _ = Async.AwaitEvent(instance.Exited)
                    Trace.info "%s %s exited with code: %d" command arguments instance.ExitCode
                    return processExitedResult()
                | AttemptToKillProcessAfterTimeout t -> return! runProcessWithTimeout t true
                | KeepTheProcessRunningAfterTimeout t -> return! runProcessWithTimeout t false
            }

    // Start a process and returns an asynchronous workflow that waits
    // for it to terminate and return the process exit code
    let startProcessAsyncAndWait command arguments workingDir flags =
        async {
            let! processResult = startProcessAsync command arguments workingDir flags NoTimeout
            return processResult.ExitCode
        }