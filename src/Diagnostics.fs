module Microsoft.FSharpLu.Diagnostics

open System
open System.Security.Principal
open Microsoft.FSharpLu.Logging

/// Determine if the process runs elevated
let isElevated () =
    use user = WindowsIdentity.GetCurrent()
    let principal = new WindowsPrincipal(user)
    principal.IsInRole(WindowsBuiltInRole.Administrator)

/// ensure that the process runs with elevated permissions
let ensureAdmin () = 
    if not (isElevated ()) then
        invalidOp "Elevated privileges required."

// Start a process and returns an asynchronous workflow that waits 
// for it to terminate
let startProcessAsync (log:Logging.Logger<_,_>) command arguments workingDir elevated minimized =
    let instance = 
        new System.Diagnostics.Process(
            StartInfo =
                System.Diagnostics.ProcessStartInfo
                    (
                        FileName = command,
                        WorkingDirectory = workingDir,
                        Arguments = arguments,
                        CreateNoWindow = false,
                        // Important: Shell execute may pop-up the 
                        // IE security zone window which
                        // synchronoulsy blocks the call to Start()!
                        UseShellExecute = false,
                        WindowStyle = (if minimized then System.Diagnostics.ProcessWindowStyle.Minimized else System.Diagnostics.ProcessWindowStyle.Normal)
                    ),
            EnableRaisingEvents = true)

    if elevated then
        instance.StartInfo.Verb <- "runas" // request elevated privilege

    log.write "Launching '%s %s'" command arguments
    let startTime = System.DateTime.Now
    instance.Exited.Add (fun a -> let endTime = System.DateTime.Now
                                  if instance.ExitCode = 0 then
                                      log.write "Process executed successfully in %O: '%O %O'" (endTime-startTime) command arguments
                                  else
                                      log.write "Process execution failed in %O with error code 0x%X: '%O %O'" (endTime-startTime) (int32 instance.ExitCode) command arguments)
    if not (instance.Start()) then
        let message = sprintf "Could not start command: '%s' with parameters '%s'" command arguments
        raise <| System.InvalidOperationException(message)
    else
        async 
            {
                let! waitForProcessToTerminate = Async.AwaitEvent(instance.Exited)
                return instance.ExitCode
            }

/// Ask for user confirmation before completing a task
let confirm message =
    printf "%s  Press CTRL+C to abort, ENTER to continue." message
    System.Console.ReadLine() |> ignore
