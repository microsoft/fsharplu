namespace Microsoft.FSharpLu.Diagnostics

open Microsoft.VisualStudio.TestTools.UnitTesting
open Microsoft.FSharpLu.Diagnostics

[<AutoOpen>]
module DiagnosticsTests =

    let processAsyncAuxTest command args timeSpan startFlags =
        Process.startProcessAsync
                    command
                    args
                    "."
                    startFlags
                    (Process.AttemptToKillProcessAfterTimeout timeSpan)
                    None
                |> Async.RunSynchronously

    let processAsyncAuxTimeout startFlags : unit =
        let timeoutCmd = "ping.exe", "127.0.0.1 -n 60"

        let timeSpan = System.TimeSpan.FromMilliseconds 10.0 //2.0

        let r = processAsyncAuxTest
                        (fst timeoutCmd)
                        (snd timeoutCmd)
                        timeSpan
                        startFlags

        if r.ProcessExited then
            if r.ExecutionTime < timeSpan then
                failwith "Expected process execution to timeout"
            else
                failwithf "Process exited but it took longer than expected. Time it took: %f ms, expected: %f ms" r.ExecutionTime.TotalMilliseconds timeSpan.TotalMilliseconds

    let processAsyncAuxTimeoutStdOut() =
        processAsyncAuxTimeout Process.ProcessStartFlags.RedirectStandardOutput

    let processAsyncAuxTimeoutStdErr() =
        processAsyncAuxTimeout Process.ProcessStartFlags.RedirectStandardError

    let processAsyncAuxTimeoutStdOutAndStdErr() =
        processAsyncAuxTimeout
            (Process.ProcessStartFlags.RedirectStandardOutput |||
             Process.ProcessStartFlags.RedirectStandardError)

    let processAsyncAuxExit() =
        let timeoutCmd = "cmd", "/C \"ping 127.0.0.1 -n 2 > nul\""
        let timeSpan = System.TimeSpan.FromMilliseconds 10000.0

        let r = processAsyncAuxTest
                        (fst timeoutCmd)
                        (snd timeoutCmd)
                        timeSpan
                        Process.ProcessStartFlags.RedirectStandardOutput

        if r.ProcessExited && r.ExecutionTime < timeSpan then
            printfn "Process exited (as expected)"
        elif r.ProcessExited then
            failwithf "Process exited but it took longer than expected. Time it took: %f ms, expected: %f ms" r.ExecutionTime.TotalMilliseconds timeSpan.TotalMilliseconds
        else
            failwith "Timed out waiting for the process "

    /// lineCount must be <10000
    let expectedOutputLength prefix lineCount =
        let prefixLength = String.length prefix
        let eol = String.length System.Environment.NewLine
        (prefixLength + 1 + eol) * 9 +
        (prefixLength + 2 + eol) * 90 +
        (prefixLength + 3 + eol) * 900 +
        (prefixLength + 4 + eol) * (lineCount - 999)

    let processAsyncAuxLargeStandardOutput() =
        let lineCount = 5000
        let cmd, arguments = "cmd", "/c \"@echo off & for /l %x in (1, 1, " + lineCount.ToString() + ") do echo Processed: %x\""
        let expectedOutputLength = expectedOutputLength "Processed: " lineCount
        let timeSpan = System.TimeSpan.FromSeconds 20.0

        let r = processAsyncAuxTest
                        cmd
                        arguments
                        timeSpan
                        Process.ProcessStartFlags.RedirectStandardOutput

        if r.ProcessExited && r.ExecutionTime < timeSpan then
            printfn "Process exited (as expected)"
            let l = r.StandardOutput.Length 
            if l <> expectedOutputLength then
                failwithf "Standard output should be %d characters long, received %d (now: %d)" expectedOutputLength l  r.StandardOutput.Length
            else
                printfn "Found at least %d output chars as expected" expectedOutputLength
        elif r.ProcessExited then
            failwithf "Process exited but it took longer than expected. Time it took: %f ms, expected: %f ms" r.ExecutionTime.TotalMilliseconds timeSpan.TotalMilliseconds
        else
            failwith "Timed out waiting for the process "

    let processAsyncAuxLargeStandardError() =
        let lineCount = 5000
        let cmd, arguments = "cmd", "/c \"@echo off & for /l %x in (1, 1, " + lineCount.ToString() + ") do echo Processed: %x 1>&2\""
        let timeSpan = System.TimeSpan.FromSeconds 20.0
        let expectedErrorLength = expectedOutputLength "Processed:  " lineCount

        let r = processAsyncAuxTest
                        cmd
                        arguments
                        timeSpan
                        Process.ProcessStartFlags.RedirectStandardError

        if r.ProcessExited && r.ExecutionTime < timeSpan then
            printfn "Process exited (as expected)"
            let l = r.StandardError.Length 
            if l <> expectedErrorLength then
                failwithf "Standard error should be %d characters long, received %d (now %d)" expectedErrorLength l r.StandardError.Length
            else
                printfn "Found at least %d output chars in stderr as expected" expectedErrorLength
        elif r.ProcessExited then
            failwithf "Process exited but it took longer than expected. Time it took: %f ms, expected: %f ms" r.ExecutionTime.TotalMilliseconds timeSpan.TotalMilliseconds
        else
            failwith "Timed out waiting for the process "


    let processAsyncAuxMultiLineStandardOutput() =
        let standardOutLineCount = 10
        let cmd = "cmd", "/c \"@echo off & for /l %x in (1, 1, 10) do echo Processed: %x/5000\""
        let timeSpan = System.TimeSpan.FromSeconds 20.0

        let r = processAsyncAuxTest
                        (fst cmd)
                        (snd cmd)
                        timeSpan
                        Process.ProcessStartFlags.RedirectStandardOutput

        if r.ProcessExited && r.ExecutionTime < timeSpan then
            printfn "Process exited (as expected)"
            let outputLines = r.StandardOutput.Split [|'\n'|]
            if outputLines.Length < standardOutLineCount then
                failwithf "Standard output should have at least %d lines" standardOutLineCount
            else
                printfn "Found at least %d lines as expected" standardOutLineCount
        elif r.ProcessExited then
            failwithf "Process exited but it took longer than expected. Time it took: %f ms, expected: %f ms" r.ExecutionTime.TotalMilliseconds timeSpan.TotalMilliseconds
        else
            failwith "Timed out waiting for the process "



[<TestClass>]
type ProcessTests() =

    [<ClassInitialize>]
    static member init(context : TestContext) =
        ()

    [<TestMethod>]
    [<Description("Test 'StartProcess' Utility function with stdout redirected")>]
    [<TestCategory("Utilities")>]
    member this.TimeoutWhenWaitingForProcessAsyncAuxTimeoutStdOut() =
        processAsyncAuxTimeoutStdOut()
        processAsyncAuxTimeoutStdErr()
        processAsyncAuxTimeoutStdOutAndStdErr()

    [<TestMethod>]
    [<Description("Test 'StartProcess' Utility function with stderr redirected")>]
    [<TestCategory("Utilities")>]
    member this.TimeoutWhenWaitingForProcessAsyncAuxTimeoutStdErr() =
        processAsyncAuxTimeoutStdErr()

    [<TestMethod>]
    [<Description("Test 'StartProcess' Utility function with stdout and stderr redirected")>]
    [<TestCategory("Utilities")>]
    member this.TimeoutWhenWaitingForProcessAsyncAuxTimeoutStdOutAndStdErr() =
        processAsyncAuxTimeoutStdOutAndStdErr()

    [<TestMethod>]
    [<Description("Test 'StartProcess' Utility function")>]
    [<TestCategory("Utilities")>]
    member this.ProcessExitedAsyncAux() =
        processAsyncAuxExit()

    [<TestMethod>]
    [<Description("Test 'StartProcess' Utility function with large standard output in child process")>]
    [<TestCategory("Utilities")>]
    member this.ProcessAsyncAuxLargeStandardOutput() =
        processAsyncAuxLargeStandardOutput()

    [<TestMethod>]
    [<Description("Test 'StartProcess' Utility function with large standard error in child process")>]
    [<TestCategory("Utilities")>]
    member this.ProcessAsyncAuxLargeStandardError() =
        processAsyncAuxLargeStandardError()

    [<TestMethod>]
    [<Description("Test 'StartProcess' Utility function with large standard output in child process")>]
    [<TestCategory("Utilities")>]
    member this.ProcessAsyncAuxMultiLineStandardOutput() =
        processAsyncAuxMultiLineStandardOutput()

    [<TestMethod>]
    [<Description("No hang in logic to await process exit")>]
    [<TestCategory("Utilities")>]
    member this.NoHangInStartProcessLogic() =
        let attemptToHang i = async {
            let timeoutInMilliseconds = 3000
            let command = "ping"
            let arguments = "-?"
            let workingDir = ""
            use instance = Process.createProcessInstance command arguments workingDir Process.ProcessStartFlags.Elevated
            let timer = System.Diagnostics.Stopwatch()
            
            timer.Start()
            let eventFired = System.Diagnostics.Stopwatch()
            
            let mutable eventTriggered = false
            use instanceExit = new System.Threading.AutoResetEvent(false)
            instance.Exited.Add
                (fun _ ->
                    eventTriggered <- true
                    eventFired.Start()
                    instanceExit.Set() |> ignore
                    try
                        printfn "[%d:Exit Event] Process execution terminated in %O with exit code 0x%X: '%O %O'" i timer.Elapsed (int32 instance.ExitCode) command arguments
                    with :? System.InvalidOperationException ->
                        printfn "[%d:Exit Event] Process execution terminated abruptly in %O with no exit code: '%O %O'" i timer.Elapsed command arguments
                        )

            #if BUGGY_WAITPROCESSEXIT
            // Keeping the old buggy version to make sure it never 
            // gets reintroduced in the future.
            // For some unexplained reason the Exited event may not
            // always signal Async.AwaitEvent, even though the
            // event added via instance.Exited.Add does get fired!
            let! waitAsync = 
                let waitEvent = Async.AwaitEvent(instance.Exited)
                Async.StartChild(waitEvent, timeoutInMilliseconds) 
            #else
            // working implementation  (passing the test)
            let waitAsync = Async.AwaitWaitHandle(instanceExit, timeoutInMilliseconds)
            #endif

            if not (instance.Start()) then
                failwithf "[%d] Could not start command: '%s' with parameters '%s'" i command arguments
            else
                try
                    let! _ = waitAsync
                    printfn "waitAsync completed"
                with 
                | :? System.TimeoutException -> 
                    if eventTriggered then
                        failwithf "The wait timed out %O after the exit event fired!" eventFired.Elapsed
                    else
                        failwith "Test inconclusive: Exit event has not fired, need to increase timeout in the unit test"

                printfn "[%d] %s %s exited with code: %d" i command arguments instance.ExitCode

            Assert.IsTrue ((instance.ExitCode = 0), "Process should return error code 0")
            
        }
        // depending on the machine CPU, 
        //a higher number may be needed to repro
        let processCount = 50
        seq { 1..processCount } 
        |> Seq.map attemptToHang
        |> Async.Parallel
        |> Async.Ignore
        |> Async.RunSynchronously
        

