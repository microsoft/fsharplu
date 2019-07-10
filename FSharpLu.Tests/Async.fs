namespace Microsoft.FSharpLu

open System.Threading
open Microsoft.FSharpLu
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type AsyncTest() =
    
    [<TestMethod>]
    [<Description("Competing worfklows should terminate in the expected order")>]
    member __.CompeteOnSleep () =
        async {
            let tasks = 
                [  async { do! Async.Sleep(100)
                           printfn "Task1 done" }
                   async { do! Async.Sleep(5000)
                           Assert.Fail("Task2 finished first") }
                ] |> Seq.ofList
            do! Async.Compete tasks
            printfn "Done"
        }
        |> Async.RunSynchronously

    [<TestMethod>]
    [<Description("Cancelling the parent worfklow should cancel competing worfklows started with Async.Compete")>]
    member __.CompeteCancellable () =
        let task1Cancelled = ref false
        let task2Cancelled = ref false
        let compete =
            let jobs =
                [ async {
                        try
                            do! Async.Sleep(-1)
                            Assert.Fail("task1 finished")
                        finally
                            task1Cancelled := true
                  }
                  async {
                      try
                          do! Async.Sleep(-1)
                          Assert.Fail("task2 finished")
                      finally
                          task2Cancelled := true
                  }
                ] |> Seq.ofList
            Async.Compete jobs
        
        let main = async {
            use cts = new CancellationTokenSource()
            Async.Start(compete, cts.Token)
            do! Async.Sleep(100)
            cts.Cancel()
            do! Async.Sleep(100)
        }
        Async.RunSynchronously main
        Assert.IsTrue !task1Cancelled
        Assert.IsTrue !task2Cancelled

    [<TestMethod>]
    [<Description("Async created with Async.CompeteWithThreadingObject returns expected result when the threading object is acquired")>]
    member __.``CompeteWithThreadingObject when semaphore wins``() =
        let t1 = async {
                    do! Async.Sleep(1000)
                    printfn "t1 done"
                    Assert.Fail()
                }
        use s = new SemaphoreSlim(1)
        s.Wait()
        let main =
             async {
                let compete = Async.CompeteWithThreadingObject t1 s
                let! c = Async.StartChild(compete)
                do! Async.Sleep(100)
                printfn "Releasing"
                let z = s.Release()
                printfn "semaphore released. %d" z
                let! r = c
                match r with 
                | Choice1Of3 _
                | Choice3Of3 _ -> Assert.Fail("Unexpected result")
                | Choice2Of3 y -> y.Dispose()
            }
        Async.RunSynchronously main

    [<TestMethod>]
    [<Description("Async created with Async.CompeteWithThreadingObject returns expected result when the threading object is acquired")>]
    member __.``CompeteWithThreadingObject when workflow wins``() =
        let t1 = async {
                    do! Async.Sleep(1000)
                    printfn "t1 done"
                    return 7
                }
        use s = new SemaphoreSlim(0)
        let main =
             async {
                let compete = Async.CompeteWithThreadingObject t1 s
                let! r = compete
                match r with 
                | Choice2Of3 _
                | Choice3Of3 _ -> Assert.Fail("Unexpected result")
                | Choice1Of3 y -> Assert.AreEqual(y, 7)
            }
        Async.RunSynchronously main

    [<TestMethod>]
    [<Description("It should be possible to cancel a workflow created with Async.CompeteWithThreadingObject and the underlying computations should also be cancelled")>]
    member __.``CompeteWithThreadingObject propagates cancellation`` () =
        let t1 = async {
                    do! Async.Sleep(1000)
                    printfn "t1 done"
                    Assert.Fail()
                }
        use s = new SemaphoreSlim(1)
        s.Wait()
        let taskCancelled = ref false
        let compete =
            async {
                try
                    let! newEvent = Async.CompeteWithThreadingObject t1 s
                    printfn "compete done"
                    Assert.Fail("compting workflow should not complete")
                finally
                    taskCancelled := true
            }
        use cts = new CancellationTokenSource()
        Async.Start(compete, cts.Token)
        let main =
             async {
                do! Async.Sleep(100)
                printfn "cancelling"
                cts.Cancel()
                do! Async.Sleep(100)
            }
        Async.RunSynchronously main
        Assert.IsTrue(!taskCancelled)