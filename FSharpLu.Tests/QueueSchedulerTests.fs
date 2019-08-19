module Microsoft.FSharpLu.Actor.QueueScheduler.Test

open Xunit
open Microsoft.FSharpLu.Logging
open Microsoft.FSharpLu.Actor.StateMachine
open Microsoft.FSharpLu.Actor.StateMachine.Agent
open Microsoft.FSharpLu.Actor.QueueScheduler
open Microsoft.FSharpLu.Actor.ServiceRequests
open Microsoft.FSharpLu.Actor
open System.Collections.Concurrent

module Example =
    type FiboStates =
    | Start
    | Calculate of int * int * int
    | End of int

    type FlipCoinStates =
    | Start
    | Flip
    | End

    /// All supported requests
    type ServiceRequests =
    | Fibo of StatefulRequest<{| i : int; requestQuota : int |}, FiboStates>
    | FlipCoin of StatefulRequest<int, FlipCoinStates>
    | Request3
    | Shutdown

    type ServiceQueues =
    | ImportantQueue
    | NotImportantQueue

    type Header = {| correlationId : string |}

    [<NoEquality;NoComparison>]
    type CustomContext =
        {
            contextName : string
        }

    let request1Handler<'QueueMessage> (request1TestOutcome:ConcurrentDictionary<int, int>) : Handler<'QueueMessage, _, _, _, _> =
        run "request1 handler" ["foo", "bar"] ServiceRequests.Fibo
            (fun operations input -> function
            | FiboStates.Start -> async {
                printfn "Received request"
                return Goto <| FiboStates.Calculate (input.i, 0, 1)
                }
            | FiboStates.Calculate (i, fibminus_1, fib) ->  async {
                if i = 0 then
                    return Goto <| FiboStates.End (fibminus_1)
                else if i = 1 then
                    return Goto <| FiboStates.End (fib)
                else
                    return SleepAndGoto (System.TimeSpan.FromMilliseconds(10.0),
                                            FiboStates.Calculate(i-1, fib, fibminus_1 + fib))
                }
            | FiboStates.End (result) -> async {
                if not <| request1TestOutcome.TryAdd(input.i, result) then
                    failwith "Result overwrite!"
                if request1TestOutcome.Count >= input.requestQuota then
                    do! operations.spawnNewRequest ServiceRequests.Shutdown
                return Transition.Return (result)
                })

    let request2Handler<'QueueMessage> : Handler<'QueueMessage, _, _, _, _> =
        run "request2 handler" ["foo", "bar"]
            ServiceRequests.FlipCoin
            (fun operations input -> function
            | FlipCoinStates.Start -> async.Return <| Goto FlipCoinStates.Flip
            | FlipCoinStates.Flip -> async {
                let r = System.Random(System.DateTime.UtcNow.Ticks |> int)
                if r.Next() % 2 = 0 then
                    return Sleep <| System.TimeSpan.FromMilliseconds(10.0 * (float <| r.Next()))
                else
                    return Goto <| FlipCoinStates.End
                }
            | FlipCoinStates.End ->
                async.Return <| Transition.Return ()
            )

    let QueuesProcessingOptions =
        {
            SleepDurationWhenAllQueuesAreEmpty = System.TimeSpan.FromMilliseconds(0.0)
            HeartBeatIntervals = System.TimeSpan.FromSeconds(1.0)
            ConcurrentRequestWorkers = 100
            WorkerReplacementTimeout = System.TimeSpan.FromHours(1.0)
        }

    let queuesHandlersOrderedByPriority<'QueueMessage>
            (request1TestOutcome:ConcurrentDictionary<int, int>)
            (schedulerFactory: ISchedulerFactory<'QueueMessage, Header, ServiceRequests, CustomContext>)
            (cts:System.Threading.CancellationTokenSource)
                :QueueProcessor<ServiceQueues,
                                Envelope<Header, ServiceRequests>,
                                QueueingContext<'QueueMessage, Header, ServiceRequests, CustomContext>> list =

         let dispatch (c:QueueingContext<'QueueMessage, Header, ServiceRequests, CustomContext>)
                      (k:IContinuation<Envelope<Header, ServiceRequests>>)
                      envelope =
              match envelope.request with
              | ServiceRequests.Fibo _ -> k.k (request1Handler request1TestOutcome schedulerFactory c envelope)
              | ServiceRequests.FlipCoin _ -> k.k (request2Handler schedulerFactory c envelope)
              | ServiceRequests.Shutdown -> k.k (async {
                                                      Trace.info "Shutdown request received"
                                                      cts.Cancel()
                                                      Trace.info "Token cancelled"
                                                      return RequestStatus.Completed None
                                                })
              | _ -> raise RejectedMessage

         [
            {
                queueId = ServiceQueues.ImportantQueue
                handler = dispatch
                maxProcessTime = System.TimeSpan.FromMinutes(1.0)
                messageBatchSize = 10
            }
            {
                queueId = ServiceQueues.NotImportantQueue
                handler = dispatch
                maxProcessTime = System.TimeSpan.FromMinutes(1.0)
                messageBatchSize = 32
            }
        ]

    let startFibonacciTest (queue:IQueueingAPI<_,_>) =
        async {
            let request1TestOutcome = ConcurrentDictionary<int, int>()
            let fibonnaci = [|0;1;1;2;3;5;8;13;21;34;55;89;144|]

            for i = 0 to fibonnaci.Length-1 do
                do! queue.post
                        {
                            metadata = None
                            header = {| correlationId = "Fibonnaci" |}
                            calleeReturnResult = None
                            request = ServiceRequests.Fibo {
                                input = {| i = i; requestQuota = fibonnaci.Length-1 |}
                                state = FiboStates.Start
                            }
                        }
            return request1TestOutcome, fibonnaci
        }

    let endFibonacciTest (request1TestOutcome:ConcurrentDictionary<int, int>, expected:int[]) = 
        async {
            for kvp in request1TestOutcome do
                let i = kvp.Key
                Assert.Equal(request1TestOutcome.[i], expected.[i])
            }

module InMemorySchedulerTest =
    open Example

    let queueProcessLoop (request1TestOutcome, _) (queues:Map<Example.ServiceQueues,_>)=
        async {
            try
                let joinStore = InMemory.newJoinStorage()
                let shutdownSource = new System.Threading.CancellationTokenSource()
                do! QueueScheduler.processingLoopMultipleQueues<Example.ServiceQueues, Envelope<Header, Example.ServiceRequests>, _, System.Guid>
                        (Microsoft.FSharpLu.Logging.Interfaces.fromTraceTag<System.Diagnostics.TagsTracer>)
                        Example.QueuesProcessingOptions
                        (Example.queuesHandlersOrderedByPriority request1TestOutcome (InMemorySchedulerFactory()) shutdownSource)
                        (fun queueId -> Map.find queueId queues)
                        (fun queue message -> {
                            queue = queue
                            queuedMessage = message
                            joinStore = joinStore
                            customContext = { contextName = "bla" }
                        })
                        ignore // no heartbeat
                        shutdownSource.Token
                        (fun _ -> []) // no tags
                        {new IOutcomeLogger<_, _> with member __.log _ = ()} // no logger
            with
            | ProcessingLoopCancelled ->
                Trace.warning "Processing loop terminated by Azure."
        }

    [<Fact>]
    let ``InMemoryScheduler Fibonnaci Test`` () =
        async {
            let queues =
                [
                    ImportantQueue, InMemoryQueue.InMemoryQueueProcessor("inmemory", ImportantQueue) :> IQueueingAPI<_,_>
                    NotImportantQueue, InMemoryQueue.InMemoryQueueProcessor("inmemory", NotImportantQueue) :> IQueueingAPI<_,_>
                ] |> Map.ofSeq

            let! testState = startFibonacciTest queues.[ImportantQueue]

            do! queueProcessLoop testState queues

            do! endFibonacciTest testState
        }