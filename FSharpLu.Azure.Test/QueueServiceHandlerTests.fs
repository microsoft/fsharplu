module Microsoft.FSharpLu.QueueServiceHandler.Test

open Microsoft.FSharpLu.Logging
open Microsoft.FSharpLu.StateMachineAgent
open Microsoft.FSharpLu.StateMachineAgent.QueueScheduler
open Microsoft.FSharpLu.StateMachineAgent.Agent
open Xunit

/// A request with input of type 'i and state 's
type RequestOf<'i, 's> =
    {
        header :
            {|
                correlationId : string
            |}
        input : 'i
        state : 's
        metadata : RequestMetadata
    }

/// Context parameters passed to every request handler
[<NoEquality;NoComparison>]
type HandlerContext<'QueueMessage, 'Request> =
    {
        contextName : string
        joinStore : Join.StorageInterface<'Request>
        queue : QueueingAPI<'QueueMessage, 'Request>
        queuedMessage : 'QueueMessage
    }

let createRequestContext<'QueueMessage, 'Request>
    joinStore
    (queue:QueueingAPI<'QueueMessage, 'Request>)
    message
    =
    {
        HandlerContext.contextName = "bla"
        joinStore = joinStore
        queue = queue
        queuedMessage = message
    }

let toScheduler (c:HandlerContext<'QueueMessage, 'Request>) embed : Agent.Scheduler<_, _> =
    {
        spawn = c.queue.post
        spawnIn = c.queue.postIn
        persist = c.queue.update c.queuedMessage
        embed = embed
        joinStore = c.joinStore
    }

/// Wrapper for Agent.execute to be used in request handlers
let inline run
        title
        (context: HandlerContext<'QueueMessage, ^Request>)
        (request: RequestOf<'Input,'State>)
        (embed: RequestMetadata -> 'State -> ^Request)
        (transition: HandlerContext<'QueueMessage, ^Request> -> 'State -> Async<Transition<'State, 'Result, ^Request>>)
    : Async<Agent.ExecutionInstruction< ^Request, _>>
    =
    Agent.execute request.state request.metadata {
        Agent.title = title
        logger = Microsoft.FSharpLu.Logging.TraceTags.info
        tags = ["foo", "bar"]
        transition = transition context
        maximumExpectedStateTransitionTime = System.TimeSpan.FromMinutes(15.0)
        maximumInprocessSleep = System.TimeSpan.FromMinutes(5.0)
        scheduler = toScheduler context embed
    }

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
    | Fibo of RequestOf<{| i : int; requestQuota : int |}, FiboStates>
    | FlipCoin of RequestOf<int, FlipCoinStates>
    | Request3
    | Shutdown

    type ServiceQueues =
        | ImportantQueue
        | NotImportantQueue

    let request1TestOutcome = System.Collections.Concurrent.ConcurrentDictionary<int, int>()

    let request1Handler context request =
        run "request1 handler" context request
            (fun metadata newState -> ServiceRequests.Fibo({ request with state = newState; metadata = metadata }))
            (fun context ->
                function
                | FiboStates.Start -> async {
                    printfn "Received request"
                    return Goto <| FiboStates.Calculate (request.input.i, 0, 1)
                  }
                | FiboStates.Calculate (i, fibminus_1, fib) ->  async {
                    if i > 1 then
                        return SleepAndGoto (System.TimeSpan.FromMilliseconds(10.0),
                                                FiboStates.Calculate(i-1, fib, fibminus_1 + fib))
                    else
                        return Goto <| FiboStates.End (fib)
                  }
                | FiboStates.End (result) -> async {
                    if not <| request1TestOutcome.TryAdd(request.input.i, result) then
                        failwith "Result overwrite!"
                    if request1TestOutcome.Count = request.input.requestQuota then
                        do! context.queue.post ServiceRequests.Shutdown
                    return Transition.Return (result)
                  })

    let request2Handler context request =
        run "request2 handler" context request
            (fun metadata newState -> ServiceRequests.FlipCoin({ request with state = newState; metadata = metadata }))
            (fun context ->
                function
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
            SleepDurationWhenAllQueuesAreEmpty = System.TimeSpan.FromMilliseconds(10.0)
            HeartBeatIntervals = System.TimeSpan.FromSeconds(1.0)
            ConcurrentRequestWorkers = 10
            WorkerReplacementTimeout = System.TimeSpan.FromHours(1.0)
        }

    let queuesHandlersOrderedByPriority<'QueueMessage> (cts:System.Threading.CancellationTokenSource)
            :QueueProcessor<ServiceQueues,ServiceRequests,HandlerContext<'QueueMessage, ServiceRequests>> list =

         let dispatch (c:HandlerContext<'QueueMessage, ServiceRequests>) (k:Continuation<ServiceRequests>) =
              function
              | ServiceRequests.Fibo r -> k.k (request1Handler c r)
              | ServiceRequests.FlipCoin r -> k.k (request2Handler c r)
              | ServiceRequests.Shutdown -> k.k (async {
                                                      Trace.info "Shutdown request received"
                                                      cts.Cancel()
                                                      Trace.info "Token cancelled"
                                                      return ExecutionInstruction.Completed None
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

module InMemorySchedulerTest =
    open Example

    let newInMemoryJoinStorage () : Agent.Join.StorageInterface<'m> =
        let storage = System.Collections.Concurrent.ConcurrentDictionary<_, _>()
        {
            add = fun joinId joinEntry -> async {
                        if not <| storage.TryAdd(joinId, joinEntry) then
                            failwithf "Add: Failed to add %A" (joinId, joinEntry)
                    }
            update = fun joinId performEntryUpdate ->
                        lock(storage) (fun () ->
                            async {
                                match storage.TryGetValue(joinId) with
                                | true, entry ->
                                    let newEntry = performEntryUpdate entry
                                    storage.[joinId] <- newEntry
                                    return newEntry
                                | false, _ -> return failwithf "Update: Failed to retrieve data with id: %A" joinId
                        }
                    )
            get = fun joinId ->
                    async {
                        match storage.TryGetValue(joinId) with
                        | false, _ -> return failwithf "Get: There is no request with id : %A" joinId
                        | true, entry -> return entry
                    }

        }

    let joinStore = newInMemoryJoinStorage ()

    let queueProcessLoop
            (cts:System.Threading.CancellationTokenSource)
            (queues:Map<Example.ServiceQueues,_>)=
        async {
            try
                let context request = createRequestContext<System.Guid, Example.ServiceRequests> joinStore request
                do! QueueScheduler.processingLoopMultipleQueues<Example.ServiceQueues, Example.ServiceRequests, _, System.Guid>
                        Example.QueuesProcessingOptions
                        (Example.queuesHandlersOrderedByPriority cts)
                        (fun queueId -> Map.find queueId queues)
                        context
                        ignore // no heartbeat
                        cts.Token
                        (fun _ -> []) // no tags
                        {new OutcomeLogger with member __.log _ = ()} // no logger
            with
            | QueueScheduler.ProcessingLoopCancelled ->
                Trace.warning "Processing loop terminated by Azure."
        }


    [<Fact>]
    let ``InMemoryScheduler Fibonnaci Test`` () =
        async {
            let importantQueue = InMemoryQueue.newQueue "inmemory" ImportantQueue
            let nonImportantQueue = InMemoryQueue.newQueue "inmemory" NotImportantQueue
            let queues =
                [
                    ImportantQueue, importantQueue
                    NotImportantQueue, nonImportantQueue
                ] |> Map.ofSeq

            let shutdownSource = new System.Threading.CancellationTokenSource()

            let fibonnaci = [|0;1;1;2;3;5;8;13;21;34;55;89;144|]

            for i = 1 to fibonnaci.Length-1 do
                let! req = Agent.createRequest joinStore
                do! importantQueue.post
                    <| ServiceRequests.Fibo(
                        {
                           header = {| correlationId = "Fibonnaci" |}
                           input = {| i = i; requestQuota = fibonnaci.Length-1|}
                           state = FiboStates.Start
                           metadata = req
                       })

            do! queueProcessLoop shutdownSource queues

            for kvp in request1TestOutcome do
                let i = kvp.Key
                Assert.Equal(request1TestOutcome.[i], fibonnaci.[i])

        }

module AzureQueueSchedulerTest =
    open Microsoft.Azure.Cosmos
    open Microsoft.Azure.Storage.Queue
    open Microsoft.FSharpLu

    let JoinTableName = "QueueServiceTest-AgentJoinStorageTable"
    let QueueNamePrefix  = "QueueServiceTest"

    let queueProcessLoop
        (terminationRequested:System.Threading.CancellationToken)
        (storageAccountConnectionString:string) =
        async {
            let! joinStore =
                AzureTableJoinStorage.newStorage
                                        (Table.CloudStorageAccount.Parse(storageAccountConnectionString))
                                        JoinTableName
                                        (System.TimeSpan.FromSeconds(1.0))
                                        (System.TimeSpan.FromSeconds(10.0))
                                        "testAgent"
            let azureStorage = Azure.Context.getStorageAccountFromConnectionString storageAccountConnectionString
            let context request = createRequestContext<CloudQueueMessage, Example.ServiceRequests> joinStore request
            let shutdownSource = new System.Threading.CancellationTokenSource()
            try
                do! QueueScheduler.processingLoopMultipleQueues<Example.ServiceQueues, Example.ServiceRequests, _, CloudQueueMessage>
                        Example.QueuesProcessingOptions
                        (Example.queuesHandlersOrderedByPriority shutdownSource)
                        (AzureQueue.newQueue azureStorage QueueNamePrefix)
                        context
                        ignore // no heartbeat
                        terminationRequested
                        (fun _ -> []) // no tags
                        {new OutcomeLogger with member __.log _ = ()} // no logger
            with
            | QueueScheduler.ProcessingLoopCancelled ->
                Trace.warning "Processing loop terminated by Azure."

        }
