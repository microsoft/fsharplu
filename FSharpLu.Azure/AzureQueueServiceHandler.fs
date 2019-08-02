// Copyright (c) Microsoft Corporation.
namespace Microsoft.FSharpLu.StateMachineAgent

/// A Request Processor with scheduling and persistence implemented using some queueing API
module QueueScheduler =

    open Microsoft.FSharpLu
    open Microsoft.FSharpLu.Azure.AppInsights
    open Microsoft.FSharpLu.StateMachineAgent

    /// Queue interface implemented by the underlying queueing infrastructure
    /// (e.g. Azure Queue, mailbox processor, ConcurrentQueue, ...)
    type QueueingAPI<'QueueMessage, 'QueueContent> =
        {
            /// queue name
            queueName : string
            /// update content and visibility of a queue message
            update : 'QueueMessage -> 'QueueContent -> System.TimeSpan -> Async<unit>
            /// update visibility of a queue message
            updateVisibility : 'QueueMessage -> System.TimeSpan -> Async<unit>
            /// delete a queue message
            delete : 'QueueMessage -> Async<unit>
            /// serialize a queue message to string
            serialize : 'QueueMessage -> string
            /// deserialize a string to a queue message content
            tryDeserialize : string -> Result<'QueueContent, string>
            /// get queue message insertion time
            insertionTime : 'QueueMessage -> System.DateTimeOffset
            /// try to pop specified number of messages from the queue
            tryGetMessageBatch : int -> System.TimeSpan -> Async<'QueueMessage list>
            /// post a new message onto the queue
            post : 'QueueContent -> Async<unit>
            /// post a new message onto the queue with the specified visibility
            postIn : 'QueueContent -> System.TimeSpan -> Async<unit>
        }

    /// Info returned for a request successfully processed
    /// 'Request is the type of request
    /// 'Result is the request result type
    /// 'QueueMessage type representing a queue message by the underlying queueing API
    type ProcessedRequestInfo<'Request, 'Result, 'QueueMessage> =
        {
            /// status of the request execution
            executionStatus : Agent.ExecutionInstruction<'Request,'Result>
            /// the request, if it was properly parsed
            request : 'Request
            /// date/time when the request was posted on the queu
            requestInsertionTime : System.DateTime
            /// Time taken to process the request
            processingTime : System.TimeSpan
            /// queue message object from the underlying queue API
            queuedMessage : 'QueueMessage
        }

    /// Info returned for a request that could not be processed
    /// 'Request is the type of request
    /// 'QueueMessage type representing a queue message by the underlying queueing API
    type FailedRequestInfo<'Request, 'QueueMessage> =
        {
            /// A description of the error that occurred
            errorMessage : string
            /// More details about the error
            details : string
            /// processed request
            request : 'Request option
            /// date/time when the request was posted on the queu
            requestInsertionTime : System.DateTime
            /// Time taken to process the request
            processingTime : System.TimeSpan
            /// queue message object from the underlying queue API
            queuedMessage : 'QueueMessage
        }

    /// Outcome of the processing of a request
    /// 'Request is the type of request
    /// 'Result is the request result type
    /// 'QueueMessage type representing a queue message by the underlying queueing API
    type RequestOutcome<'Request, 'Result, 'QueueMessage> =
    | Processed of ProcessedRequestInfo<'Request, 'Result, 'QueueMessage>
    | Error of FailedRequestInfo<'Request, 'QueueMessage>
    | Rejected of FailedRequestInfo<'Request, 'QueueMessage>
    | ExceptionThrown of System.Exception * FailedRequestInfo<'Request, 'QueueMessage>

    /// A continuation that handles processing of an agent execution status.
    /// Defining this as a class type rather than let-binding is a "trick"
    /// to prevent the F# typecheker to unify the ghost generic type type parameter 'T
    /// against the first occurrence of k.
    /// This means that Continuation.k can be used in the same function
    /// for different instances of the type parameter 'T.
    type Continuation<'Request> =
        abstract k<'Request, 'Result> : Async<Agent.ExecutionInstruction<'Request, 'Result>> -> Async<unit>

    /// Loggger interface for request outcome
    type OutcomeLogger<'Request, 'QueueMessage> =
        abstract log : RequestOutcome<'Request, 'Result, 'QueueMessage> -> unit

    /// Handler for queued requests
    /// 'Request is the type of queued request
    /// 'Context is a custom context parameter that will get passed to every request handler
    type Handler<'Context, 'Request> = 'Context -> Continuation<'Request> -> 'Request -> Async<unit>

    /// Defines a processor consuming queued request.
    /// 'QId defines an enumeration of possible queue Ids
    /// 'R is the type of requests handled by the processor
    /// 'C is a custom context parameter that will get passed to every request handler
    [<NoEquality;NoComparison>]
    type QueueProcessor<'QId, 'Request, 'Context> =
        {
            /// Name of the queue
            queueId : 'QId

            /// Function handling a request received on a Queue
            handler : Handler<'Context, 'Request>

            /// Initial value of the maximum expected processing time.
            ///
            /// Note for Azure queues: After this timeout expires Azure automatically reposts the message onto the queue.
            /// We want this value to be high enough to cover requests with long processing time (e.g. creating a VM in Azure)
            /// but not too high to avoid delays in case a real backend failure occurs.
            ///
            /// A request handler can dynamically override this value if more time is needed to process a request.
            /// State machine agents (QueueRequestHandling module) facilitate this by automatically updating the expected
            /// processing time on each transition of the state machine.
            maxProcessTime : System.TimeSpan

            /// Number of messages to pull at once from the a queue
            ///
            /// For Azure Queues, keep this number low for maxium utilization of concurrency between worker role instances
            /// Note: maximum allowed is 32 (if greater you'll get a warning followed by an obscure exception
            /// from Azure, See https://msdn.microsoft.com/en-us/library/azure/dd179474.aspx)
            messageBatchSize : int
        }

    /// Queue request processor options
    type Options =
        {
            /// Amount of time to sleep when all request queues are empty
            SleepDurationWhenAllQueuesAreEmpty : System.TimeSpan
            /// Time interval after which a new heartbeat even may be sent
            HeartBeatIntervals : System.TimeSpan
            /// Number of requests processed concurrently at the same time
            ConcurrentRequestWorkers : int
            /// Timeout period when waiting for an available processor from the pool.
            /// After this time, it is assumed that at least one processor is dead
            /// and a new one is allocated.
            WorkerReplacementTimeout : System.TimeSpan
        }

    /// Exception raised by message handlers when a message is rejected
    exception RejectedMessage

    /// Process a single request from a Queue
    let processRequest<'Context, 'Request, 'QueueMessage>
            (context:'Context)
            (queueSystem:QueueingAPI<'QueueMessage, 'Request>)
            (queuedMessage:'QueueMessage)
            (handler:Handler<'Context, 'Request>)
            (getTags:'Request -> (string*string) list)
            (logger: OutcomeLogger<'Request, 'QueueMessage>) =
        async {

            let requestInsertionTime =
                (queueSystem.insertionTime queuedMessage).UtcDateTime

            let queueMessageAsString = queueSystem.serialize queuedMessage
            let parseResult = queueSystem.tryDeserialize queueMessageAsString

            match parseResult with
            | Result.Error deserializationError ->
                logger.log <|
                    RequestOutcome.Error
                        {
                            errorMessage = "Could not parse queued message"
                            details = deserializationError
                            request = None
                            requestInsertionTime = requestInsertionTime
                            processingTime = System.TimeSpan.Zero
                            queuedMessage = queuedMessage
                        }
                do! queueSystem.delete queuedMessage

            | Result.Ok parsedMessage ->
                let tags = getTags parsedMessage
                TraceTags.info "Processing queued message"
                                 (tags @ ["parsedMessage", (sprintf "%A" parsedMessage)])

                let processingTime = System.Diagnostics.Stopwatch()
                try
                    processingTime.Start()

                    do! handler context
                            { new Continuation<'Request> with
                              member __.k executionStatusAsync =
                                async {
                                    // Schedule the remaining execution of the specified queued request
                                    let! executionStatus = executionStatusAsync
                                    processingTime.Stop()
                                    match executionStatus with
                                    | Agent.ExecutionInstruction.Completed _ ->
                                        do! queueSystem.delete queuedMessage
                                    | Agent.ExecutionInstruction.SleepAndResume visibilityTimeout ->
                                        do! queueSystem.updateVisibility queuedMessage visibilityTimeout
                                    | Agent.ExecutionInstruction.SleepAndResumeAt (visibilityTimeout, newRequest) ->
                                        do! queueSystem.update queuedMessage newRequest visibilityTimeout

                                    logger.log <|
                                        RequestOutcome.Processed
                                            {
                                                executionStatus = executionStatus
                                                request = parsedMessage
                                                requestInsertionTime = requestInsertionTime
                                                processingTime = processingTime.Elapsed
                                                queuedMessage = queuedMessage
                                            }
                                }
                            }
                            parsedMessage
                with
                | RejectedMessage ->
                    processingTime.Stop()
                    logger.log <|
                        RequestOutcome.Rejected
                            {
                                errorMessage = "This request type was rejected from queue handler"
                                details = sprintf "Queue name: %s" queueSystem.queueName
                                request = Some parsedMessage
                                requestInsertionTime = requestInsertionTime
                                processingTime = processingTime.Elapsed
                                queuedMessage = queuedMessage
                            }
                    do! queueSystem.delete queuedMessage
                | e ->
                    processingTime.Stop()
                    let elapsed = processingTime.Elapsed
                    TraceTags.trackException e (tags @[
                                                  "postedTime", requestInsertionTime.ToString()
                                                  "processingTime", elapsed.ToString()
                                                  "request", queueMessageAsString
                                                ])

                    logger.log <|
                        RequestOutcome.ExceptionThrown
                            (e,
                            {
                                errorMessage = "Exception raised while processing request"
                                details = sprintf "Exception: %O" e
                                request = Some parsedMessage
                                requestInsertionTime = requestInsertionTime
                                processingTime = processingTime.Elapsed
                                queuedMessage = queuedMessage
                            })
                    do! queueSystem.delete queuedMessage
        }

    exception ProcessingLoopCancelled

    /// A processing loop handling requests posted on multiple
    /// queues with different assigned priorities
    let processingLoopMultipleQueues<'QueueId, 'Request, 'Context, 'QueueMessage>
                (options:Options)
                (queueProcessorsOrderedByPriority: (QueueProcessor<'QueueId, 'Request, 'Context>) list)
                (getQueue:'QueueId -> QueueingAPI<_,_>)
                (createRequestContext : QueueingAPI<_,_> -> 'QueueMessage -> 'Context)
                (signalHeartBeat : unit -> unit)
                (terminationRequested: System.Threading.CancellationToken)
                getTags
                (logger:OutcomeLogger<'Request, 'QueueMessage>)
                =
        async {
            use processingPool = new Microsoft.FSharpLu.Async.Synchronization.Pool(options.ConcurrentRequestWorkers)
            let pool = processingPool :> Microsoft.FSharpLu.Async.Synchronization.IPool

            /// Find pending request from the queue with highest priority
            let rec findHighestPriorityRequests queues =
                async {
                    match queues with
                    | [] -> return None
                    | (queue, handling : QueueProcessor<'QueueId, 'Request, 'Context>)::rest ->
                        let! m = queue.tryGetMessageBatch handling.messageBatchSize handling.maxProcessTime
                        match m with
                        | [] -> return! findHighestPriorityRequests rest
                        | messages -> return Some (queue, handling, messages)
                }

            /// Process a batch of messages
            let processMessageBatch handler queue (queueMessageBatch:'QueueMessage list) =
                Async.Parallel
                    [
                        for queuedMessage in queueMessageBatch ->
                            async {
                                use! resourceAvailable =
                                    pool.AcquireAsync(Some(options.WorkerReplacementTimeout.TotalMilliseconds |> int))

                                if terminationRequested.IsCancellationRequested then
                                    raise ProcessingLoopCancelled
                                let context = createRequestContext queue queuedMessage
                                do! processRequest
                                        context
                                        queue
                                        queuedMessage
                                        handler
                                        getTags
                                        logger
                            }
                            |> Async.Catch // Individual request should be able to fail independently without taking down the entire batch
                    ]
                |> Async.Ignore


            let queuesOrderedByPriority =
                queueProcessorsOrderedByPriority
                |> Seq.map (fun handling ->
                        let queue = getQueue handling.queueId
                        Trace.info "Monitoring queue %s" queue.queueName
                        queue, handling)
                |> Seq.toList

            /// Outermost loop: process requests from each channel by priority
            /// Note: One drawback is that higher-priority queues may starve lower-priority ones
            let rec processChannels (heartBeatWatch:System.Diagnostics.Stopwatch) =
                async {
                    // Send heartbeat if sufficient time has passed since the last one
                    if heartBeatWatch.Elapsed > options.HeartBeatIntervals then
                        signalHeartBeat ()
                        heartBeatWatch.Restart()

                    if terminationRequested.IsCancellationRequested then
                        raise ProcessingLoopCancelled
                    else
                        let sleepDurationWhenAllQueuesAreEmptyInMs =
                            options.SleepDurationWhenAllQueuesAreEmpty.TotalMilliseconds |> int

                        let! highestRequest = findHighestPriorityRequests queuesOrderedByPriority
                        match highestRequest with
                        | None ->
                            // all channels are empty: we rest for a bit
                            do! Async.Sleep(sleepDurationWhenAllQueuesAreEmptyInMs)

                        | Some (queue, handling, firstMessageBatch) ->
                            do! processMessageBatch handling.handler queue firstMessageBatch

                        return! processChannels heartBeatWatch
                }


            let heartBeatWatch = System.Diagnostics.Stopwatch()
            heartBeatWatch.Start()
            do! processChannels heartBeatWatch
        }

    /// Deserialize a request
    let inline tryDeserializeJson< ^Request> queuedMessageAsString =
        match Microsoft.FSharpLu.Json.Compact.tryDeserialize< ^Request> queuedMessageAsString with
        | Choice1Of2 message -> Result.Ok message
        | Choice2Of2 error -> Result.Error error


/// In-memory queue system implemented with System.Collections.ConcurrentQueue
/// with not fault-tolerance (if the process crashes the queue content is lost)
/// and not visiblity timeout when consuming messages
module InMemoryQueue =
    type QueueEntry< ^QueueContent> =
        {
            insertionTime : System.DateTimeOffset
            content : ^QueueContent
            visible : bool
        }

    /// Create an instance of an queue processor based on mailbox processor
    let inline newQueue<'QueueId, ^QueueContent> queueNamePrefix (queueId:'QueueId)
           : QueueScheduler.QueueingAPI<System.Guid, ^QueueContent> =
        let queueName = sprintf "%s-%A" queueNamePrefix queueId
        let queue = new System.Collections.Concurrent.ConcurrentDictionary<System.Guid, QueueEntry< ^QueueContent>>()
        {
            queueName = queueName

            update = fun queuedRequest queueContent visibility -> async {
                    let newContent =
                        queue.AddOrUpdate(
                                queuedRequest,
                                (fun _ -> { insertionTime = System.DateTimeOffset.UtcNow
                                            content = queueContent
                                            visible = true }),
                                (fun _ c -> { c with content = queueContent }))
                    return ()
                }

            // no-op: visiblity timeout is infinite in this implementation
            updateVisibility = fun queuedRequest visibilityTimeout -> async.Return ()

            delete = fun queuedRequest -> async {
                    let success, _ = queue.TryRemove(queuedRequest)
                    if not success then
                        failwith "could not remove entry from queue"
                }

            insertionTime = fun queueMessage ->
                let success, message = queue.TryGetValue(queueMessage)
                if not success then
                    failwith "could not find queue entry"
                message.insertionTime

            serialize = fun queueMessage ->
                let success, message = queue.TryGetValue(queueMessage)
                if not success then
                    failwith "could not find queue entry"
                Microsoft.FSharpLu.Json.Compact.serialize message.content

            tryDeserialize = QueueScheduler.tryDeserializeJson< ^QueueContent>

            tryGetMessageBatch = fun batchSize _  -> async {
                    let nextBatch =
                        lock queue (fun () ->
                            queue :> seq<_>
                            |> Seq.where (fun m -> m.Value.visible)
                            |> Seq.sortBy (fun m -> m.Value.insertionTime)
                            |> Seq.map (fun c ->
                                if not <| queue.TryUpdate(c.Key, { c.Value with visible = false }, c.Value) then
                                    failwith "impossible: queue entry disappeard!"
                                c.Key
                            )
                            |> Seq.truncate batchSize
                            |> Seq.toList
                        )
                    Microsoft.FSharpLu.Logging.Trace.info "Batch: %A" nextBatch
                    return nextBatch
                }

            post = fun (content:^QueueContent) -> async {
                    let id = System.Guid.NewGuid()
                    let queueEntry =
                        { insertionTime = System.DateTimeOffset.UtcNow
                          content = content
                          visible = true
                        }
                    if not <| queue.TryAdd(id, queueEntry) then
                        failwith "impossible: guid collision"
            }

            postIn = fun (content:^QueueContent) delay -> async {
                    let id = System.Guid.NewGuid()
                    let queueEntry =
                        { insertionTime = System.DateTimeOffset.UtcNow
                          content = content
                          visible = true
                        }
                    let child = async {
                        do! Async.Sleep (int <| delay.TotalMilliseconds)
                        if not <| queue.TryAdd(id, queueEntry) then
                            failwith "impossible: guid collision"
                    }
                    let r = Async.StartChild child
                    return ()
            }
        }

/// Queue system implemented with Azure Queues
module AzureQueue =

    open Microsoft.Azure.Storage.Queue
    open QueueScheduler
    open Microsoft.FSharpLu.Azure.Queue
    open Microsoft.FSharpLu.Logging
    open Microsoft.Azure.Storage

    type ProcessedRequestInfo<'Request, 'Result> = ProcessedRequestInfo<'Request, 'Result, CloudQueueMessage>
    type FailedRequestInfo<'Request> =  FailedRequestInfo<'Request, CloudQueueMessage>

    /// Create an instance of an queue processor based on Azure Queue
    let inline newQueue<'QueueId, ^QueueContent> (azureStorage:CloudStorageAccount) queueNamePrefix (queueId:'QueueId)
        : QueueScheduler.QueueingAPI<CloudQueueMessage, ^QueueContent> =
        // Create the reference to the Azure queue
        let queueClient = azureStorage.CreateCloudQueueClient()
        let queueName = queueNamePrefix + "-" + ((sprintf "%A" queueId).ToLowerInvariant())
        let queue = queueClient.GetQueueReference(queueName)
        if not <| queue.CreateIfNotExists() then
            Trace.info "Queue %s was already created." queueName
        {
            queueName = queueName

            // Update content and increase visibility of an Azure Queue message
            update = fun queuedRequest content visibility -> updateMessage queue queuedRequest content visibility

            // Update visibility of an Azure Queue message and ensure it will be persisted at the end of the invisibility period.
            // Note: this may require to delete and re-post the same message to get around the Azure Queue message lifetime of 7 days.
            updateVisibility = fun queuedRequest visibilityTimeout ->
                async {
                    try
                        do! increaseVisibilityTimeout queue queuedRequest visibilityTimeout
                    with
                    | e ->
                        TraceTags.error "Could not increase message visibility in Azure backend queue. Update errors are usually due to a request taking too long to process, causing the message visibility timeout to be reached, subsequently leading to two concurrent instances of the service to process the same request."
                                        [ "queuedMessage", queuedRequest.AsString
                                          "exception", e.ToString()]
                }

            // Delete a request from the queue
            delete = fun queuedRequest ->
                async {
                    try
                        do! queue.DeleteMessageAsync(queuedRequest) |> Async.AwaitTask
                    with
                    | e ->
                        TraceTags.error "Could not delete message from Azure backend queue. This is usually due to a request taking too long to process, causing the message visibility timeout to be reached, subsequently leading to two concurrent instances of the service to process the same request."
                                            [ "queuedMessage", queuedRequest.AsString
                                              "exception", e.ToString()]
                }

            insertionTime = fun queueMessage -> queueMessage.InsertionTime.GetValueOrDefault()

            serialize = fun queueMessage -> queueMessage.AsString

            tryDeserialize = QueueScheduler.tryDeserializeJson

            // Attempt to retrieve one batch of message from the Azure queue
            tryGetMessageBatch = fun messageBatchSize maxProcessTime ->
                async {
                    let visibilityTimeout =
                        System.TimeSpan.FromSeconds(float messageBatchSize * maxProcessTime.TotalSeconds)
                        |> Microsoft.FSharpLu.Azure.Queue.softValidateVisibilityTimeout
                    let! messages = queue.GetMessagesAsync(messageBatchSize, System.Nullable visibilityTimeout, null, null) |> Async.AwaitTask
                    return messages |> Seq.toList
                }

            post = fun request -> Microsoft.FSharpLu.Azure.Queue.postMessage queue request

            postIn = fun request -> Microsoft.FSharpLu.Azure.Queue.schedulePostMessage queue request

        }
