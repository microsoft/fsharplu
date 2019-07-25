// Copyright (c) Microsoft Corporation.
namespace Microsoft.FSharpLu.StateMachineAgent

/// A Request Service Processor where persistence is implemented using Azure Storage Queues
module AzureQueue =

    open Microsoft.Azure.Storage.Queue
    open Microsoft.FSharpLu
    open Microsoft.FSharpLu.Async
    open Microsoft.FSharpLu.Azure.AppInsights
    open Microsoft.FSharpLu.StateMachineAgent

    /// Update content and increase visibility of an Azure Queue message
    let update<'Q> (queue:CloudQueue) (queuedRequest:CloudQueueMessage) (m:'Q) visibility =
        Microsoft.FSharpLu.Azure.Queue.updateMessage queue queuedRequest m visibility

    /// Update visibility of an Azure Queue message and ensure it will be persisted at the end of the invisibility period.
    /// Note: this may require to delete and re-post the same message to get around the Azure Queue message lifetime of 7 days.
    let updateVisibility (queue:CloudQueue) (queuedRequest:CloudQueueMessage) visibilityTimeout =
        async {
            try
                do! Microsoft.FSharpLu.Azure.Queue.increaseVisibilityTimeout queue queuedRequest visibilityTimeout
            with
            | e ->
                TraceTags.error "Could not increase message visibility in Azure backend queue. Update errors are usually due to a request taking too long to process, causing the message visibility timeout to be reached, subsequently leading to two concurrent instances of the service to process the same request."
                                [ "queuedMessage", queuedRequest.AsString
                                  "exception", e.ToString()]
        }

    /// Delete a request from the queue
    let delete (queue:CloudQueue) (queuedRequest:CloudQueueMessage) =
        async {
            try
                do! queue.DeleteMessageAsync(queuedRequest) |> Async.AwaitTask
            with
            | e ->
                TraceTags.error "Could not delete message from Azure backend queue. This is usually due to a request taking too long to process, causing the message visibility timeout to be reached, subsequently leading to two concurrent instances of the service to process the same request."
                                    [ "queuedMessage", queuedRequest.AsString
                                      "exception", e.ToString()]
        }

    /// Execute the specified action on the specified queued request
    let executeAction (queue:CloudQueue) (queuedRequest:CloudQueueMessage) = function
        | Agent.RequestAction.Delete ->
            delete queue queuedRequest
        | Agent.RequestAction.PostponeFor visibilityTimeout ->
            updateVisibility queue queuedRequest visibilityTimeout
        | Agent.RequestAction.PostponeAndReplace (newRequest, visibilityTimeout) ->
            update queue queuedRequest newRequest visibilityTimeout

    /// Implementation of agent operations using an Azure Queue
    let inline toAgentOperations< ^T> queue =
        {
            Agent.Operations.spawn = Microsoft.FSharpLu.Azure.Queue.postMessage< ^T> queue
            Agent.Operations.spawnIn = Microsoft.FSharpLu.Azure.Queue.schedulePostMessage< ^T> queue
        }

    /// Handler for requests queued on an Azure Storage queue
    /// 'R is the type of requests handled
    /// 'C is a custom context parameter that will get passed to every request handler
    type Handler<'C, 'R> = 'C -> 'R -> Async<Agent.RequestAction<'R>>

    /// Defines a processor for request persisted on an Azure Storage Queue.
    /// 'Q defines an enumeration of possible queue Ids
    /// 'R is the type of requests handled by the processor
    /// 'C is a custom context parameter that will get passed to every request handler
    [<NoEquality;NoComparison>]
    type QueueProcessor<'Q, 'R, 'C> =
        {
            /// Name of the queue
            queueId : 'Q

            /// Function handling a request received on the Azure Queue
            handler : Handler<'C, 'R>

            /// Initial value of the maximum expected processing time.
            ///
            /// After this timeout expires Azure automatically reposts the message onto the queue.
            /// We want this value to be high enough to cover requests with long processing time (e.g. creating a VM in Azure)
            /// but not too high to avoid delays in case a real backend failure occurs.
            ///
            /// Request handlers can dynamically override this value if more time is needed to process a request.
            /// State machine agents (QueueRequestHandling module) facilitate this by automatically updating the expected
            /// processing time on each transition of the state machine.
            maxProcessTime : System.TimeSpan

            /// Number of messages to pull at once from the Azure queue
            /// Keep this number low for maxium utilization of concurrency between worker role instances
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

    /// Attempt to retrieve one batch of message from the Azure queue
    let tryGetMessageBatch (queue:CloudQueue) (processor:QueueProcessor<'Q, 'R, 'C>) =
        async {
            let visibilityTimeout = System.TimeSpan.FromSeconds(float processor.messageBatchSize * processor.maxProcessTime.TotalSeconds)
                                    |> Microsoft.FSharpLu.Azure.Queue.softValidateVisibilityTimeout
            let! messages = queue.GetMessagesAsync(processor.messageBatchSize, System.Nullable visibilityTimeout, null, null) |> Async.AwaitTask
            return messages |> Seq.toList
        }

    /// Initialize the queue-based communication channel using the specified Azure Queue reference
    let initChannel (queue:CloudQueue) =
        async {
            Trace.info "Monitoring queue %s" queue.Name
            let! r = queue.CreateIfNotExistsAsync().AsAsync
            if not r then
                Trace.info "Queue %s was already created." queue.Name
            return queue
        }

    /// Find pending request from the queue with highest priority
    let rec private findHighestPriorityRequests queues =
        async {
            match queues with
            | [] -> return None
            | (queue, handling)::rest ->
                let! m = tryGetMessageBatch queue handling
                match m with
                | [] -> return! findHighestPriorityRequests rest
                | messages -> return Some (queue, handling, messages)
        }

    /// Exception raised by message handlers when a message is rejected
    exception RejectedMessage

    /// Info returned for a request successfully processed
    type ProcessedRequestInfo<'R> =
        {
            /// agent action performed after processing the request
            action : Agent.RequestAction<'R>
            /// the request, if it was properly parsed
            request : 'R
            /// date/time when the request was posted on the queu
            requestInsertionTime : System.DateTime
            /// Time taken to process the request
            processingTime : System.TimeSpan
            /// queue message object from the Azure API
            queuedMessage : CloudQueueMessage
        }

    /// Info returned for a request that could not be processed
    type FailedRequestInfo<'R> =
        {
            /// A description of the error that occurred
            errorMessage : string
            /// More details about the error
            details : string
            /// agent action performed after processing the request
            action : Agent.RequestAction<'R>
            /// processed request
            request : 'R option
            /// date/time when the request was posted on the queu
            requestInsertionTime : System.DateTime
            /// Time taken to process the request
            processingTime : System.TimeSpan
            /// queue message object from the Azure API
            queuedMessage : CloudQueueMessage
        }

    /// Outcome of the processing of a request
    type RequestOutcome<'R> =
    | Processed of ProcessedRequestInfo<'R>
    | Error of FailedRequestInfo<'R>
    | Rejected of FailedRequestInfo<'R>
    | ExceptionThrown of System.Exception * FailedRequestInfo<'R>

    /// Process a single request from an Azure Queue
    let processMessage<'C,'R>
            (context:'C)
            (queue:CloudQueue)
            (queuedMessage:CloudQueueMessage)
            (tryDeserialize:string -> Async<Result<'R,string>>)
            (handler:Handler<'C, 'R>)
            (getTags:'R -> (string*string) list)
            (logger: RequestOutcome<'R> -> unit) =
        async {

            let requestInsertionTime =
                // This gymnastic is needed to workAsync around spurious warning #52 from F# compiler in release mode.
                // See http://stackoverflow.com/questions/13753312/warning-produced-by-f-value-has-been-copied-to-ensure-the-original-is-not-muta
                let insertionTime = queuedMessage.InsertionTime |> (fun x -> x.GetValueOrDefault())
                insertionTime.UtcDateTime

            let! parseResult = tryDeserialize queuedMessage.AsString

            match parseResult with
            | Result.Error deserializationError ->
                let outcome =
                    RequestOutcome.Error
                        {
                            errorMessage = "Could not parse queued message"
                            details = deserializationError
                            action  = Agent.RequestAction.Delete
                            request = None
                            requestInsertionTime = requestInsertionTime
                            processingTime = System.TimeSpan.Zero
                            queuedMessage = queuedMessage
                        }
                logger outcome
                do! delete queue queuedMessage
                return outcome

            | Result.Ok parsedMessage ->
                let tags = getTags parsedMessage
                TraceTags.info "Processing queued message"
                                 (tags @ ["parsedMessage", (sprintf "%A" parsedMessage)])

                let processingTime = System.Diagnostics.Stopwatch()
                try
                    processingTime.Start()
                    let! action = handler context parsedMessage
                    processingTime.Stop()

                    do! executeAction queue queuedMessage action
                    let outcome =
                        RequestOutcome.Processed
                            {
                                action  = action
                                request = parsedMessage
                                requestInsertionTime = requestInsertionTime
                                processingTime = processingTime.Elapsed
                                queuedMessage = queuedMessage
                            }

                    logger outcome
                    return outcome
                with
                | RejectedMessage ->
                    processingTime.Stop()
                    let outcome =
                        RequestOutcome.Rejected
                            {
                                errorMessage = "This request type was rejected from queue handler"
                                details = sprintf "Queue name: %s" queue.Name
                                action  = Agent.RequestAction.Delete
                                request = Some parsedMessage
                                requestInsertionTime = requestInsertionTime
                                processingTime = processingTime.Elapsed
                                queuedMessage = queuedMessage
                            }
                    logger outcome
                    do! delete queue queuedMessage
                    return outcome
                | e ->
                    processingTime.Stop()
                    let elapsed = processingTime.Elapsed
                    TraceTags.trackException e (tags @[
                                                  "postedTime", requestInsertionTime.ToString()
                                                  "processingTime", elapsed.ToString()
                                                  "request", queuedMessage.AsString
                                                ])

                    let outcome =
                        RequestOutcome.ExceptionThrown
                            (e,
                            {
                                errorMessage = "Exception raised while processing request"
                                details = sprintf "Exception: %O" e
                                action  = Agent.RequestAction.Delete
                                request = Some parsedMessage
                                requestInsertionTime = requestInsertionTime
                                processingTime = processingTime.Elapsed
                                queuedMessage = queuedMessage
                            })
                    do! delete queue queuedMessage
                    return outcome
        }

    exception ProcessingLoopCancelled

    /// A processing loop handling requests posted on multiple Azure Queues with
    /// different assigned priorities
    let processingLoopMultipleQueues<'Q, 'R, 'C>
                (options:Options)
                (queuesOrderedByPriority:(CloudQueue * QueueProcessor<'Q, 'R, 'C>) list)
                (createRequestContext : CloudQueue -> CloudQueueMessage -> 'C)
                (signalHeartBeat : unit -> unit)
                (terminationRequested:System.Threading.CancellationToken)
                (tryDeserialize:'C -> string -> Async<Result<'R, string>>)
                getTags
                logger
                =
        async {
            use processingPool = new Microsoft.FSharpLu.Async.Synchronization.Pool(options.ConcurrentRequestWorkers)
            let pool = processingPool :> Microsoft.FSharpLu.Async.Synchronization.IPool

            /// Process a batch of messages
            let processMessageBatch handler (queue:CloudQueue) (queueMessageBatch:CloudQueueMessage list) =
                Async.Parallel
                    [
                        for queuedMessage in queueMessageBatch ->
                            async {
                                use! resourceAvailable =
                                    pool.AcquireAsync(Some(options.WorkerReplacementTimeout.TotalMilliseconds |> int))

                                if terminationRequested.IsCancellationRequested then
                                    raise ProcessingLoopCancelled
                                let context = createRequestContext queue queuedMessage
                                let! outcome =
                                    processMessage
                                        context
                                        queue
                                        queuedMessage
                                        (tryDeserialize context)
                                        handler
                                        getTags
                                        logger
                                return ()
                            }
                            |> Async.Catch // Individual request should be able to fail independently without taking down the entire batch
                    ]
                |> Async.Ignore


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