// Copyright (c) Microsoft Corporation.

/// Provide an implementation of Microsoft.FSharpLu.QueueProcessing.QueueScheduler.QueueingAPI
/// based on Azure storage Queues
module Microsoft.FSharpLu.Actor.QueueScheduler.AzureQueue

open Microsoft.Azure.Storage.Queue
open Microsoft.FSharpLu.Azure.Queue
open Microsoft.FSharpLu.Logging
open Microsoft.Azure.Storage
open Microsoft.FSharpLu.Actor.ServiceRequests

type ProcessedRequestInfo<'Request, 'Result> = ProcessedRequestInfo<'Request, 'Result, CloudQueueMessage>
type FailedRequestInfo<'Request> =  FailedRequestInfo<'Request, CloudQueueMessage>

/// Create an instance of an queue processor based on Azure Queue
let inline newQueue<'QueueId, ^QueueContent> (azureStorage:CloudStorageAccount) queueNamePrefix (queueId:'QueueId)
    : QueueingAPI<CloudQueueMessage, ^QueueContent> =
    // Create the reference to the Azure queue
    let queueClient = azureStorage.CreateCloudQueueClient()
    let queueName = (queueNamePrefix + "-" + sprintf "%A" queueId).ToLowerInvariant()
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

        prettyPrintQueueMessage = fun queueMessage -> queueMessage.AsString

        tryGetContent = fun queueMessage ->
                match Microsoft.FSharpLu.Json.Compact.tryDeserialize< ^QueueContent> queueMessage.AsString with
                | Choice1Of2 message -> Result.Ok message
                | Choice2Of2 error -> Result.Error error

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

/// A scheduler factory based on Azure Queue queueing system
type AzureQueueSchedulerFactory<'Header, 'Request, 'CustomContext>
        (
            /// Maximum expected time to process each transition of the state machine
            maximumExpectedStateTransitionTime : System.TimeSpan
        ) =
    interface ISchedulerFactory<CloudQueueMessage, 'Header, 'Request, 'CustomContext> with
        member __.create<'Input, 'State> context envelope =
            {
                spawn = context.queue.post
                joinStore = context.joinStore

                onInProcessSleep = fun sleepTime ->
                    // Increase visibility timeout of the Azure queue message
                    // so that we can resume where we left off if the process crashes, is killed, upgraded...
                    context.queue.updateVisibility context.queuedMessage (sleepTime + maximumExpectedStateTransitionTime)

                onGoto = fun (newState:'State) ->
                    let amendedEnvelop = envelope.updateState<'Input, 'State> newState
                    context.queue.update context.queuedMessage amendedEnvelop maximumExpectedStateTransitionTime
                embed = fun metadata state -> envelope.updateMetadataAndState<'Input, 'State> metadata state
            }

/// Workaround for F# compiler bug when building with `dotnet build`
/// C:\sources\fsharpLu\FSharpLu.Azure.Test\AzureQueueSchedulerTests.fs(36,81):
///  error FS0001: The type 'AzureQueueSchedulerFactory<Header,ServiceRequests,CustomContext>' is not compatible
///  with the type 'ISchedulerFactory<CloudQueueMessage,Header,ServiceRequests,CustomContext>'
let createAzureSchedulerFactory<'Header, 'Request, 'CustomContext> maximumExpectedStateTransitionTime =
    AzureQueueSchedulerFactory<'Header, 'Request, 'CustomContext>(maximumExpectedStateTransitionTime)
                            :> ISchedulerFactory<CloudQueueMessage, 'Header, 'Request, 'CustomContext>
