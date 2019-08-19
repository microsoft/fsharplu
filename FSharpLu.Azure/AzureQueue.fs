// Copyright (c) Microsoft Corporation.
/// F# helpers to the Azure Queue API
module Microsoft.FSharpLu.Azure.Queue

open Microsoft.Azure.Storage
open Microsoft.Azure.Storage.Queue
open Microsoft.FSharpLu
open Microsoft.FSharpLu.Async
open Microsoft.FSharpLu.Azure.AppInsights.DiagnosticsAndAppInsights
open System

/// Azure queue messages have a maximum lifetime (currently 7 days per Azure documentation at
/// https://msdn.microsoft.com/en-us/library/azure/dd179474.aspx
///   https://azure.microsoft.com/en-us/documentation/articles/service-bus-azure-and-service-bus-queues-compared-contrasted/).
/// Hence, message invisibility duration must account for the delay between when a message becomes visible and when it
///  is processed.  Otherwise, the message lifetime may expire and it may disappear from the queue before having a chance to
///  be picked up by a worker.  This duration is conservatively set to 2 days.
let MaximumExpectedMessageProcessingTime = System.TimeSpan.FromDays(2.0)

/// This is the Azure hard-coded maximum invisibility duration for a queued message
let AzureHardCodedMaximumVisibilityTimeout = System.TimeSpan.FromDays(7.0)

/// Serialize a message as string to be stored on the queue
let inline serialize< ^T> (message:^T) =
    Json.Compact.serialize(message)

/// Queue message deserialization
let inline tryDeserialize< ^T> json =
    Json.Compact.tryDeserialize< ^T>(json)

/// Post a message to an Azure queue.
///
/// Note: Declared as inline to avoid having to add a reference to assembly
/// Microsoft.Azure.Storage in every project referencing Microsoft.FSharpLu.Azure.dll
/// even though the function is not being called.
let inline postMessage< ^T> (queue:CloudQueue) (message:^T) =
    async {
        let json = serialize< ^T> message
        let message = CloudQueueMessage(json)
        return! queue.AddMessageAsync(message).AsAsync
    }

/// Post a message to an Azure queue identified by its Uri
let inline postMessageByUri< ^T> queueUri message =
    let queue = CloudQueue(queueUri)
    postMessage< ^T> queue message

/// Retrieves a list of messages from the queue
let inline dequeueAndDeleteMessages< ^T> tags queueUri count =
    async {
        let queue = CloudQueue(queueUri)
        let! cloudMessages = queue.GetMessagesAsync(count) |> Async.AwaitTask
        let mesages = cloudMessages |> Seq.toList
        for m in mesages do
            do! queue.DeleteMessageAsync(m) |> Async.AwaitTask

        return
            mesages
            |> List.map (fun cloudMessage ->
                match tryDeserialize< ^T> cloudMessage.AsString with
                | Choice1Of2 message -> message
                | Choice2Of2 json -> TraceTags.failwith (sprintf "Unable to cast message to type '%s'" (typeof< ^T>.Name))
                                                        (tags @ ["message", json]))
    }

/// Azure queue message visibility validation outcome
type VisibilityTimeoutValidation =
    /// The requested visibility timeout is too long to be updated in place, the message needs to be
    /// deleted and recreated
    | MustRecreate of System.TimeSpan
    /// The visibility timeout is short enought to be updated in place in the Azure Queue
    | UpdateInplace of System.TimeSpan

/// Validate granularity of an Azure Queue message timeout
let validateGranularity (timeout:System.TimeSpan) =
    if timeout.Milliseconds > 0 then
        Trace.warning "Azure Queues do not support message visibility with milliseconds granularity, specified value will be truncated!"
        System.TimeSpan.FromSeconds(float (int (timeout.TotalSeconds)))
    else
        timeout

/// Validates that the specified visibility timeout is within the required bounds for Azure Queue messages and
/// returns the timeout that should be used to update visibility and whether the message should be deleted and
/// re-created.
/// The message expiration time should be specified in 'messageExpirationTime' if the message is
/// already present on the queue.
/// - Returns 'None' if the visibility timeout may be safely updated to the specified timeout value.
/// - Otherwise, 'Some N' is returned, and the message must be deleted and re-posted in order to
///   extend the lifetime of the message to the desired visibility value.
/// We are doing this validation ourselves to introduce a buffer of time for message processing,
/// because the Azure API throws an obscure exception when the maximum timeout for Azure Queues is exceeded, and
/// because our messages occasionally require a longer lifetime than the Azure Queue maximum.
/// (see https://msdn.microsoft.com/en-us/library/azure/dd179474.aspx).
let validateVisibilityTimeout timeout (messageExpirationTime:System.DateTimeOffset option) =
    match messageExpirationTime with
    | _ when timeout + MaximumExpectedMessageProcessingTime > AzureHardCodedMaximumVisibilityTimeout ->
        // Returns the maximum timeout value that can be used to update the message visibility
        // given the allowable maximum invisibility limit in Azure and the expected maximum message processing time.
        Trace.info @"Depending on the message processing time (assumed no greater than %O),
                        the requested invisibility duration of %O could exceed the Azure hard-coded limit of %O.
                        See https://msdn.microsoft.com/en-us/library/azure/dd179474.aspx)"
                        MaximumExpectedMessageProcessingTime timeout AzureHardCodedMaximumVisibilityTimeout
        MustRecreate (AzureHardCodedMaximumVisibilityTimeout - MaximumExpectedMessageProcessingTime)
    | Some expirationTime when System.DateTimeOffset.UtcNow + timeout + MaximumExpectedMessageProcessingTime > expirationTime ->
        // If the desired message lifetime after being extended by 'timeout' is later than the expiration time,
        // then the message must be deleted and re-created since it will expire even though the specified
        // timeout value is less than the allowable maximum value.
        MustRecreate <| validateGranularity timeout
    | Some _
    | None ->
        UpdateInplace <| validateGranularity timeout

/// Validates that the specified visibility timeout is within the required bounds, and returns the
/// maximum allowed value if the argument exceeds the allowed maximum.
let softValidateVisibilityTimeout timeout =
    match validateVisibilityTimeout timeout None with
    | UpdateInplace timeout -> timeout
    | MustRecreate timeout -> timeout

/// Post a message on Azure Queue that should only be visible a later point in time
/// specified by a timeout value.
/// Warning: Azure has a hardcoded timeout limit. If you specify a greater value
/// the function will use instead the maximum value accepted by Azure.
let public schedulePostMessage<'T> (queue:CloudQueue) message delay =
    let json = serialize message
    queue.AddMessageAsync(
            CloudQueueMessage(json),
            Unchecked.defaultof<_>,
            System.Nullable (softValidateVisibilityTimeout delay),
            QueueRequestOptions(),
            OperationContext() )
        |> Async.AwaitTask

/// Update content and increase visibility timeout of an Azure Queue message
let public updateMessage (queue:CloudQueue) (queuedRequest:CloudQueueMessage) message visibilityTimeout =
    async {
        let json = serialize message
        let messageExpirationTime = Option.fromNullable queuedRequest.ExpirationTime

        match validateVisibilityTimeout visibilityTimeout messageExpirationTime with
        | UpdateInplace timeout ->
            queuedRequest.SetMessageContent2(json, false)
            do! queue.UpdateMessageAsync(queuedRequest, timeout,
                                MessageUpdateFields.Visibility ||| MessageUpdateFields.Content).AsAsync
        | MustRecreate timeout ->
            // delete and re-post the same message to get around the Azure Queue message lifetime of 7 days.
            do! queue.DeleteMessageAsync(queuedRequest).AsAsync
            do! queue.AddMessageAsync(
                    CloudQueueMessage(json),
                    Unchecked.defaultof<_>,
                    System.Nullable timeout,
                    QueueRequestOptions(),
                    OperationContext()).AsAsync
    }

/// Increase visibility timeout of an Azure Queue message.
/// Note: this may require to delete and re-post the same message to get around the Azure Queue message lifetime of 7 days.
let public increaseVisibilityTimeout (queue:CloudQueue) (queuedRequest:CloudQueueMessage) visibilityTimeout =
    async {
        let messageExpirationTime = Option.fromNullable queuedRequest.ExpirationTime

        match validateVisibilityTimeout visibilityTimeout messageExpirationTime with
        | UpdateInplace timeout ->
            do! queue.UpdateMessageAsync(queuedRequest, timeout, MessageUpdateFields.Visibility) |> Async.AwaitTask
        | MustRecreate timeout -> 
            let json = queuedRequest.AsString
            do! queue.DeleteMessageAsync(queuedRequest).AsAsync
            do! queue.AddMessageAsync(
                    CloudQueueMessage(json),
                    Unchecked.defaultof<_>,
                    System.Nullable timeout,
                    QueueRequestOptions(),
                    OperationContext()) |> Async.AwaitTask
    }

/// Generate a sas key for limited access to a queue so account name and key are not needed.
let getSasQueueToken (queue:CloudQueue) permissions (duration:TimeSpan) =
    let sasConstraints = SharedAccessQueuePolicy( Permissions = permissions,
                                                  SharedAccessExpiryTime = System.Nullable<_>(System.DateTimeOffset.UtcNow.Add(duration)))
    queue.GetSharedAccessSignature(sasConstraints)
