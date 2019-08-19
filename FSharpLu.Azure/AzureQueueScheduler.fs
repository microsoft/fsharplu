// Copyright (c) Microsoft Corporation.

/// Provide an implementation of Microsoft.FSharpLu.QueueProcessing.QueueScheduler.QueueingAPI
/// based on Azure storage Queues
module Microsoft.FSharpLu.Actor.QueueScheduler.AzureQueue

open Microsoft.Azure.Storage.Queue
open Microsoft.FSharpLu.Logging
open Microsoft.Azure.Storage
open Microsoft.FSharpLu.Actor.ServiceRequests
open Microsoft.FSharpLu.Azure.Queue
open Microsoft.FSharpLu.Async

type ProcessedRequestInfo<'Request, 'Result> = ProcessedRequestInfo<'Request, 'Result, CloudQueueMessage>
type FailedRequestInfo<'Request> =  FailedRequestInfo<'Request, CloudQueueMessage>

/// Create an instance of an queue processor based on Azure Queue
/// `clear` - true to clear the queue content if it already exists
type AzureQueueScheduler<'QueueId, 'QueueContent> (azureStorage:CloudStorageAccount, queueNamePrefix, queueId:'QueueId, ?clear) =

    let clear = Option.defaultValue false clear

    // Create the reference to the Azure queue
    let queueClient = azureStorage.CreateCloudQueueClient()
    let theQueueName = (queueNamePrefix + "-" + sprintf "%A" queueId).ToLowerInvariant()
    let queue = 
        let q = queueClient.GetQueueReference(theQueueName)
        if not <| q.CreateIfNotExists() then
            Trace.info "Queue %s was already created." theQueueName
        if clear then
            q.Clear()
        q
    
    member __.AzureQueue with get () = queue
    
    /// Serialize an object to Json with the specified converter
    static member inline serialize (object:'T) :string =
        Newtonsoft.Json.JsonConvert.SerializeObject(object, Microsoft.FSharpLu.Json.Compact.TupleAsArraySettings.formatting, Microsoft.FSharpLu.Json.Compact.TupleAsArraySettings.settings) 
        
    /// Deserialize a Json to an object of type 'T
    static member inline tryDeserialize<'T> json : Result< ^T, System.Exception> =
        try
            let o = Newtonsoft.Json.JsonConvert.DeserializeObject< ^T>(json, Microsoft.FSharpLu.Json.Compact.TupleAsArraySettings.settings) 
            if obj.ReferenceEquals(o, null) then
                Result.Error <| (Newtonsoft.Json.JsonReaderException("Json deserialization returned null") :> System.Exception)
            else
                Result.Ok o
        with
        | :? Newtonsoft.Json.JsonReaderException
        | :? Newtonsoft.Json.JsonSerializationException as exn ->
            Result.Error <| exn
        | exn ->
            Result.Error <| (Newtonsoft.Json.JsonReaderException("Exception while deserializing stream: %O", exn) :> System.Exception)


    interface IQueueingAPI<CloudQueueMessage, 'QueueContent> with
        member __.queueName = theQueueName

        member __.tryGetContent queuedRequest =
            AzureQueueScheduler<_,_>.tryDeserialize<'QueueContent> queuedRequest.AsString            
        
        member __.post request =
            async {
                let json = AzureQueueScheduler<_,_>.serialize<'QueueContent> request
                let message = CloudQueueMessage(json)
                return! queue.AddMessageAsync(message).AsAsync
            }

        member __.postIn request t =
            Microsoft.FSharpLu.Azure.Queue.schedulePostMessage< ^QueueContent> queue request t

        // Update content and increase visibility of an Azure Queue message
        member __.update queuedRequest content visibility =
            Microsoft.FSharpLu.Azure.Queue.updateMessage queue queuedRequest content visibility

        // Update visibility of an Azure Queue message and ensure it will be persisted at the end of the invisibility period.
        // Note: this may require to delete and re-post the same message to get around the Azure Queue message lifetime of 7 days.
        member __.updateVisibility queuedRequest visibilityTimeout =
            async {
                try
                    do! increaseVisibilityTimeout queue queuedRequest visibilityTimeout
                with
                | e ->
                    TraceTags.error "Could not increase message visibility in Azure backend queue. Update errors are usually due to a request taking too long to process, causing the message visibility timeout to be reached, subsequently leading to two concurrent instances of the service to process the same request."
                                        [ "queuedMessage", queuedRequest.AsString
                                          "exception", e.ToString()
                                          "visibilityTimeout", visibilityTimeout.ToString()
                                          "nextVisibileTime", queuedRequest.NextVisibleTime.GetValueOrDefault().ToString()
                                          "nowInUtc", System.DateTime.UtcNow.ToString()
                                          "expiryDate", queuedRequest.ExpirationTime.ToString()
                                          "id", queuedRequest.Id
                                          "popreceipt", queuedRequest.PopReceipt.ToString()]
           }

        // Delete a request from the queue
        member __.delete queuedRequest =
            async {
                try
                    do! queue.DeleteMessageAsync(queuedRequest) |> Async.AwaitTask
                with
                | e ->
                    TraceTags.error "Could not delete message from Azure backend queue. This is usually due to a request taking too long to process, causing the message visibility timeout to be reached, subsequently leading to two concurrent instances of the service to process the same request."
                                        [ "queuedMessage", queuedRequest.AsString
                                          "exception", e.ToString()]
            }

        member __.insertionTime queuedRequest = queuedRequest.InsertionTime.GetValueOrDefault()

        member __.prettyPrintQueueMessage queuedRequest = queuedRequest.AsString


        // Attempt to retrieve one batch of message from the Azure queue
        member __.tryGetMessageBatch messageBatchSize maxProcessTime =
            async {
                let visibilityTimeout =
                    System.TimeSpan.FromSeconds(float messageBatchSize * maxProcessTime.TotalSeconds)
                    |> Microsoft.FSharpLu.Azure.Queue.softValidateVisibilityTimeout
                let! messages = queue.GetMessagesAsync(messageBatchSize, System.Nullable visibilityTimeout, null, null) |> Async.AwaitTask
                return messages |> Seq.toList
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

                embedCallReturnValue = 
                    { new Microsoft.FSharpLu.Actor.StateMachine.Agent.ICallReturnEmbedder<_> with
                        member __.embed<'t> (result:'t) m = m.embedResult result
                    }
            }

/// Workaround for F# compiler bug when building with `dotnet build`
/// C:\sources\fsharpLu\FSharpLu.Azure.Test\AzureQueueSchedulerTests.fs(36,81):
///  error FS0001: The type 'AzureQueueSchedulerFactory<Header,ServiceRequests,CustomContext>' is not compatible
///  with the type 'ISchedulerFactory<CloudQueueMessage,Header,ServiceRequests,CustomContext>'
let createAzureSchedulerFactory<'Header, 'Request, 'CustomContext> maximumExpectedStateTransitionTime =
    AzureQueueSchedulerFactory<'Header, 'Request, 'CustomContext>(maximumExpectedStateTransitionTime)
                            :> ISchedulerFactory<CloudQueueMessage, 'Header, 'Request, 'CustomContext>
