module Microsoft.FSharpLu.Actor.QueueScheduler.Azure.Test

open Xunit
open Microsoft.FSharpLu.Logging
open Microsoft.FSharpLu.Actor.ServiceRequests
open Microsoft.FSharpLu.Actor
open Microsoft.FSharpLu.Actor.QueueScheduler
open Microsoft.FSharpLu.Actor.QueueScheduler.Test
open Microsoft.FSharpLu.Actor.QueueScheduler.Test.Example

open Microsoft.Azure.Cosmos
open Microsoft.Azure.Storage.Queue
open Microsoft.FSharpLu
open Microsoft.FSharpLu.Actor.QueueScheduler.AzureQueue

let JoinTableName = "QueueServiceTestAgentJoinStorageTable"
let QueueNamePrefix  = "QueueServiceTest"
let MaximumExpectedStateTransitionTime = System.TimeSpan.FromSeconds(5.0)

let queueProcessLoop (storageAccountConnectionString:string) queues maximumExpectedStateTransitionTime =
    async {
        let! joinStore =
            AzureTableJoinStorage.newStorage
                                    (Table.CloudStorageAccount.Parse(storageAccountConnectionString))
                                    JoinTableName
                                    (System.TimeSpan.FromMilliseconds(50.0))
                                    (System.TimeSpan.FromSeconds(4.0))
                                    "testAgent"
        let shutdownSource = new System.Threading.CancellationTokenSource()
        try
            do! QueueScheduler.processingLoopMultipleQueues<Example.ServiceQueues, Envelope<Example.Header, Example.ServiceRequests>, _, CloudQueueMessage>
                    (Microsoft.FSharpLu.Logging.Interfaces.fromTraceTag<Microsoft.FSharpLu.Azure.AppInsights.TagsTracer>)
                    Example.QueuesProcessingOptions
                    (Example.queuesHandlersOrderedByPriority (AzureQueueSchedulerFactory<Example.Header, Example.ServiceRequests, Example.CustomContext>(maximumExpectedStateTransitionTime)) shutdownSource)
                    (fun queueId -> Map.find queueId queues)
                    (fun queue queuedMessage -> {
                        queue = queue
                        queuedMessage = queuedMessage
                        joinStore = joinStore
                        customContext = { contextName = "azurebla" }
                    })
                    ignore // no heartbeat
                    shutdownSource.Token
                    (fun _ -> []) // no tags
                    {new IOutcomeLogger<_,_> with member __.log _ = ()} // no logger
        with
        | QueueScheduler.ProcessingLoopCancelled ->
            Trace.warning "Processing loop terminated by Azure."

    }

[<SkippableFact>]
let ``AzureQueue Fibonnaci Test`` () =
    async {
        let storageAccountConnectionString = System.Environment.GetEnvironmentVariable("TESTSTORAGEACCOUNT_CONNECTIONSTRING")
        Skip.If(isNull storageAccountConnectionString, "No Azure stroage account available for testing. Set env var TESTSTORAGEACCOUNT_CONNECTIONSTRING to run this test.")

        let azureStorage = Azure.Context.getStorageAccountFromConnectionString storageAccountConnectionString
        let queues =
            [
                ImportantQueue, AzureQueue.newQueue azureStorage QueueNamePrefix ImportantQueue
                NotImportantQueue, AzureQueue.newQueue azureStorage QueueNamePrefix NotImportantQueue
            ] |> Map.ofSeq

        let! expected = startFibonacciTest queues.[ImportantQueue]
        
        do! queueProcessLoop storageAccountConnectionString queues MaximumExpectedStateTransitionTime

        do! endFibonacciTest expected
    }
