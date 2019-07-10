namespace Microsoft.FSharpLu.Azure.Test.Queue

open System
open Microsoft.Azure.Storage
open Microsoft.Azure.Storage.Queue
open Microsoft.FSharpLu.Azure.Queue
open Microsoft.FSharpLu.Async

[<AutoOpen>]
module Tests =

    let validateHardCodedMaximumVisibilityTimeout(storageAccount: string, storageKey: string) =
        async {
            let auth = Auth.StorageCredentials(storageAccount, storageKey)
            let storage = CloudStorageAccount(auth, true)

            let queueClient = storage.CreateCloudQueueClient()

            let queueName = "maxvisibilitytest"

            let q = queueClient.GetQueueReference(queueName)
            let! exists = q.ExistsAsync().AsAsync
            if exists then
                failwithf "Queue %s already exists, use a different queue name" queueName
            let! _ = q.CreateIfNotExistsAsync().AsAsync

            let msg = box "wubalubadubdub"
            let rec validate (t: TimeSpan) =
                async{
                    try
                        do! schedulePostMessage q msg t
                        printfn "Scheduled with visibility: %O" t
                        return! validate (t.Add(TimeSpan.FromDays 1.0))
                    with
                    e ->
                        printfn "Failed to schedule with visibility: %O" t
                        try
                            let t = t.Subtract(TimeSpan.FromDays 1.0)
                            // expected to succeed
                            do! schedulePostMessage q msg t
                            return t
                        with e ->
                            return failwithf "Failed to schedule post with smaller timespan %O" t

                }

            let! c =
                Async.Catch(
                    async {
                        let! t = 1.0 |> TimeSpan.FromDays |> validate
                        if t <> AzureHardCodedMaximumVisibilityTimeout then
                            failwithf "Expected maximum visibility timeout to be: %O but got: %O" AzureHardCodedMaximumVisibilityTimeout t
                    })
            do! q.DeleteAsync().AsAsync

            match c with
            | Choice1Of2 () -> ()
            | Choice2Of2 e -> reraise e
        }
