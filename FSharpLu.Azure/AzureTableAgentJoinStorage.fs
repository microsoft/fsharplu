// Copyright (c) Microsoft Corporation.

/// An Azure Table implementation of the Agent Join storage
/// This can be used to run agent state machines with fork and join
/// backed up by an Azure Table.
module Microsoft.FSharpLu.Actor.AzureTableJoinStorage

open Microsoft.Azure.Cosmos
open Microsoft.FSharpLu.Actor.StateMachine
open Microsoft.FSharpLu
open Microsoft.FSharpLu.ErrorHandling
open Microsoft.FSharpLu.Azure.Request.ErrorHandling
open System.Collections.Generic

/// Instantiate an implementation of Agent.Join.StorageInterface
/// backed up by an Azure Table
let newStorage
        (storage: Table.CloudStorageAccount)
        (tableName: string)
        (retryInterval: System.TimeSpan)
        (totalTimeout: System.TimeSpan)
        (agentId: string) /// Customer identifier that gets recorded in every entry in Azure Table
    : Async<Agent.Join.IStorage> =
    async {
        let tableClient = Table.CloudTableClient(storage.TableStorageUri, storage.Credentials)
        let table = tableClient.GetTableReference(tableName)
        let! _ = table.CreateIfNotExistsAsync() |> Async.AwaitTask

        let retrieve (joinId:JoinId) =
            async {
                let partitionKey = joinId.timestamp.Ticks.ToString("D19")
                let rowKey = joinId.guid.ToString()
                let retrieve = Table.TableOperation.Retrieve(partitionKey, rowKey)
                let! result = table.ExecuteAsync(retrieve) |> Async.AwaitTask
                match Option.ofObj result.Result with
                | None -> return failwithf "Could not find entry with id %A" joinId
                | Some r -> return r :?> Table.DynamicTableEntity
            }

        let entityValuesToJoinEntry (properties: IDictionary<string, Table.EntityProperty>): Agent.Join.Entry<'m> =
            let inline deserialize (key: string): 'a =
                properties.[key].StringValue |> Json.Default.deserialize<'a>
            {
                whenAllSubscribers = deserialize "whenAllSubscribers"
                whenAnySubscribers = deserialize "whenAnySubscribers"
                status = deserialize "status"
                childrenStatuses = deserialize "childrenStatuses"
                parent = deserialize "parent"
                created = deserialize "created"
                modified = deserialize "modified"
            }

        let joinEntryToEntityValues (joinEntry: Agent.Join.Entry<'m>) =
            dict [
                "parent", Table.EntityProperty(Json.Default.serialize joinEntry.parent)
                "childrenStatuses", Table.EntityProperty(Json.Default.serialize joinEntry.childrenStatuses)
                "status", Table.EntityProperty(Json.Default.serialize joinEntry.status)
                "whenAnySubscribers", Table.EntityProperty(Json.Default.serialize joinEntry.whenAnySubscribers)
                "whenAllSubscribers", Table.EntityProperty(Json.Default.serialize joinEntry.whenAllSubscribers)
                "created", Table.EntityProperty(Json.Default.serialize joinEntry.created)
                "modified", Table.EntityProperty(Json.Default.serialize joinEntry.modified)
                "agentId", Table.EntityProperty(agentId)
            ]

        return
            {
                new Agent.Join.IStorage with
                member __.add joinId joinEntry =
                        async {
                            let partitionKey = joinId.timestamp.Ticks.ToString("D19")
                            let rowKey = joinId.guid.ToString()

                            let values = joinEntryToEntityValues joinEntry
                            let entity = Table.DynamicTableEntity(partitionKey, rowKey, "*", values)

                            let tableOperation = Table.TableOperation.Insert(entity)
                            let! _ = table.ExecuteAsync(tableOperation) |> Async.AwaitTask
                            return ()
                        }

                member __.update joinId doEntryUpdate =
                    let start = System.DateTime.UtcNow
                    let rec doUpdate () =
                        async {
                            let spent = System.DateTime.UtcNow - start
                            if spent > totalTimeout then
                                failwithf "Update: Timed out after trying for %A (totalTimeout: %A)" spent totalTimeout

                            let! r = retrieve joinId

                            let joinEntry = entityValuesToJoinEntry r.Properties
                            let updatedJoinEntry = doEntryUpdate joinEntry

                            let partitionKey = joinEntry.created.Ticks.ToString("D19")
                            let rowKey = joinId.guid.ToString()

                            let values = joinEntryToEntityValues updatedJoinEntry
                            let entity = Table.DynamicTableEntity(partitionKey, rowKey, r.ETag, values)
                            let tableOperation = Table.TableOperation.Replace(entity)
                            try
                                let! _ = table.ExecuteAsync(tableOperation) |> Async.AwaitTask
                                return updatedJoinEntry
                            with
                            | HttpCommunication.Client.TooManyRequestsException(msg) ->
                                do! Async.Sleep (int retryInterval.TotalMilliseconds)
                                return! doUpdate ()

                            | IsAggregateOf (SomeStorageException System.Net.HttpStatusCode.PreconditionFailed) e ->
                                do! Async.Sleep (int retryInterval.TotalMilliseconds)
                                return! doUpdate ()
                        }
                    doUpdate ()

                member __.get joinId =
                    async {
                        let! r = retrieve joinId
                        return entityValuesToJoinEntry r.Properties
                    }
            }
    }
