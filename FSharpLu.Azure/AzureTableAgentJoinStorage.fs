// Copyright (c) Microsoft Corporation.
namespace Microsoft.FSharpLu.StateMachineAgent

module AzureTableAgentJoinStorage =
    open Microsoft.Azure.Cosmos
    open Microsoft.FSharpLu.StateMachineAgent
    open Microsoft.FSharpLu
    open Microsoft.FSharpLu.ErrorHandling
    open Microsoft.FSharpLu.Azure.Request.ErrorHandling
    open System.Collections.Generic

    let newStorage(credentials: Table.StorageCredentials, uri: Table.StorageUri) (tableName: string) (retryIntervalMilliseconds: int, totalTimeout: int) : Async<Agent.Storage.JoinStorage<'m>> =
        async {
            let tableClient = Table.CloudTableClient(uri, credentials)
            let table = tableClient.GetTableReference(tableName)
            let! _ = table.CreateIfNotExistsAsync() |> Async.AwaitTask

            let retrieve (joinId) =
                async {
                    let rowKey = joinId.ToString()
                    let partitionKey = joinId.ToString()
                    let retrieve = Table.TableOperation.Retrieve(partitionKey, rowKey)
                    let! result = table.ExecuteAsync(retrieve) |> Async.AwaitTask
                    match Option.ofObj result.Result with
                    | None -> return failwithf "Could not find entry with id %A" joinId
                    | Some r -> return r :?> Table.DynamicTableEntity
                }

            let entityValuesToJoinEntry (properties: IDictionary<string, Table.EntityProperty>): Agent.Storage.JoinEntry<'m> =
                let inline deserialize (key: string): 'a =
                    properties.[key].StringValue |> Json.Default.deserialize<'a>
                {
                    whenAllSubscribers = deserialize "whenAllSubscribers"
                    whenAnySubscribers = deserialize "whenAnySubscribers"
                    status = deserialize "status"
                    childrenStatuses = deserialize "childrenStatuses"
                    parent = deserialize "parent"
                }


            let joinEntryToEntityValues (joinEntry: Agent.Storage.JoinEntry<'m>) =
                dict [
                    "parent", Table.EntityProperty(Json.Default.serialize joinEntry.parent)
                    "childrenStatuses", Table.EntityProperty(Json.Default.serialize joinEntry.childrenStatuses)
                    "status", Table.EntityProperty(Json.Default.serialize joinEntry.status)
                    "whenAnySubscribers", Table.EntityProperty(Json.Default.serialize joinEntry.whenAnySubscribers)
                    "whenAllSubscribers", Table.EntityProperty(Json.Default.serialize joinEntry.whenAllSubscribers)
                ]


            return 
                {
                    add = 
                        fun joinId joinEntry ->
                            async {
                                //let partitionKey = System.DateTime.UtcNow.Ticks.ToString("D19")
                                let partitionKey = joinId.ToString()
                                let rowKey = joinId.ToString()

                                let values = joinEntryToEntityValues joinEntry
                                let entity = Table.DynamicTableEntity(partitionKey, rowKey, "*", values)

                                let tableOperation = Table.TableOperation.Insert(entity)
                                let! _ = table.ExecuteAsync(tableOperation) |> Async.AwaitTask
                                return ()
                            }

                    update =
                        fun joinId doEntryUpdate ->
                            let rec doUpdate (runningDuration: int) =
                                async {
                                    if runningDuration > totalTimeout then
                                        failwithf "Update: Timed out after trying for %d milliseconds (totalTimeout: %d)" runningDuration totalTimeout
                                    let partitionKey = joinId.ToString()
                                    let rowKey = joinId.ToString()
                                
                                    let! r = retrieve joinId

                                    let joinEntry = entityValuesToJoinEntry r.Properties
                                    let updatedJoinEntry = doEntryUpdate joinEntry

                                    let values = joinEntryToEntityValues updatedJoinEntry
                                    let entity = Table.DynamicTableEntity(partitionKey, rowKey, r.ETag, values)
                                    let tableOperation = Table.TableOperation.Replace(entity)
                                    try
                                        let! _ = table.ExecuteAsync(tableOperation) |> Async.AwaitTask
                                        return updatedJoinEntry
                                    with
                                    | HttpCommunication.Client.TooManyRequestsException(msg) ->
                                        do! Async.Sleep retryIntervalMilliseconds
                                        return! doUpdate(runningDuration + retryIntervalMilliseconds)

                                    | IsAggregateOf (SomeStorageException System.Net.HttpStatusCode.PreconditionFailed) e ->
                                        do! Async.Sleep retryIntervalMilliseconds
                                        return! doUpdate(runningDuration + retryIntervalMilliseconds)
                                }
                            doUpdate(0)

                    get = fun joinId ->
                        async {
                            let! r = retrieve joinId
                            return entityValuesToJoinEntry r.Properties
                        }
                }
        }
