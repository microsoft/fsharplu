/// This modules contains helper functions to retrieve log events from Azure Tables
module Microsoft.FSharpLu.Azure.Table

open System
open Microsoft.FSharpLu.Async
open Microsoft.FSharpLu
open Microsoft.Azure.Cosmos

// Azure tables are totally Type Provider material since it is possible to query table schema

type AzureTableEntry = System.Collections.Generic.IDictionary<string,Table.EntityProperty>

type Table.EntityProperty with
    member this.AsInt32Opt
        with get() =
            Option.ofNullable this.Int32Value

    member this.AsInt64Opt
        with get() =
             Option.ofNullable this.Int64Value

    member this.AsDateTimeOpt
        with get() =
            Option.ofNullable this.DateTime

    member this.AsString
        with get() =
            this.StringValue


/// Retrieve all entries from Azure table where PartitionKey is derived from DateTime
let queryTableEntries (credentials: Table.StorageCredentials, uri: Table.StorageUri) (tableName: String, continuationToken: Table.TableContinuationToken option) (query: Table.TableQuery) =
    async {
        let tableClient = Table.CloudTableClient(uri, credentials)
        let table = tableClient.GetTableReference(tableName)

        let! exists = table.ExistsAsync().AsAsync
        if exists then
            let! xs =
                let ct = Option.toObj continuationToken
                table.ExecuteQuerySegmentedAsync(query, ct).AsAsync

            let ct = Option.ofObj(xs.ContinuationToken)
            return Some(ct, xs.Results)
        else
            return None
    }

/// Retrieve entries from Azure table, and apply 'convert' function to every table entry
let getEvents
    (name: string)
    (convert: AzureTableEntry-> 'a) 
    (continutationToken, credentials: Table.StorageCredentials, uri: Table.StorageUri)
    (query: Table.TableQuery) =

    let convertTableEntries (xs: Table.DynamicTableEntity seq) (f: AzureTableEntry -> 'a) =
        [
            for x in xs do
                if not <| obj.ReferenceEquals(null, x)  then
                    yield f x.Properties
        ]

    async{
        let! entries = queryTableEntries (credentials, uri) (name, continutationToken) query
        return entries |> Option.map (fun (ct,xs) -> ct, convertTableEntries xs convert)
    }
