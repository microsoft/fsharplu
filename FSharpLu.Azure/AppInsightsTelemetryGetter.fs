module Microsoft.FSharpLu.Azure.AppInsightsGetter

open System
open System.Net
open System.Net.Http
open Microsoft.FSharpLu.Async
open Microsoft.FSharpLu.Azure
open Microsoft.FSharpLu.Azure.AppInsights
open Newtonsoft.Json

///AppInsights telemetry request/response container
type AppInsightsTelemetry =
    {
        ///AppInsights telemetry data returned by the response
        data: string
        ///MS Client Request ID for the AppInsights telemetry request
        requestId: Guid
        ///Timestamp of the AppInsights telemetry request
        utcRequestTimeStamp: DateTimeOffset
    }

/// Returns an HTTP client connected to the AppInsight API
let newHttpClient (auth:Auth.ADAuthenticationMethod) =
    async {
        let! authToken = Microsoft.FSharpLu.Azure.Auth.getAuthorizationToken auth
        let c = new HttpClient(BaseAddress = Uri(Microsoft.FSharpLu.Azure.Constants.management))
        c.DefaultRequestHeaders.Add("Authorization", "Bearer " + authToken)
        return c
    }

///<summary>AppInsights getter class</summary>
///<param name="httpClient">An HTTP client connected to the AppInsight API</param>
///<param name="subscription">Azure subscription id</param>
///<param name="resourceGroup">Resource group containing AppInsights deployment</param>
///<param name="appInsightsDeployment">AppInsights deployment to execute analyticsQuery on</param>
type AppInsightsTelemetryGetter(httpClient:HttpClient, subscription: Subscription, resourceGroup: string, appInsightsDeployment: string) =

    // App Insights API team asked to add this header to the requests, this way they can easier debug and diagnose issues that might happen on the API controller side
    let [<Literal>] MsClientIdHeader = "x-ms-client-request-id"

    let getAppInsights (url: string) =
        async {
            let requestId = Guid.NewGuid()
            httpClient.DefaultRequestHeaders.Remove(MsClientIdHeader) |> ignore
            httpClient.DefaultRequestHeaders.Add(MsClientIdHeader, requestId.ToString())

            //This try/catch a work around for the bug in dotnet core where we get OperationCancelledException raised on a timeout (https://github.com/dotnet/corefx/issues/20296)
            //IMPORTANT: this work item will break the timeout handling after it gets fixed.
            try
                let! response = httpClient.GetAsync(url) |> Async.AwaitTask
                if response.IsSuccessStatusCode then
                    let! data = response.Content.ReadAsStringAsync() |> Async.AwaitTask
                    return {data = data; requestId = requestId; utcRequestTimeStamp = DateTimeOffset.UtcNow}
                else
                    return failwithf "Failed to get app insights data from URL %s due to %s [client request id: %A]" url response.ReasonPhrase requestId
            with
            | :? System.OperationCanceledException as ex ->
                if Async.DefaultCancellationToken.IsCancellationRequested then
                    return reraise ex
                else
                    return raise (System.TimeoutException(sprintf "Timed out while accessing : %s" url))
        }

    let makeUrl (analyticsQuery: string) =
        sprintf "/subscriptions/%s/resourceGroups/%s/providers/microsoft.insights/components/%s/query?api-version=%s&query=%s" subscription resourceGroup appInsightsDeployment Constants.azureAppInsightsApiVersion analyticsQuery

    /// analyticsQuery is of the format expected by AppInsights analytics See https://azure.microsoft.com/en-us/documentation/articles/app-insights-analytics-reference/ for more info.
    member this.Query(analyticsQuery: string) =
        analyticsQuery |> WebUtility.UrlEncode |> makeUrl |> getAppInsights


/// Low level AppInsights deserialization datastructures and query execution
module AppInsightsQuery =
        open Microsoft.FSharpLu

        ///Expected number of tables returned by AppInsights response
        let [<Literal>] NumberOfTablesExpectedFromAppInsights = 1

        // Used for AppInsights results JSON deserialization. Keep all caps.
        type ColumnSchema =
            {
                [<JsonProperty("name")>]
                ColumnName : string
                [<JsonProperty("type")>]
                DataType : string
            }

        /// AppInsights table definition
        type Table =
            {
                [<JsonProperty("name")>]
                TableName : string
                [<JsonProperty("columns")>]
                Columns : ColumnSchema array
                [<JsonProperty("rows")>]
                Rows : string array array
            }

        /// Tables collection returned by AppInsights query
        type Tables =
            {
                [<JsonProperty("tables")>]
                Tables : Table array
            }

            static member Empty = {Tables = Array.empty}

        /// AppInsights query result definition
        type AppInsightsResults =
            {
                tables: Tables
                queryId : System.Guid
                queryTimeStamp : System.DateTimeOffset
            }

        /// Query app insights using supplied AppInsights telemetry getter client and perform basic sanity checks
        let queryAppInsights (appInsights: AppInsightsTelemetryGetter) (query: string) =
            async {
                try
                    let! queryResults = appInsights.Query query
                    if String.IsNullOrEmpty queryResults.data then
                        TraceTags.warning "Got no results when querying AppInsights" [ "queryResults", sprintf "%A" queryResults;  "query", query]
                        return {tables = Tables.Empty; queryId = queryResults.requestId; queryTimeStamp = queryResults.utcRequestTimeStamp }
                    else
                        return { tables = Json.Default.deserialize<Tables> queryResults.data; queryId = queryResults.requestId; queryTimeStamp = queryResults.utcRequestTimeStamp }
                with
                | :? System.TimeoutException as e ->
                    TraceTags.error "Got exception when querying job info app insights traces" [ "query", query; "exceptions", e.ToString(); "isTransient", "true" ]
                    return Async.reraise e
                | e ->
                    TraceTags.error "Got exception when querying job info app insights traces" [ "query", query; "exceptions", e.ToString() ]
                    return Async.reraise e
            }