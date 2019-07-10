module Microsoft.FSharpLu.Azure.Metrics

open System
open System.Net.Http
open Microsoft.FSharpLu.Async
open Newtonsoft

/// Azure Metrics REST Api returns json of the following format:
/// {value: [{currentValue:float, id:string, limit: float, unit: string, name: {localizedValue: string, value: string}}] }
/// MetricName, Metric and MetricValues records map to the json returned by Azure Metrics API.
/// Requred for desrialization of the Json string.

/// Metric name returned by Azure Metrics REST API call
type MetricName =
    {
        localizedValue: string
        value: string
    }

/// Metric value returned as part of Azure Metrics REST API call
type Metric =
    {
        currentValue: float
        id: string
        limit: float
        name: MetricName
        unit: string
    }

/// List of values returned as part of Azure Metrics REST API call
type MetricValues =
    {
        value: Metric list
    }

/// Subscription
type Subscription = string

/// Region
type Region = string

/// Create an HTTP client to the Azure metric API
let newHttpClient (auth:Auth.ADAuthenticationMethod) =
    async {
        let! authToken = Microsoft.FSharpLu.Azure.Auth.getAuthorizationToken auth
        let c = new HttpClient(BaseAddress = Uri(Microsoft.FSharpLu.Azure.Constants.management))
        c.DefaultRequestHeaders.Add("Authorization", "Bearer " + authToken)
        return c
    }

/// Azure Metrics retrieval class. E.g.: cores used vs cores available, network infrastructure used vs available
type AzureMetrics(httpClient:HttpClient) =

    let getMetrics (url: string) =
        async {
            let! metrics = httpClient.GetStringAsync(url).AsAsync
            return metrics
        }

    let makeUrl (resourceType : string) (subscription: Subscription) (region: Region) =
        sprintf "/subscriptions/%s/providers/%s/locations/%s/usages?api-version=%s" subscription resourceType region Constants.azureMetricsApiVersion

    let getNetworkUsage(subscription: Subscription)(region: Region) =
        makeUrl "Microsoft.Network" subscription region
        |> getMetrics

    let getComputeUsage(subscription: Subscription)(region: Region) =
        makeUrl "Microsoft.Compute" subscription region
        |> getMetrics

    member this.GetNetworkUsage(subscription: Subscription)(region: Region) =
        async{
            let! json = getNetworkUsage subscription region
            let vs = Json.JsonConvert.DeserializeObject<MetricValues>(json)
            return vs.value
        }

    member this.GetComputeUsage(subscription: Subscription)(region: Region) =
        async{
            let! json = getComputeUsage subscription region
            let vs = Json.JsonConvert.DeserializeObject<MetricValues>(json)
            return vs.value
        }

