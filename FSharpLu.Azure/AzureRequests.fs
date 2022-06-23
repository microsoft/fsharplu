/// Helpers for Microsoft.Rest.Azure the REST client used to access Azure Management
module Microsoft.FSharpLu.Azure.Request


open System
open System.Net.Http
open Microsoft.Rest.Azure
open Microsoft.FSharpLu.Azure.AppInsights
open Microsoft.FSharpLu.Async
open Microsoft.FSharpLu
open Microsoft.FSharpLu.ErrorHandling
open FSharp.Control

module Constants =
    /// Intervals between two calls when polling Azure Resource Manager
    let PollingInterval = TimeSpan.FromSeconds(10.0)

[<AutoOpen>]
module ErrorHandling =

    /// Match Azure storage exceptions with the specified status code
    let SomeStorageException (httpStatusCode:System.Net.HttpStatusCode) (ex:System.Exception) =
        match ex with
        | :? Microsoft.Azure.Cosmos.Table.StorageException as e
            when e.RequestInformation.HttpStatusCode = (int)httpStatusCode ->
                Some ex
        | :? Microsoft.Azure.Storage.StorageException as e
            when e.RequestInformation.HttpStatusCode = (int)httpStatusCode ->
              Some ex
        | _ -> None


    /// Match any cloud exception
    let AnyCloudError (e:System.Exception) =
        match e with
        | :? Microsoft.Rest.Azure.CloudException as e -> Some e.Body
        | _ -> None

    /// Match a cloud exception with the specified error code
    let SomeCloudExceptionError errorCode (e:System.Exception) =
        match e with
        | :? Microsoft.Rest.Azure.CloudException as e when
                not (isNull e.Body) && e.Body.Code = errorCode
                -> Some e.Body
        | _ -> None

    let SomeCloudExceptionStatusCode (statusCode:System.Net.HttpStatusCode) (e:System.Exception) =
        match e with
        | :? Microsoft.Rest.Azure.CloudException as e when
                (not <| isNull e.Response) && e.Response.StatusCode = statusCode
                -> Some e.Response
        | _ -> None

    let SomeLongRunningOperationFailedException e = SomeCloudExceptionError "Failed" e
    let SomeConflictErrorException e = SomeCloudExceptionError "ConflictError" e

    let SomeNotFoundException e = SomeCloudExceptionError "NotFound" e
    let SomeResourceNotFoundException e = SomeCloudExceptionError "ResourceNotFound" e
    let SomeResourceGroupNotFoundException e = SomeCloudExceptionError "ResourceGroupNotFound" e
    let SomeExpiredAuthenticationToken e = SomeCloudExceptionError "ExpiredAuthenticationToken" e
    let SomeDeploymentNotFound e = SomeCloudExceptionError "DeploymentNotFound" e
    let SomeNetworkAclsValidationFailure e = SomeCloudExceptionError "NetworkAclsValidationFailure" e
    let SomeClusterNotFoundException e = SomeCloudExceptionError "ClusterNotFound" e
    let InvalidTemplateDeployment e = SomeCloudExceptionError "InvalidTemplateDeployment" e
    let InvalidAddressPrefix e = SomeCloudExceptionError "SecurityRuleInvalidAddressPrefix" e
    let InternalServerError e = SomeCloudExceptionError "InternalServerError" e

    let (|NotFoundException|_|) (e:System.Exception) =
        match e with
        | :? Microsoft.Rest.Azure.CloudException as e when not (isNull e.Response) && e.Response.StatusCode = System.Net.HttpStatusCode.NotFound
            -> Some e.Body
        | _ -> None

    let rec (|NameResolutionFailure|_|) (e:System.Exception) =
        match e with
        | :? System.Net.WebException as e
            when e.Status = System.Net.WebExceptionStatus.NameResolutionFailure ->
            Some e
        | :? System.Net.Http.HttpRequestException
            when not <| isNull e.InnerException ->
                match e.InnerException with
                | NameResolutionFailure e -> Some e
                | _ -> None
        | _ -> None

    let SomeNameResolutionFailure = function
        | NameResolutionFailure e -> Some e
        | _ -> None

    // This function dumps all of the details of a CloudError type (which can contain other CloudErrors) into a string.
    // See https://github.com/Azure/azure-sdk-for-net/blob/master/src/SdkCommon/ClientRuntime.Azure/ClientRuntime.Azure/CommonModels/CloudError.cs
    // for details of the CloudError type.
    let printCloudError (cloudError:CloudError) (indentLevel:int) =
        let rec printCloudErrorInternal (cloudError:CloudError) (errorDetails:System.Text.StringBuilder) (indentLevel:int) =
            let indent = String.replicate indentLevel "    "
            errorDetails.AppendLine(sprintf "%s CloudError.Code: %s" indent cloudError.Code) |> ignore
            errorDetails.AppendLine(sprintf "%s CloudError.Message: %s" indent cloudError.Message) |> ignore
            errorDetails.AppendLine(sprintf "%s CloudError.Target: %s" indent cloudError.Target) |> ignore

            if not (isNull cloudError.AdditionalInfo) then
                for info in cloudError.AdditionalInfo do
                    errorDetails.AppendLine(sprintf "%s CloudError.AdditionalInfo: %A" indent info) |> ignore

            if not (isNull cloudError.Details) then
                for errorDetail in cloudError.Details do
                    errorDetails.AppendLine((printCloudErrorInternal errorDetail errorDetails (indentLevel + 1)) : string) |> ignore

            errorDetails.ToString()

        let errorDetails = System.Text.StringBuilder()
        printCloudErrorInternal cloudError errorDetails indentLevel

[<AutoOpen>]
module PagedResults =
    open System.Threading.Tasks

    type AzurePageEnumerator<'p, 't> =
        {
            getFirstPage : unit -> System.Threading.Tasks.Task<'p>
            getNextPage : string -> System.Threading.Tasks.Task<'p>
            pageAsList : 'p -> 't list
            getLink : 'p -> string
        }

    /// Enumerate ARM resources using the provided getFirstPage and getNextPage functions.
    /// Apply transformer function pageAsList to convert each page into a list of elements.
    /// Returns the concatenation of all elements from all pages.
    let inline enumerateAllPagesAsync<'p, 't> tags resourceName (operators : AzurePageEnumerator<'p, 't>) =
        async {
            let rec aux accumulator link =
                async {
                    if obj.ReferenceEquals(null, link) then
                        return accumulator
                    else
                        try
                            let! page = operators.getNextPage link |> Async.AwaitTask
                            let link = operators.getLink page
                            return! aux (operators.pageAsList page::accumulator) link
                        with
                        | IsAggregateOf AnyCloudError e ->
                            return TraceTags.failwith
                                            (sprintf "Could not retrieve list of %s" resourceName)
                                            (tags @ [ "resourceName", resourceName
                                                      "code", e.Code
                                                      "message", e.Message ])
                }

            let! firstPage = operators.getFirstPage() |> Async.AwaitTask
            let link = operators.getLink firstPage
            let! allPages = aux [operators.pageAsList firstPage] link
            return allPages
                    |> List.rev
                    |> List.concat
        }

    /// Enumerate all elements in a given cloud directory/container
    /// and return only those of type 'File.
    /// The enumeration is not-recursive (e.g. any directory structure is ignored).
    /// This can be used to enumerate all blobs under an Azure Blob container, for instance.
    let inline enumerateAllImmediateFiles<'Container, 'File, 'Item, 'Token, ^Segment
                                            when ^Segment : (member Results:seq<'Item>)
                                            and ^Segment : (member ContinuationToken:'Token)
                                            and 'Token : null>
                    (container:'Container)
                    (listChildrenSegmented:('Container -> 'Token -> Task< ^Segment>)) : AsyncSeq<'File> =

        let getNextSegment = function
            | None -> async.Return None
            | Some token ->
                async {
                    let! segment = listChildrenSegmented container token |> Async.AwaitTask
                    let results, newContinuationToken  =
                            (^Segment : (member Results: seq<'Item>) segment),
                            (^Segment : (member ContinuationToken:'Token) segment)

                    let isFile item = (box item) :? 'File
                    let filesInSegment = results |> Seq.filter isFile |> Seq.cast<'File>
                    return Some (filesInSegment, Option.fromObject newContinuationToken)
                }

        AsyncSeq.unfoldAsync getNextSegment (Some null)
        |> AsyncSeq.concatSeq

    /// Enumerate all elements in a given cloud directory, optionally recursively,
    /// using the provided segmented-child enumeration function.
    /// This can be used, for instance, to enumerate all files under an Azure File directory
    /// or an Azure blob container.
    ///
    /// E.g. For Azure File enumeration use the following code
    ///
    ///     open Microsoft.Azure.Storage.File
    ///     listAllItemsRecursively<CloudFileDirectory, CloudFile, IListFileItem, FileContinuationToken, FileResultSegment>
    ///             cloudDirectory
    ///             true
    ///             (fun dir token -> dir.ListBlobsSegmentedAsync(token))
    ///

    let inline enumerateAllFiles<'Directory, 'File, 'Item, 'Token, ^Segment
                                    when ^Segment : (member Results:seq<'Item>)
                                    and ^Segment : (member ContinuationToken:'Token)
                                    and 'Token : null>
                    (rootDir:'Directory)
                    recursively
                    (listChildrenSegmented:('Directory -> 'Token -> Task< ^Segment>)) : AsyncSeq<'File> =
        let enumerateImmediateChildren dir =
            let getNextSegment dir state =
                async {
                     match state with
                     | None -> return None
                     | Some token ->
                         let! segment = listChildrenSegmented dir token |> Async.AwaitTask
                         let results, newContinuationToken  =
                                 (^Segment : (member Results: seq<'Item>) segment),
                                 (^Segment : (member ContinuationToken:'Token) segment)
                         return Some (results, Option.fromObject newContinuationToken)
                }
            AsyncSeq.unfoldAsync (getNextSegment dir) (Some null)
            |> AsyncSeq.concatSeq

        let rec enumerateRecursiveFiles (item:'Item) : AsyncSeq<'File> =
            match box item with
            | :? 'File as file ->
                AsyncSeq.singleton file
            | :? 'Directory as subDir when recursively ->
                asyncSeq {
                    let recrusiveFiles =
                        enumerateImmediateChildren subDir
                        |> AsyncSeq.map enumerateRecursiveFiles
                    for files in recrusiveFiles do
                       for f in files do
                           yield f
                }
            | _ ->
                AsyncSeq.empty

        enumerateRecursiveFiles (box rootDir :?> 'Item)

[<AutoOpen>]
/// Helpers to query status of long-running Azure asynchronous operations
/// using our own implementation as a workaround
/// for https://github.com/Azure/azure-sdk-for-net/issues/1777
/// (See #2675: 'Migrate to latest Microsoft.Azure.Management.Compute Nuget" for more details)
module AsynchronousAzureOperations =

    open Newtonsoft.Json
    open Newtonsoft.Json.Linq
    open System.Net

    /// A URL used to check the status of an asynchronous operation
    type AsyncOperationUrl = string

    /// Information needed to poll Azure for the result of an aynsynchronous operation.
    type AsyncOperationRequest =
        {
            /// Asynchronous result URL. The URLs generated by Azure are URLs of the form:
            /// https://management.azure.com/subscriptions/{subscriptionId}/providers/Microsoft.Compute/locations/westus/operations/41ed9bed-c9fd-4602-a57d-70f61ee5ff83?api-version=2016-03-30
            AsyncStatusUrl : AsyncOperationUrl
            /// Recommended polling interval
            PollingInterval : System.TimeSpan option
        }

    /// Try to get the value of a given field from a HTTP response header
    let inline tryGetField tags (x:Microsoft.Rest.IHttpOperationResponse) name =
        match x.Response.Headers.TryGetValues name with
        | true, values ->
            match values |> Seq.toList with
            | [ azureAsyncOperation ] -> Some azureAsyncOperation
            | [] -> TraceTags.failwith "Could not extract field value from HTTP response header" (tags@["fieldName", name])
            | _ -> TraceTags.failwith "Too many values assigned to HTTP response header field (Expecting a single value)" (tags@["fieldName", name])
        | false,  _->
            None

    /// Type extension used to extract the AsyncOperationUrl from an AzureAsyncOperation<'T> or an AzureOperationResponse
    /// This is used to track status of asynchronous Azure operations as documented in
    /// https://docs.microsoft.com/en-us/azure/azure-resource-manager/resource-manager-async-operations
    type Microsoft.Rest.IHttpOperationResponse with
        /// Return the URL that can be used to check the status of the asynchronous operation
        member x.GetAsyncOperationUrl(?tags) : AsyncOperationRequest =
            let tags = Option.defaultValue [] tags
            match tryGetField tags x "Azure-AsyncOperation" with
            | Some azureAsyncOperation ->
                {
                     AsyncStatusUrl = azureAsyncOperation
                     PollingInterval = None
                }
            | None ->
                match tryGetField tags x "Location" with
                | None ->
                    TraceTags.failwith "Fields `Azure-AsyncOperation` and `Location` both missing from HTTP response header."
                                (tags @ [ "headers", (sprintf "%A" x.Response.Headers)
                                          "response", (sprintf "%A" x.Response.Content) ])
                | Some locationUrl ->
                    {
                         AsyncStatusUrl = locationUrl
                         PollingInterval = tryGetField tags x "Retry-After"
                                            |> Option.bind Microsoft.FSharpLu.Parsing.tryParseInt
                                            |> Option.map (fun s -> System.TimeSpan.FromSeconds(float s))
                    }

    /// Return type for function getAsyncOperationStatusWithOutput
    type AsyncOperationStatusOutput<'T> =
        {
            Status : AzureAsyncOperation
            RawResponseContent : string
            Output : 'T
        }

    /// Return None if the operation is still in progress, or the terminal status if the operation has completed
    let tryGetCompletedAsyncOperation (asyncOperation:AsyncOperationStatusOutput<'T>) =
        if Seq.contains asyncOperation.Status.Status AzureAsyncOperation.TerminalStatuses then
            Some asyncOperation
        else
            None

    /// Poll azure for a request to complete
    let waitForRequestToCompleteAsync (request:AsyncOperationRequest) (timeout:TimeSpan) (getOperationStatusAsync:AsyncOperationUrl -> Async<AsyncOperationStatusOutput<'T>>) =
        let interval =
            request.PollingInterval
            |> Option.orDefault Constants.PollingInterval
        fun () ->
            getOperationStatusAsync request.AsyncStatusUrl
            |> Async.map tryGetCompletedAsyncOperation
        |> Async.retryUntilSome timeout interval

    /// Deserialize the specified content to type ^C
    let inline deserializeContent< ^C> tags (client:IAzureClient) description content =
        if String.IsNullOrWhiteSpace content then
            TraceTags.failwith (sprintf "Could not retrieve content to be parsed (%s)" description) tags
        else
            try
                JObject.Parse(content).ToObject< ^C>(JsonSerializer.Create(client.DeserializationSettings))
            with
                :? JsonException as e ->
                    TraceTags.failwith (sprintf "Could not parse content for %s" description)
                                       (tags @[
                                            "content", content
                                            "exception", e.ToString()])

    /// <summary>
    /// Gets the status of a long running Azure async operation
    /// </summary>
    /// <param name="client">IAzureClient</param>
    /// <param name="operationUrl">URL of the resource.</param>
    /// <param name="customHeaders">Headers that will be added to request</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Status of the async operation and output of the operation</returns>
    let inline getAsyncOperationStatusWithOutput< ^T when ^T : null> tags (client:IAzureClient) (asyncOperationRequestUrl:AsyncOperationUrl) =
        async {
            let cancellationToken = Async.DefaultCancellationToken

            // Validate
            if isNull asyncOperationRequestUrl then
                TraceTags.failwith "OperationUrl is null" tags

            // Create HTTP transport objects
            use httpRequest =
                new HttpRequestMessage(
                    Method = HttpMethod.Get,
                    RequestUri = Uri(asyncOperationRequestUrl.Replace(" ", "%20")))

            // Set Credentials
            if isNull client.Credentials then
                TraceTags.failwith "Credentials information missing from Azure client" tags

            do! client.Credentials.ProcessHttpRequestAsync(httpRequest, cancellationToken).AsAsync

            // Send Request
            let! httpResponse = client.HttpClient.SendAsync(httpRequest, cancellationToken).AsAsync
            let! responseContent = httpResponse.Content.ReadAsStringAsync().AsAsync

            match httpResponse.StatusCode with
            | HttpStatusCode.OK
            | HttpStatusCode.Accepted
            | HttpStatusCode.Created
            | HttpStatusCode.NoContent ->
                return
                    {
                        AsyncOperationStatusOutput.RawResponseContent = responseContent
                        AsyncOperationStatusOutput.Status = deserializeContent tags client "status of long-running operation" responseContent
                        AsyncOperationStatusOutput.Output = deserializeContent< ^T> tags client "output of long-running operation" responseContent
                    }

            | statusCode ->
                let errorBody =
                    try
                        Microsoft.Rest.Serialization.SafeJsonConvert.DeserializeObject<CloudError>(responseContent, client.DeserializationSettings)
                    with
                    :? JsonException -> null // failed to deserialize, return empty body

                return raise <|
                        CloudException(
                            String.Format(System.Globalization.CultureInfo.InvariantCulture, "Long running operation failed with status '{0}'", statusCode),
                            Body = errorBody,
                            Request = Microsoft.Rest.HttpRequestMessageWrapper(httpRequest, null),
                            Response = Microsoft.Rest.HttpResponseMessageWrapper(httpResponse, responseContent))
        }