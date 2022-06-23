/// This module implements a lightweight Http server using HttpListener.
/// The purpose of the server is to enable inter-process communication
module Microsoft.FSharpLu.HttpCommunication

open System
open System.Net
open System.Threading
open Microsoft.FSharpLu
open Microsoft.FSharpLu.Async
open Microsoft.FSharpLu.Logging
open System.Net.Http
open System.Runtime.Serialization


module Constants =
    let CorrelationIdKey = "CorrelationId"
    let CorrelationIdHeader = "x-ms-sf-correlationId"
    let AppInsightsClientKey = "AppInsightsClientKey"
    let JsonMediaType = "application/json"

    [<Literal>]
    /// Have to define the TooManyRequests status code since this particular status code is not defined as part of the HttpStatusCode enum.
    /// (http://referencesource.microsoft.com/#System/net/System/Net/HttpStatusCode.cs,9b95c882b40ef96e)
    let TooManyRequestHttpStatus = 429

let encodeText (text:string) (encoding:System.Text.Encoding) =
    encoding.GetBytes(text)

let decodeContent (content:byte[]) (encoding:System.Text.Encoding) =
    encoding.GetChars(content)

module ErrorHandling =

    // The api guidelines https://github.com/Microsoft/api-guidelines/blob/vNext/Guidelines.md
    // recommends that a standard structure be used for returning errors. These types define that
    // standard structure.
    // The structure will be deserialized into json and returned in the error response body.
    [<DataContract>]
    type ApiInnerErrorCode =
        // Inner error codes can be extended without breaking the version contract
        | [<EnumMember>] NoJobExecutionStage = 0

    // These error codes must remain stable. Adding/Subtracting forces a new version of the api.
    [<DataContract>]
    type ApiErrorCode =
        /// Trying to do something that is not allowed
        | [<EnumMember>] Conflict = 0

        /// A bad parameter was handed to us
        | [<EnumMember>] BadParameter = 1

        /// There is a problem with the specified job
        | [<EnumMember>] JobError = 2

        /// An internal dependency on an Api failed. For example Azure storage, etc.
        | [<EnumMember>] InternalApiError = 3

        /// When checking for Api version if the requested version is unsupported
        // This value was choosen for comapatibility with the asp.net versioning API
        | [<EnumMember>] UnsupportedApiVersion = 4

        /// Returned if there is something wrong is detected with the query string parameter
        | [<EnumMember>] InvalidQueryStringParameter = 5

        /// Unauthorized user
        | [<EnumMember>] Unauthorized = 6

        /// A quota as been reached
        | [<EnumMember>] QuotaReached = 7

        /// A quota as been reached
        | [<EnumMember>] ScannerError = 8


    /// Inner error can be used for cascading exception handlers or where there are field validations
    /// and multiple errors need to be returned.
    [<DataContract>]
    type InnerError =
        {
            /// Inner detailed message
            [<DataMember>]
            Message : string
       }

    [<DataContract>]
    type ApiErrors =
        {
            /// The main error encountered
            [<DataMember>]
            Code : ApiErrorCode

            /// A detail string that can be used for debugging
            [<DataMember>]
            Message : string

            /// Function name that generated the error
            [<DataMember(IsRequired = false)>]
            Target : string

            /// An array of details about specific errors that led to this reported error.
            [<DataMember(IsRequired = false)>]
            Details : ApiErrors[]

            /// An object containing more specific information than the current object about the error.
            [<DataMember(IsRequired = false)>]
            InnerError : InnerError

        }

    /// The guidelines specify that the top level structure has only this one member.
    [<DataContract>]
    type ApiError =
        {
            /// Main field for errors.
            [<DataMember>]
            Error : ApiErrors
        }

/// Simple Http server intended for inter-process communication
module Server =
    open System.Text

    let getRequestContentBytes (req:HttpListenerRequest) =
        let contentLength =
            try
                let length = System.Convert.ToInt32 req.ContentLength64

                if length < 0 then
                    // The `HttpListenerRequest` class has a content length value of -1
                    // if the length is unknown.
                    //
                    // We continue here, because either our reader will fail due to the
                    // negative read count, and we'll catch it anyway, or it won't need
                    // the exact content length to read the content stream.
                    Trace.warning "HTTP request content length is unknown"

                Ok length
            with
            | exn ->
                Trace.error "Error downcasting request content length: %O" exn
                Error exn

        let readContentBytes count =
            try
                use reader = new System.IO.BinaryReader(req.InputStream, req.ContentEncoding)
                let contentBytes = reader.ReadBytes(count)

                let readLen = contentBytes.Length
                if count <> readLen then
                    Trace.warning "Expected content length of %i, read %i bytes" count readLen

                Ok contentBytes
            with
            | exn ->
                Trace.error "Error reading request body: %O" exn
                Error exn

        contentLength |> Result.bind readContentBytes

    let getRequestContent (req:HttpListenerRequest) =
        getRequestContentBytes req
        |> Result.bind (fun bytes -> String(decodeContent bytes req.ContentEncoding) |> Ok)

    let getRelativeUri (hostUri:Uri) (req:HttpListenerRequest) =
        hostUri.MakeRelativeUri(req.Url).OriginalString

    let newCorrelationId() =
        Guid.NewGuid().ToString("N")

    /// Extract the correlationId that is stored in the HTTP request properties by the CorrelationId filter, create
    /// one if the property is not found.
    let getCorrelationId (request:HttpRequestMessage) =
    #if netstandard20 || NET6_0
        let success, value = request.Options.TryGetValue(HttpRequestOptionsKey(Constants.CorrelationIdKey))
    #else
        let success, value = request.Properties.TryGetValue(Constants.CorrelationIdKey)
    #endif
        if success then
            value.ToString()
        else
            // I expect that this class is used after an incoming http request, so I don't expect
            // this to happen.
            Trace.warning "A CorrelationId was not found in the header properties, this should not happen."
            newCorrelationId()

    /// Encapsulates Http listener with cancellation via IDisposable interface.
    type Listener =
        {
            cancellationTokenSource:CancellationTokenSource
            listener : HttpListener
        }
        interface System.IDisposable with
            member x.Dispose() =
                        x.cancellationTokenSource.Cancel()
                        x.listener.Stop()
                        x.listener.Close()
                        (x.listener :> IDisposable).Dispose()


    /// Starts listening for client requests on the specified Uri and handling requests via the specified handler.
    /// Returns an IDisposable listener instance that the caller is responsible for disposing.
    let listener (hostUri:Uri)
                 (cancellationTokenSource:CancellationTokenSource)
                 (handler:(HttpListenerRequest->HttpListenerResponse->Async<unit>)) =
        let hl = new HttpListener()

        hl.Prefixes.Add hostUri.AbsoluteUri
        Trace.info "Listener starting on %s" hostUri.AbsoluteUri

        hl.Start()

        let getContext() = Async.FromBeginEnd(hl.BeginGetContext, hl.EndGetContext)

        let runServer = async {
            let! token = Async.CancellationToken
            while not token.IsCancellationRequested && hl.IsListening do
                // Get the next request
                let! context = getContext()

                Trace.info "Server processing new request."

                let safeHandler =
                    async {
                        try
                            do! handler context.Request context.Response
                        with
                        | :? HttpListenerException as e ->
                            Trace.error "Exception occurred when processing the HttpListener request: %O" e
                    }

                // Handle the request
                // TODO (marinapo): add tracing for task cancellation.
                Async.Start(safeHandler, token)
        }
        Async.Start (runServer, cancellationToken = cancellationTokenSource.Token)
        { listener = hl ; cancellationTokenSource = cancellationTokenSource}

    let sendResponse (resp:HttpListenerResponse) (statusCode:HttpStatusCode) (responseData:string option) =
        resp.ContentType <- "text/html"
        resp.StatusCode <- statusCode |> int
        match responseData with
        | Some s ->
            let contentBytes = encodeText s Encoding.UTF8
            resp.OutputStream.Write(contentBytes, 0, contentBytes.Length)
        | None -> ()
        // Dipose the stream to send the response to the client
        // Note: calling resp.OutputStream.Close() before resp.OutputStream.Dispose()
        // would result in the following exception
        // on Linux:
        //  Unhandled Exception: System.ObjectDisposedException: Cannot access a disposed object.
        //  Object name: 'System.Net.HttpListenerResponse'.
        resp.OutputStream.Dispose()
        resp.Close()

/// Helper module for sending client requests to the above Http server.
module Client =
    open Newtonsoft.Json
    open System.Net.Http.Headers
    open System.Threading.Tasks
    open System.Text

    let sendRequest hostUri (requestText:string) (relativeUri: string) =
        use client = new System.Net.WebClient()
        let requestContent = encodeText requestText Encoding.UTF8
        let address = Uri(hostUri, relativeUri).AbsoluteUri

        let responseContent = client.UploadData(address, requestContent)
        String(decodeContent responseContent Encoding.UTF8)

    type HttpRequestOptions =
        {
            Timeout : TimeSpan
            MaxRetryCount : int
            RetryInterval : TimeSpan
            Certificate : System.Security.Cryptography.X509Certificates.X509Certificate2 option
            ServerEndpoint : string
            CorrelationId : string
        }

        static member Default =
            {
                Timeout = TimeSpan.FromMinutes(20.0)
                MaxRetryCount = 10
                RetryInterval = TimeSpan.FromSeconds(15.0)
                Certificate = None
                ServerEndpoint = String.Empty
                CorrelationId = String.Empty
            }

    let timeoutFromRetryCount (retryInterval:TimeSpan) retryCount =
        TimeSpan.FromSeconds(retryInterval.TotalSeconds * (float retryCount))

    /// Generic error wrapper for failures returned from the request
    exception CommunicationException of string * HttpStatusCode with
        override this.Message =
            sprintf "%A" this

    /// Catch ApiErrors from the request
    exception ApiErrorException of HttpStatusCode * ErrorHandling.ApiError with
        override this.Message =
            sprintf "%A" this
        static member StatusCode = HttpStatusCode.Conflict

    /// The requested service is experiencing too many requests
    exception TooManyRequestsException of string with
        override this.Message =
            sprintf "%A" this
        static member StatusCode: HttpStatusCode = enum Constants.TooManyRequestHttpStatus

    /// Request timeout from the request
    exception RequestTimeoutException of string with
        override this.Message =
            sprintf "%A" this
        static member StatusCode = System.Net.HttpStatusCode.RequestTimeout

    /// Internal Server Error from the request
    exception InternalServerError of string with
        override this.Message =
            sprintf "%A" this
        static member StatusCode = System.Net.HttpStatusCode.InternalServerError

    /// Precondition Failed Server Error from the request
    exception PreconditionFailedServerError of string with
        override this.Message =
            sprintf "%A" this
        static member StatusCode = System.Net.HttpStatusCode.PreconditionFailed

    /// Resource Not found
    exception ResourceNotFoundException of string with
        override this.Message =
            sprintf "%A" this
        static member StatusCode = System.Net.HttpStatusCode.NotFound

    /// If the stream length changes in the posting process (because another thread
    /// is actively tracing), it can throw this exception
    exception ProtocolViolationException of string with
        override this.Message =
                sprintf "%A" this
        static member StatusCode = System.Net.HttpStatusCode.ResetContent

    /// Bad Request Error when input is invalid
    exception BadRequestError of string with
        override this.Message =
            sprintf "%A" this
        static member StatusCode = System.Net.HttpStatusCode.BadRequest

    let createHttpClient (httpOptions: HttpRequestOptions) =
        ServicePointManager.SecurityProtocol <- SecurityProtocolType.Tls12 ||| SecurityProtocolType.Tls11 ||| SecurityProtocolType.Tls
        let client =
            match httpOptions.Certificate with
            | Some certificate ->
                // The default server certificate validation will cause in some cases "SslPolicyErrors.RemoteCertificateNameMismatch" because
                // of a conflict with the client certificate even when the server certificate is valid. This behavior occurs when the backend web app is deployed on a large instance
                // we configure the server certificate validation to only use the certificate chain and ignore the certificate name mismatch.
// #if NET472
//                 let clientHandler = new WebRequestHandler()
//                 clientHandler.ServerCertificateValidationCallback <-
//                     fun o certificate chain sslPolicyErrors -> chain.Build (certificate:?> System.Security.Cryptography.X509Certificates.X509Certificate2)
// #else
  //  #if NETSTANDARD2_0
                let clientHandler = new HttpClientHandler(ClientCertificateOptions = ClientCertificateOption.Manual)
                clientHandler.ServerCertificateCustomValidationCallback <-
                    fun o certificate chain sslPolicyErrors -> chain.Build certificate
  //  #else
    //            raise <| System.NotImplementedException("createHttpClient not implemented for this framework.")
  //  #endif
//#endif

                clientHandler.ClientCertificates.Add(certificate) |> ignore
                new HttpClient(clientHandler,
                            BaseAddress = Uri(httpOptions.ServerEndpoint),
                            Timeout = httpOptions.Timeout)
            | None ->
                new HttpClient(
                                BaseAddress = Uri(httpOptions.ServerEndpoint),
                                Timeout = httpOptions.Timeout
                            )
        client.DefaultRequestHeaders.ConnectionClose <- (System.Nullable<bool> true)

        let correlationId =
            if String.IsNullOrWhiteSpace(httpOptions.CorrelationId) then
                Server.newCorrelationId()
            else
                httpOptions.CorrelationId

        client.DefaultRequestHeaders.Add(Constants.CorrelationIdHeader, correlationId)
        client.DefaultRequestHeaders.Accept.Clear()
        client.DefaultRequestHeaders.Accept.Add(MediaTypeWithQualityHeaderValue(Constants.JsonMediaType))
        client

    module Methods =
        // Add extension method for Patch method. It's not provided in the standard .NET library.
        type System.Net.Http.HttpClient with
            member  __.PatchAsJsonAsync<'T> (uri:string, value:'T) =
                let data = JsonConvert.SerializeObject value
                let stringContent = new StringContent(data, null, Constants.JsonMediaType)

                let request = new HttpRequestMessage(method = HttpMethod("PATCH"), requestUri = uri, Content = (stringContent :> HttpContent))
                __.SendAsync(request)

            member  __.HeadAsAsync<'T> (uri:string) =
                let request = new HttpRequestMessage(method = HttpMethod("HEAD"), requestUri = uri)
                __.SendAsync(request)

        let tryParseApiError (responseMessage:string) =
            let s = JsonSerializerSettings(NullValueHandling = NullValueHandling.Ignore)
            s.Converters.Add(Converters.StringEnumConverter())
            try
                let apiError = JsonConvert.DeserializeObject<ErrorHandling.ApiError>(responseMessage, s)
                if obj.ReferenceEquals(apiError, null) then
                    None
                else
                    Some apiError
            with | _ -> None

        /// Asynchrosnously retrieve data from an HTTP response message
        let getDataAsync<'a> (response:HttpResponseMessage) =
                async {
                    Trace.verbose "get data for request: %O" response.RequestMessage.RequestUri
                    if (not <| isNull response) && response.IsSuccessStatusCode then
                        if typeof<'a> = typeof<unit> then
                            return (() :> obj) :?> 'a |> Result.Ok
                        // String is special case that is not handled by ReadAsAsync()
                        elif typeof<'a> = typeof<string>
                            && response.Content.Headers.ContentType.MediaType.IndexOf(Constants.JsonMediaType, StringComparison.OrdinalIgnoreCase) < 0 then
                            let! content = response.Content.ReadAsStringAsync () |> Async.AwaitTask
                            return (content :> obj) :?> 'a |> Result.Ok
                        elif typeof<'a> = typeof<System.IO.Stream> then
                            let! content = response.Content.ReadAsStreamAsync () |> Async.AwaitTask
                            return (content :> obj) :?> 'a |> Result.Ok
                        else
                            let! content = response.Content.ReadAsAsync() |> Async.AwaitTask
                            return content |> Result.Ok
                    else
                        let! errorMessage = response.Content.ReadAsStringAsync() |> Async.AwaitTask
                        return Result.Error (int response.StatusCode, errorMessage)
                }

        let private queryServer (errorHandler) (queryServer:unit -> Task<HttpResponseMessage>) queryMessage: Async<'a> =
            async {
                let! response = queryServer().AsAsync
                Trace.verbose "query succeeded with response: %s" queryMessage
                let! data = getDataAsync<'a> response
                match data with
                | Result.Ok r ->
                    return r
                | Result.Error e ->
                    return! errorHandler response
            }

        let private queryServerWithRetry errorHandler (retryCount, retryInterval) query queryMessage  =
            async {
                let q() =
                    async {
                        try
                            let! q = queryServer errorHandler query queryMessage
                            return Some q
                        with
                        | ProtocolViolationException _
                        | InternalServerError _
                        | RequestTimeoutException _ -> return None
                    }

                let timeout = timeoutFromRetryCount retryInterval retryCount
                let! r = Async.retryUntilSomeOrTimeout timeout retryInterval q
                match r with
                | None -> return TimeoutException (sprintf "Did not receive response after %d seconds" (int timeout.TotalSeconds)) |> raise
                | Some r -> return r
            }

        let toHttpResponseMessage (response: HttpWebResponse) =
            let responseMessage = new HttpResponseMessage(HttpStatusCode.OK)
            responseMessage.Content <- new StreamContent(response.GetResponseStream())
            responseMessage.Content.Headers.ContentType <- MediaTypeHeaderValue response.ContentType
            responseMessage.Content.Headers.ContentDisposition <- ContentDispositionHeaderValue.Parse(response.Headers.Get("Content-Disposition").Replace(',', ';'))
            responseMessage

        ////// Error Handlers
        let defaultErrorHandler (response: HttpResponseMessage) =
            async {
                let! errorMessage = response.Content.ReadAsStringAsync().AsAsync
                let message = sprintf "%s -- Failure %A -- %s" (response.RequestMessage.RequestUri.ToString()) response.StatusCode errorMessage
                Trace.error "%s" message

                let apiErrorOption = tryParseApiError errorMessage
                match apiErrorOption with
                | Some apiError -> return raise <| ApiErrorException (response.StatusCode, apiError)
                | None ->
                    return raise <|
                        match response.StatusCode with
                        | _ when response.StatusCode = TooManyRequestsException.StatusCode -> TooManyRequestsException message
                        | _ when response.StatusCode = InternalServerError.StatusCode -> InternalServerError message
                        | _ when response.StatusCode = PreconditionFailedServerError.StatusCode -> System.Exception(errorMessage)
                        | _ when response.StatusCode = RequestTimeoutException.StatusCode -> TimeoutException message :> exn
                        | _ when response.StatusCode = BadRequestError.StatusCode -> BadRequestError errorMessage
                        | _ -> CommunicationException (message, response.StatusCode)
            }

        let defaultWebResponseErrorHandler (response: HttpWebResponse) query =
            async {
                let message = sprintf "%s response failure %s -- status code : %O" response.Method query response.StatusCode
                Trace.error "%s" message
                return CommunicationException (message, response.StatusCode) |> raise
            }

        ////// HTTP GET
        let httpGetWithHandler, httpGetWithRetryWithHandler =
            let q (httpClient:HttpClient) (query:string) = (fun () -> query |> httpClient.GetAsync), (sprintf "GET  %s" query)

            let get errorHandler (httpClient:HttpClient) query =
                async {return! (q httpClient query) ||> queryServer errorHandler}

            let getWithRetry errorHandler (httpClient:HttpClient) retry query =
                async {return! (q httpClient query) ||> (queryServerWithRetry errorHandler retry)}

            get, getWithRetry

        let httpGet client query = httpGetWithHandler defaultErrorHandler client query
        let httpGetWithRetry client retry query = httpGetWithRetryWithHandler defaultErrorHandler client retry query

        let httpGetf client query = Printf.kprintf (httpGet client) query
        let httpGetWithRetryf client retry query = Printf.kprintf (httpGetWithRetry retry client) query

        let httpGetWithHandlerf errorHandler client query = Printf.kprintf (httpGetWithHandler errorHandler client) query
        let httpGetWithRetryWithHandlerf errorHandler client retry query = Printf.kprintf (httpGetWithRetryWithHandler errorHandler client retry) query

        ////// HTTP GET - WebResponse
        let httpGetWebResponseWithHandler errorHandler (httpOptions: HttpRequestOptions) (query:string) =
            async {
                let request = WebRequest.Create(httpOptions.ServerEndpoint + query) :?> HttpWebRequest
                request.Method <- WebRequestMethods.Http.Get
                request.Accept <- Constants.JsonMediaType
                request.Headers.Add(Constants.CorrelationIdHeader, httpOptions.CorrelationId)

                httpOptions.Certificate
                |> Option.map request.ClientCertificates.Add
                |> ignore
                let! r = request.GetResponseAsync() |> Async.AwaitTask
                let response = r :?> HttpWebResponse
                if response.StatusCode = HttpStatusCode.OK then
                    Trace.info "%s response Success  %s" response.Method query
                    return response
                else return! errorHandler response query
            }

        let httpGetWebResponse (httpOptions: HttpRequestOptions) (query:string) = httpGetWebResponseWithHandler defaultWebResponseErrorHandler httpOptions query

        ////// HTTP GET - HttpResponseMessage
        let httpGetResponseMessage (httpOptions: HttpRequestOptions) (query:string) =
            async {
                let! response = httpGetWebResponse httpOptions query
                return response |> toHttpResponseMessage
            }

        ////// HTTP GET - Stream
        let httpGetStream (httpOptions: HttpRequestOptions) (query:string) =
            async {
                let! response = httpGetWebResponse httpOptions query
                return response.GetResponseStream()
            }

        ////// HTTP POST
        let httpPostWithHandler, httpPostWithRetryWithHandler =
            let q (httpClient:HttpClient) (query:string) data =
                (fun () -> (query, new StringContent(JsonConvert.SerializeObject data, null, Constants.JsonMediaType)) |> httpClient.PostAsync), (sprintf "POST  %s" query)

            let post errorHandler httpClient query data =
                async {return! (q httpClient query data) ||> queryServer errorHandler}

            let postWithRetry errorHandler httpClient retry query data =
                async {return! (q httpClient query data) ||> (queryServerWithRetry errorHandler retry)}

            post, postWithRetry

        let httpPost client query = httpPostWithHandler defaultErrorHandler client query
        let httpPostWithRetry client retry query = httpPostWithRetryWithHandler defaultErrorHandler client retry query

        let httpPostf client query = Printf.kprintf (httpPost client) query
        let httpPostWithRetryf errorHandler retry client query = Printf.kprintf (httpPostWithRetry errorHandler client retry) query

        let httpPostWithHandlerf errorHandler client query = Printf.kprintf (httpPostWithHandler errorHandler client) query
        let httpPostWithRetryWithHandlerf errorHandler client retry query = Printf.kprintf (httpPostWithRetryWithHandler errorHandler client retry) query

        ////// POST - FromStream
        let httpPostFromStreamWithHandler, httpPostFromStreamWithRetryWithHandler =
            let q (httpClient:HttpClient) (query:string) (data:System.IO.Stream) =
                (fun () -> (query, new StreamContent (data)) |> httpClient.PostAsync), (sprintf "POST  %s" query)

            let post errorHandler httpClient query data =
                async {return! (q httpClient query data) ||> queryServer errorHandler}

            let postWithRetry errorHandler httpClient retry query data =
                async {return! (q httpClient query data) ||> queryServerWithRetry errorHandler retry}

            post, postWithRetry

        let httpPostFromStream client query = httpPostFromStreamWithHandler defaultErrorHandler client query
        let httpPostFromStreamWithRetry client retry query = httpPostFromStreamWithRetryWithHandler defaultErrorHandler client retry query

        let httpPostFromStreamf client query = Printf.kprintf (httpPostFromStreamWithHandler defaultErrorHandler client) query
        let httpPostFromStreamWithRetryf client retry query = Printf.kprintf (httpPostFromStreamWithRetryWithHandler defaultErrorHandler client retry) query

        let httpPostFromStreamWithHandlerf errorHandler client query = Printf.kprintf (httpPostFromStreamWithHandler errorHandler client) query
        let httpPostFromStreamWithRetryWithHandlerf errorHandler client retry query = Printf.kprintf (httpPostFromStreamWithRetryWithHandler errorHandler client retry) query

        ////// POST - WebResponse
        let httpPostWebResponseWithHandler errorHandler (httpOptions: HttpRequestOptions) query data =
            async {
                let request = WebRequest.Create(httpOptions.ServerEndpoint + query) :?> HttpWebRequest
                request.Method <- WebRequestMethods.Http.Post
                request.Accept <- Constants.JsonMediaType
                request.Headers.Add(Constants.CorrelationIdHeader, httpOptions.CorrelationId)

                httpOptions.Certificate
                |> Option.map request.ClientCertificates.Add
                |> ignore
                let content = Encoding.UTF8.GetBytes(JsonConvert.SerializeObject(data))
                request.ContentLength <- int64 content.Length

                let dataStream = request.GetRequestStream()
                do dataStream.Write(content, 0, content.Length)
                do dataStream.Close()

                let! r = request.GetResponseAsync() |> Async.AwaitTask
                let response = r :?> HttpWebResponse
                if response.StatusCode = HttpStatusCode.OK then
                    Trace.info "%s response Success  %s" response.Method query
                    return response
                else return! errorHandler response query
        }

        let httpPostWebResponse (httpOptions: HttpRequestOptions) query data = httpPostWebResponseWithHandler defaultWebResponseErrorHandler httpOptions query data

        ////// POST - HttpResponseMessage
        let httpPostResponseMessage (httpOptions: HttpRequestOptions) (query:string) data =
            async {
                let! response = httpPostWebResponse httpOptions query data
                return response |> toHttpResponseMessage
            }

        ////// POST - Stream
        let httpPostStream (httpOptions: HttpRequestOptions) (query:string) data =
            async {
                let! response = httpPostWebResponse httpOptions query data
                return response.GetResponseStream()
            }

        ////// HTTP PUT
        let httpPutWithHandler, httpPutWithRetryWithHandler =
            let q (httpClient : HttpClient) (query: string) data =
                (fun () -> (query, new StringContent(JsonConvert.SerializeObject data, null, Constants.JsonMediaType)) |> httpClient.PutAsync), (sprintf "PUT  %s" query)

            let put errorHandler httpClient query data =
                async {return! (q httpClient query data) ||> queryServer errorHandler}

            let putWithRetry errorHandler httpClient retry query data =
                async {return! (q httpClient query data) ||> (queryServerWithRetry errorHandler retry)}

            put, putWithRetry

        let httpPut client query = httpPutWithHandler defaultErrorHandler client query
        let httpPutWithRetry client retry query =  httpPutWithRetryWithHandler defaultErrorHandler client retry query

        let httpPutf client query = Printf.kprintf (httpPut client) query
        let httpPutWithRetryf client retry query = Printf.kprintf (httpPutWithRetry client retry) query

        ////// HTTP HEAD
        let httpHeadWithHandler =
            let q (httpClient:HttpClient) (query:string) = (fun () -> query |> httpClient.HeadAsAsync)

            let head (httpClient:HttpClient) (query:string) =
                async {return! (query |> httpClient.HeadAsAsync).AsAsync}
            head

        let httpHead client query = httpHeadWithHandler client query
        let httpHeadf client query = Printf.kprintf (httpHead client) query

        ////// HTTP PATCH
        let httpPatchWithHandler, httpPatchWithRetryWithHandler =
            let q (httpClient : HttpClient) (query: string) data =
                (fun () -> (query, data) |> httpClient.PatchAsJsonAsync), (sprintf "PATCH  %s" query)

            let patch errorHandler httpClient query data =
                async {return! (q httpClient query data) ||> queryServer errorHandler}

            let patchWithRetry errorHandler httpClient retry query data =
                async {return! (q httpClient query data) ||> (queryServerWithRetry errorHandler retry)}

            patch, patchWithRetry

        let httpPatch client query = httpPatchWithHandler defaultErrorHandler client query
        let httpPatchWithRetry client retry query = httpPatchWithRetryWithHandler defaultErrorHandler client retry query

        let httpPatchf client query = Printf.kprintf (httpPatch client) query
        let httpPatchWithRetryf client retry query = Printf.kprintf (httpPatchWithRetry client retry) query

        ////// HTTP DELETE
        let private httpDeleteQuery (httpClient : HttpClient) (query: string) =
            (fun() -> query |> httpClient.DeleteAsync), (sprintf "DELETE  %s" query)

        let httpDeleteWithHandler errorHandler httpClient query =
            httpDeleteQuery httpClient query ||> queryServer errorHandler

        let httpDeleteWithRetryWithHandler errorHandler httpClient retry query =
            httpDeleteQuery httpClient query ||> queryServerWithRetry errorHandler retry

        let httpDelete client query = httpDeleteWithHandler defaultErrorHandler client query
        let httpDeleteWithRetry client retry query = httpDeleteWithRetryWithHandler defaultErrorHandler client retry query

        let httpDeletef client query = Printf.kprintf (httpDelete client) query
        let httpDeleteWithRetryf client retry query = Printf.kprintf (httpDeleteWithRetry client retry) query

module Utils =
    let private tryPickException<'a when 'a :> Exception> = ErrorHandling.matchAggregatedException (fun ex -> match ex with :? 'a as e -> Some e | _ -> None)

    /// Wheter or not Internet can be accessed
    type InternetAccessResult =
    | Access of string
    | NoAccess of string
    | Undetermined of string

    /// Perform HTTP GET on a collection of URLs. Return collection of booleans to signal whether GET was successful or not.
    let hasInternetAccess (urlsToPing: string list) =
        urlsToPing
        |> List.map (fun url ->
            async {
                let errMsg = sprintf "Access to Internet is undetermined: access to the URL %s failed but the exception is unrecognized." url

                try
                    use client = new System.Net.Http.HttpClient()
                    let! r = url |> client.GetAsync |> Async.AwaitTask
                    return Access (sprintf "Successfully accessed: %s with status code %A" url r.StatusCode)
                with
                | :? System.AggregateException as aggregateExn ->
                    match tryPickException<System.Net.Http.HttpRequestException> aggregateExn with
                    | Some httpRequestExn
                         when httpRequestExn.Message = "An error occurred while sending the request."
                              || httpRequestExn.Message.Contains("A connection attempt failed because the connected party did not properly respond after a period of time") ->
                        return
                            match httpRequestExn.InnerException with
                            | :? System.Net.WebException as webExn
                                    when webExn.Message.StartsWith("The remote name could not be resolved:")
                                        || webExn.Message.Contains("Unable to connect to the remote server") ->
                                NoAccess "No internet access (System.Net.WebException)"

                            | :? System.Net.WebException as webExn ->
                                Undetermined (sprintf "%s Expected System.AggregateException -> System.Net.Http.HttpRequestException('An error occurred while sending the request') -> System.Net.WebException('The remote name could not be resolved' or 'Unable to connect to the remote server') but got: 1) %A\n 2) %A\n 3) %A" errMsg aggregateExn httpRequestExn webExn)

                            | :? System.Net.Sockets.SocketException as socketExn
                                    when socketExn.Message.Contains("A connection attempt failed because the connected party did not properly respond after a period of time") ->
                                NoAccess "No internet access (System.Net.Sockets.SocketException)"

                            | _ when httpRequestExn.InnerException.GetType().FullName = "System.Net.Http.WinHttpException"
                                 && httpRequestExn.InnerException.Message.StartsWith("The operation timed out") ->
                                NoAccess "No internet access (System.Net.Http.WinHttpException)"

                            | _ ->
                                Undetermined (sprintf "%s Expected a System.AggregateException -> System.Net.Http.HttpRequestException('An error occurred while sending the request') -> System.Net.WebException but got: 1) %A\n 2) %A" errMsg aggregateExn httpRequestExn)

                    | Some httpRequestExn ->
                        return Undetermined (sprintf "%s Expected a System.AggregateException -> System.Net.Http.HttpRequestException('An error occurred while sending the request') but got: 1) %A\n 2) %A" errMsg aggregateExn httpRequestExn)

                    | None ->
                        return Undetermined (sprintf "%s Expected a System.AggregateException -> System.Net.Http.HttpRequestException but got: %A" errMsg aggregateExn)

                | :? System.Threading.Tasks.TaskCanceledException ->
                    return NoAccess "Task Cancelled"
                | ex ->
                    return Undetermined (sprintf "%s Expected a System.AggregateException but got: %A" errMsg ex)
            }
        )

    /// Stream helper for reading client stream
    /// This is needed because the read stream function's some internal states
    /// are not correctly set when getting a stream from a url
    type ReadOnlyClientStream(stream:IO.Stream, size, disposeInnerSream) =
        inherit IO.Stream()
        let mutable position = 0L
        override this.CanRead         = stream.CanRead
        override this.CanSeek         = true
        override this.CanWrite        = false
        override this.Length          = size
        override this.Position
                with get ()           = position
                and set value         = position <- value
        override this.Flush ()        = stream.Flush()
        override this.Seek (ofs,orig) = NotImplementedException "" |> raise
        override this.SetLength x     = NotImplementedException "" |> raise
        override this.Read (b,o,c)    =
            let r = stream.Read(b,o,c)
            position <- position + (int64 r)
            r
        override this.Write (b,o,c)   = NotImplementedException "" |> raise
        override this.Close ()        = stream.Close()
        override this.Dispose (disp)  = if disp && disposeInnerSream then stream.Dispose()
                                        base.Dispose disp

        static member FromHttpResponseMessage (response: System.Net.Http.HttpResponseMessage) =
            async {
                if response.IsSuccessStatusCode then
                    let fileSize = response.Content.Headers.ContentLength.Value
                    let! s = response.Content.ReadAsStreamAsync() |> Async.AwaitTask
                    return new ReadOnlyClientStream(s, fileSize, true)
                else
                    let! errorMessage = response.Content.ReadAsStringAsync() |> Async.AwaitTask
                    let message = sprintf "Status code: %A --  message: %s" response.StatusCode errorMessage
                    return Exception message |> raise
            }
