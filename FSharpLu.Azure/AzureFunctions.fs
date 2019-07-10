module Microsoft.FSharpLu.Azure.AzureFunctions

open System
open System.Net
open System.Net.Http
open Microsoft.FSharpLu.Azure.Auth
open Microsoft.FSharpLu.Async
open Microsoft.FSharpLu
open Newtonsoft.Json

/// Web App publishing credentials datastructure returned by REST call to Azure Management API
type PublishingInfo =
    {
        /// username used for publishing Azure function zip package
        publishingUserName: string
        /// password used for publishing Azure function zip package
        publishingPassword: string
        /// Azure function URL used for Azure Function publishing and management
        scmUri: Uri
    }

// Get HTTP client used for managing Azure Function
let getHttpClient(publishingInfo: PublishingInfo) =
    let client = new HttpClient(BaseAddress = publishingInfo.scmUri)
    let authToken = System.Convert.ToBase64String(System.Text.Encoding.ASCII.GetBytes(sprintf "%s:%s" publishingInfo.publishingUserName publishingInfo.publishingPassword))
    client.DefaultRequestHeaders.Add("Authorization", "Basic " + authToken)
    client

/// Wrapper container needed to properly deserialize the response from Azure Management API
type Response =
    {
        /// Properties of the response with publishing information
        properties: PublishingInfo
    }

/// Retrieve publishing credentials from Azure Management API for the specific Azure Function App container
let getPublishingCredentials (subscription: Subscription) (resourceGroup: string) (functionAppsContainer: string) =
    async {
        let resource =
            sprintf "/subscriptions/%s/resourceGroups/%s/providers/Microsoft.Web/sites/%s/config/publishingcredentials/list?api-version=2016-08-01" subscription.SubscriptionId resourceGroup functionAppsContainer

        let! authToken = Microsoft.FSharpLu.Azure.Auth.getAuthorizationToken subscription.Authentication
        use httpClient =
            let c = new HttpClient(BaseAddress = Uri(Microsoft.FSharpLu.Azure.Constants.management))
            c.DefaultRequestHeaders.Add("Authorization", "Bearer " + authToken)
            c
        let! r = httpClient.PostAsync(resource, new StringContent("")).AsAsync
        let! responseJson = r.Content.ReadAsStringAsync().AsAsync
        match r.StatusCode with
        | HttpStatusCode.OK ->
            let response = Json.Default.deserialize<Response>(responseJson)
            return Result.Ok(response.properties)
        | anyOtherStatus ->
            return Result.Error(sprintf "Got %A intead of OK from Azure Management API while requesting publishing credentials for %s: %s" anyOtherStatus functionAppsContainer responseJson)
    }

/// Location of item to package as part of the Azure function and where to place it within the Azure Function package.
type PackageCopyItemsDefenition =
    {
        /// full path to the source file or directory to include in the package
        fullSourcePath: string
        /// relative location of the source file or directory within the package
        relativeDestinationPath: string
    }

/// Create Azure Function zip package in the required format from the the items deployed as part of the Azure function
let createAzureFunctionPackage (azureFunctionName: string) (itemsToPackage: PackageCopyItemsDefenition seq) =
    async {
        let tempDirectory =
            let tempFileName = IO.Path.GetTempFileName()
            let tempDirPath = IO.Path.Combine(IO.Path.GetTempPath(), IO.Path.GetFileNameWithoutExtension(tempFileName))
            IO.Directory.CreateDirectory(tempDirPath)

        let sourceDirectory =
            let path = IO.Path.Combine(tempDirectory.FullName, azureFunctionName)
            IO.Directory.CreateDirectory(path)

        let destinationZipPackage = IO.Path.Combine(tempDirectory.FullName, "azureFunctionPackage.zip")

        for item in itemsToPackage do
            let info = IO.DirectoryInfo(item.fullSourcePath)
            if info.Attributes &&& IO.FileAttributes.Directory = IO.FileAttributes.Directory then
                //directory, perform recursive copy
                Microsoft.FSharpLu.File.copyDirectory item.fullSourcePath (IO.Path.Combine(sourceDirectory.FullName, item.relativeDestinationPath))
            else
                //otherwise do file copy
                do! Microsoft.FSharpLu.Async.copyFile item.fullSourcePath (IO.Path.Combine(sourceDirectory.FullName, item.relativeDestinationPath))

        IO.Compression.ZipFile.CreateFromDirectory(sourceDirectory.FullName, destinationZipPackage, IO.Compression.CompressionLevel.Optimal, true)
        return destinationZipPackage
    }

/// Copy and deploy Azure function package to the SCM URI returned as part of the getPublishingCredentials call
let publishAzureFunctionPackage (publishingInfo: PublishingInfo) (zipPackagePath: string) =
    async {
        use httpClient = getHttpClient(publishingInfo)
        let! r = httpClient.PutAsync("/api/zip/site/wwwroot", new StreamContent(IO.File.OpenRead(zipPackagePath))).AsAsync
        match r.StatusCode with
        | HttpStatusCode.OK | HttpStatusCode.NoContent ->
            return Result.Ok()
        | anyOtherStatus ->
            let! responseContent = r.Content.ReadAsStringAsync().AsAsync
            return Result.Error (sprintf "Got %A intead of OK from Azure Management API while requesting publishing Azure FunctionPackage for %s: %s" anyOtherStatus zipPackagePath responseContent)
    }

/// Azure Function binding returned by Azure Function management API
type FunctionBinding =
    {
        /// name of the binding
        name: string
        /// binding type
        [<JsonProperty(PropertyName="type")>]
        bindingType: string
        /// direction: in/out
        direction: string
        /// name of the queue if any, otherwise it is null
        queueName: string
        /// type of the connection
        connection: string
        /// used if function is of http trigger type
        authLevel: string
    }

/// Azure function settings
type FunctionSettings =
    {
        /// collection of bindings assigned to the Azure function
        bindings: FunctionBinding list
        /// true if function is disabled, and false if function is enabled
        disabled: bool
    }

/// Get specified Azure function settings
let getFunctionSettings (publishingInfo: PublishingInfo) (functionName: string) =
     async {
        use httpClient = getHttpClient(publishingInfo)
        let! r = httpClient.GetAsync(sprintf "/api/vfs/site/wwwroot/%s/function.json" functionName).AsAsync
        let! responseContent = r.Content.ReadAsStringAsync().AsAsync
        match r.StatusCode with
        | HttpStatusCode.OK ->
            return Result.Ok(Json.Default.deserialize<FunctionSettings> responseContent)
        | anyOtherStatus ->
            return Result.Error (sprintf "Got %A instead of OK when getting %s Azure function settings: %A" anyOtherStatus functionName responseContent)
    }

/// Update specified Azure function settings
let updateFunctionSettings(publishingInfo: PublishingInfo) (functionName: string) (settings: FunctionSettings) =
    async {
        use httpClient = getHttpClient(publishingInfo)
        httpClient.DefaultRequestHeaders.Add("If-Match", "*")
        let! r = httpClient.PutAsync(sprintf "/api/vfs/site/wwwroot/%s/function.json" functionName, new Http.StringContent(Json.Default.serialize settings)).AsAsync
        match r.StatusCode with
        | HttpStatusCode.OK | HttpStatusCode.NoContent ->
            return Result.Ok()
        | anyOtherStatus ->
            let! responseContent = r.Content.ReadAsStringAsync().AsAsync
            return Result.Error (sprintf "Got %A instead of OK when updating %s Azure function settings: %A" anyOtherStatus functionName responseContent)
    }

/// Delete Azure Function
let deleteFunction(publishingInfo: PublishingInfo) (functionName: string) =
    async {
        use httpClient = getHttpClient(publishingInfo)
        let! r = httpClient.DeleteAsync(sprintf "/api/functions/%s" functionName).AsAsync
        match r.StatusCode with
        | HttpStatusCode.OK | HttpStatusCode.NoContent ->
            return Result.Ok()
        | anyOtherStatus ->
            let! responseContent = r.Content.ReadAsStringAsync().AsAsync
            return Result.Error (sprintf "Got %A instead of OK when deleting %s Azure function settings: %A" anyOtherStatus functionName responseContent)
    }

/// Secret key definition for doing REST calls to an Azure Function
type Key =
    {
        /// name of the key. For example: default
        name: string
        /// value of the key
        value : string
    }

/// Secret key values are returned as an array, this is used for deserializing the return value
type Keys =
    {
        /// array of secret keys
        keys: Key array
    }

/// Get specified Azure function settings
let getFunctionUrl (publishingInfo: PublishingInfo) (functionAppsContainer: string) (functionName: string) =
    async {
        let! authToken =
            // Get the auth token used for authorization when retrieving Azure function secret keys
            async {
                use httpClient = getHttpClient publishingInfo
                let! r = httpClient.GetAsync("/api/functions/admin/token").AsAsync
                match r.StatusCode with
                | HttpStatusCode.OK ->
                    let! token = r.Content.ReadAsStringAsync().AsAsync
                    return token |> Json.Default.deserialize<string>
                | anyOtherStatusCode ->
                    let! error = r.Content.ReadAsStringAsync().AsAsync
                    return failwithf "Failed to get auth token status code %A, error: %s" anyOtherStatusCode error
            }
        let url = sprintf "https://%s.azurewebsites.net" functionAppsContainer
        use httpClient =
            let c = new HttpClient(BaseAddress = Uri(url))
            c.DefaultRequestHeaders.Add("Authorization", "Bearer " + authToken)
            c
        let! r = httpClient.GetAsync(sprintf "/admin/functions/%s/keys" functionName).AsAsync
        match r.StatusCode with
        | HttpStatusCode.OK ->
            let! content = r.Content.ReadAsStringAsync().AsAsync
            let keys = content |> Json.Default.deserialize<Keys>
            // we care about "default" key value, since this is what Azure creates by default.
            let defaultKey = keys.keys |> Array.find(fun k -> k.name = "default")
            return Result.Ok(sprintf "%s/api/%s?code=%s" url functionName defaultKey.value)
        | anyOtherStatus ->
            let! message = r.Content.ReadAsStringAsync().AsAsync
            return Result.Error(sprintf "Got %A when getting Azure Function POST url %s Azure function settings: %s" anyOtherStatus functionName message)
    }
