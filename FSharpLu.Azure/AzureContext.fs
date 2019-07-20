// Copyright (c) Microsoft Corporation.

/// F# helpers to access Azure Compute and Azure Storage API
module Microsoft.FSharpLu.Azure.Context

open Microsoft.Azure.KeyVault
open Microsoft.Azure.Management.Compute
open Microsoft.Azure.Management.Network
open Microsoft.Azure.Management.ResourceManager
open Microsoft.FSharpLu.Logging
open Microsoft.Rest.TransientFaultHandling
open Microsoft.Azure.Storage
open Microsoft.FSharpLu.Azure.Auth
open System.Net
open System.Runtime.CompilerServices
open Microsoft.FSharpLu.HttpCommunication

/// type used to retry operations on REST responses given specific StatusCode values passed in a list
type HttpTransientErrorDetectionStrategy (httpStatusCodeList:list<HttpStatusCode>) =
    interface Microsoft.Rest.TransientFaultHandling.ITransientErrorDetectionStrategy with
        member x.IsTransient(ex:System.Exception) =
           match ex with
            | :? HttpRequestWithStatusException as httpException ->
                List.contains httpException.StatusCode httpStatusCodeList
            | _ -> false

/// Context used to access Azure infrastructure as a service (IAAS) resources (Compute, Network and resource groups)
type InfrastructureContext =
    {
        /// name the resource group where to deploy VMs
        groupName : string
        /// client used to access Azure Compute resources
        compute : ComputeManagementClient
        /// client used to access Azure Resource Groups
        resource : IResourceManagementClient
        /// client used to access Azure Network resources
        network : INetworkManagementClient
        /// Logging tags to be included in every call to TraceTags.info/warning/error
        tags : (string*string) list
    }
    with
        interface System.IDisposable with
            override x.Dispose() =
                x.compute.Dispose()
                x.network.Dispose()
                x.resource.Dispose()

type BlobStorageContext =
    {
        endpoint : string
        client : Blob.CloudBlobClient
    }
with
    member x.storageAccount
        with [<MethodImpl(MethodImplOptions.NoInlining)>]get() =
            x.client.Credentials.AccountName

    member x.storageAccountKey
        with [<MethodImpl(MethodImplOptions.NoInlining)>]get() =
            x.client.Credentials.ExportBase64EncodedKey()

/// Create a new Azure Compute context
let defaultRetryPolicy =
    Microsoft.Rest.TransientFaultHandling.RetryPolicy(
        Microsoft.Rest.TransientFaultHandling.TransientErrorIgnoreStrategy(),
        Microsoft.Rest.TransientFaultHandling.IncrementalRetryStrategy())

let defaultNetworkRetryPolicy =
    Microsoft.Rest.TransientFaultHandling.RetryPolicy(
        /// Value 429 does not have a pre-defined value in the HttpStatusCode enum
        HttpTransientErrorDetectionStrategy [enum Constants.TooManyRequestHttpStatus],
        Microsoft.Rest.TransientFaultHandling.ExponentialBackoffRetryStrategy())

let newAzureComputeManagementContext (token:Microsoft.Rest.TokenCredentials) azure =
    let client = new ComputeManagementClient(token, SubscriptionId = azure.SubscriptionId)
    client.HttpClient.DefaultRequestHeaders.ConnectionClose <- System.Nullable true
    client.SetRetryPolicy(defaultRetryPolicy)
    client

/// Create a new Azure Resource group context
let newAzureResourceGroupManagementContext (token:Microsoft.Rest.TokenCredentials) azure =
    let client = new ResourceManagementClient(token, SubscriptionId = azure.SubscriptionId)
    client.HttpClient.DefaultRequestHeaders.ConnectionClose <- System.Nullable true
    client.SetRetryPolicy(defaultRetryPolicy)
    client :> IResourceManagementClient

/// Create a new Azure Network context
let newAzureNetworkContext (token:Microsoft.Rest.TokenCredentials) azure =
    let client = new NetworkManagementClient(token, SubscriptionId = azure.SubscriptionId)
    client.HttpClient.DefaultRequestHeaders.ConnectionClose <- System.Nullable true
    client.SetRetryPolicy(defaultNetworkRetryPolicy)
    client :> INetworkManagementClient

/// Create a new Azure Vault Management context
let newVaultManagementContext (token:Microsoft.Rest.TokenCredentials) azure =
    let client = new Microsoft.Azure.Management.KeyVault.KeyVaultManagementClient(token, SubscriptionId = azure.SubscriptionId)
    client.HttpClient.DefaultRequestHeaders.ConnectionClose <- System.Nullable true
    client.SetRetryPolicy(defaultRetryPolicy)
    client :> Microsoft.Azure.Management.KeyVault.IKeyVaultManagementClient

/// Create a deployment context to access a resource group with the specified name
/// Note: upon expiration the authentication token is automatically renewed by the managed Azure API.
let public newAzureInfrastructureContext azure groupName tags =
    async {
        let! token = getAuthorizationToken azure.Authentication
        let tokenCredentials = Microsoft.Rest.TokenCredentials(token)
        return {
            groupName = groupName
            compute = newAzureComputeManagementContext tokenCredentials azure
            resource = newAzureResourceGroupManagementContext tokenCredentials azure
            network = newAzureNetworkContext tokenCredentials azure
            tags = tags
        }
    }

[<Literal>]
let DefaultAzureDomain =
    "core.windows.net"

let defaultStorageEndpoint =
    sprintf "https://%s" DefaultAzureDomain

let storageRootUrl storageAccount =
    sprintf "https://%s.blob.%s" storageAccount DefaultAzureDomain

let blobUri (s:BlobStorageContext) blobRelativePath =
    System.Uri(sprintf "%s/%s" (storageRootUrl s.storageAccount) blobRelativePath)

let createBlobContext (storageAccount:string) (storageKey:string) =
    let creds = Auth.StorageCredentials(storageAccount, storageKey)
    let blobAccountUri = System.Uri(storageRootUrl storageAccount)
    {
        endpoint = defaultStorageEndpoint
        client = new Blob.CloudBlobClient(blobAccountUri, creds)
    }

let createBlobContextFromAzureCloudStorageAccount (cloudStorage:CloudStorageAccount) =
    {
        endpoint = defaultStorageEndpoint
        client = new Blob.CloudBlobClient(cloudStorage.BlobStorageUri, cloudStorage.Credentials)
    }

let createVaultContext authmethod =
    let callback = Auth.createAccessTokenCallback authmethod
    new KeyVaultClient(KeyVaultClient.AuthenticationCallback(callback))

let inline assertStatusCodeIs tags validStatusCode statusCode message =
  if statusCode <> validStatusCode then
    TraceTags.failwith (sprintf "Status code assertion failed. Expected %A" validStatusCode)
        (tags @ [ "message", message
                  "statusCodeString", sprintf "%O" statusCode
                  "statusCode", sprintf "%A" statusCode
                  "validStatusCode", sprintf "%A" validStatusCode
                 ])

let inline assertExistsOrCreated tags statusCode message =
  if statusCode <> System.Net.HttpStatusCode.OK && statusCode <> System.Net.HttpStatusCode.Created then
    TraceTags.failwith "Status code assertion failed. Expected OK or Created"
        (tags @ [ "message", message
                  "statusCodeString", sprintf "%O" statusCode
                  "statusCode", sprintf "%A" statusCode
                 ])

let inline assertOKorAccepted tags statusCode message =
  if statusCode <> System.Net.HttpStatusCode.OK && statusCode <> System.Net.HttpStatusCode.Accepted then
    TraceTags.failwith "Status code assertion failed. Expected OK or Accepted"
        (tags @ [ "message", message
                  "statusCodeString", sprintf "%O" statusCode
                  "statusCode", sprintf "%A" statusCode
                 ])

let inline assertReq tags statusCode message =
    assertStatusCodeIs tags System.Net.HttpStatusCode.OK statusCode message

/// Get Azure storage account context from a connection string
let getStorageAccountFromConnectionString (connectionString:string) =
    CloudStorageAccount.Parse(connectionString)

/// Get Azure storage account context from a name/key pair
let getStorageAccountFromCredentials accountName (key:string) =
    CloudStorageAccount(Auth.StorageCredentials(accountName, key), true)

/// Convert storage account name/key pair to connection string
let getStorageAccountConnectionStringFromCredentials accountName (key:string) =
    let s = getStorageAccountFromCredentials accountName key
    s.ToString(true)

/// Return storage account name and key from a connection string setting
let getStorageAccountNameKeyFromConnectionString (connectionString:string) =
    let s = getStorageAccountFromConnectionString connectionString
    s.Credentials.AccountName, s.Credentials.ExportBase64EncodedKey()

/// Convert Azure Region name to region ID
let toRegionId (region:string) =
    region.ToLower().Replace(" ", "")

/// Return the short code form for specified region
/// When adding entries to this list make sure to add it as well in deployment\AzureRegions.json
let regions =
    [
        "Central US", "cus"
        "East US", "eus"
        "East US 2", "eus2"
        "North Central US", "ncus"
        "South Central US", "scus"
        "West US", "wus"
        "North Europe", "ne"
        "West Europe", "we"
        "East Asia", "ea"
        "Southeast Asia", "sa"
        "Japan East", "je"
        "Japan West", "jw"
        "Brazil South", "bs"
        "Australia East", "ae"
        "Australia Southeast", "as"
    ] |> List.map (fun (x,y) -> (toRegionId x, y)) |> Map.ofList

/// Adding 'setter' functions so that more fine grained policies can be set further up the stack as needed
let setNetworkRetryPolicy (client:INetworkManagementClient) policy =
    let c = client :?> NetworkManagementClient
    c.SetRetryPolicy(policy)

let setComputeRetryPolicy (client:IComputeManagementClient) policy =
    let c = client :?> ComputeManagementClient
    c.SetRetryPolicy(policy)

let setResourceRetryPolicy (client:IResourceManagementClient) policy =
    let c = client :?> ResourceManagementClient
    c.SetRetryPolicy(policy)
