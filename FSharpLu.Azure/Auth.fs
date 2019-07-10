module Microsoft.FSharpLu.Azure.Auth

open Microsoft.IdentityModel.Clients.ActiveDirectory
open System.Security.Cryptography.X509Certificates
open Microsoft.FSharpLu.Async

/// Azure AD principal credentials
type AzureADPrincipal =
    {
        tenantId : string
        clientId : string
        objectId : string
        password : unit -> string
    }

/// Azure AD application parameters
type AzureADApplication =
    {
        clientId : string
        tenantId : string
        redirectUri : string
    }

/// Authentication method with Azure AD
type ADAuthenticationMethod =
    /// Interactive user authentication based on the specified
    /// Azure AD client application parameters
    | User of AzureADApplication
    /// Service Principal
    | Principal of AzureADPrincipal

    member x.TenantId = match x with User u -> u.tenantId | Principal p -> p.tenantId

/// Structure holding Azure subscription information and password
type Subscription =
    {
        SubscriptionId : string
        Authentication : ADAuthenticationMethod
    }

/// Acquire a token using the specified authentication method for the specified authenticationContext
let acquireTokenAsync (authMethod:ADAuthenticationMethod) (authenticationContext:AuthenticationContext) =
    match authMethod with
    | User u ->
#if NET462
        authenticationContext.AcquireTokenAsync(
                    Microsoft.FSharpLu.Azure.Constants.management,
                    u.clientId,System.Uri u.redirectUri,
                    PlatformParameters(PromptBehavior.Auto)).AsAsync
#else
    #if NETSTANDARD2_0
            authenticationContext.AcquireTokenAsync(
                        Microsoft.FSharpLu.Azure.Constants.management,
                        u.clientId,System.Uri u.redirectUri,
                        PlatformParameters(PromptBehavior.Auto, null)).AsAsync
    #else
            raise <| System.NotImplementedException("acquireTokenAsync not implemented for this framework.")
    #endif
#endif
    | Principal p ->
        authenticationContext.AcquireTokenAsync(
                    Microsoft.FSharpLu.Azure.Constants.management,
                    ClientCredential(p.clientId, p.password())) |> Async.AwaitTask

/// Get an AD token using the specified authentication method
let getAuthorizationTokenInfo (authMethod:ADAuthenticationMethod) =
    async {
        let authenticationContext = AuthenticationContext(sprintf "https://login.windows.net/%s" authMethod.TenantId)
        let! tokenInfo = acquireTokenAsync authMethod authenticationContext
        if isNull tokenInfo then
            raise <| new System.InvalidOperationException("Failed to obtain the token from AD")

        let applicationId, objectId =
            match authMethod with
            | User _ ->
                None, tokenInfo.UserInfo.UniqueId
            | Principal p ->
                Some p.clientId, p.objectId

        return tokenInfo, applicationId, objectId
    }

/// Get an AD token using the specified authentication method. Return just the token without the associated info
let getAuthorizationToken (authMethod:ADAuthenticationMethod) =
    async {
        let! token, _, _ = getAuthorizationTokenInfo authMethod
        return token.AccessToken
    }

/// Callback used with Azure ARM APIs to regenerate an access token upon expiration
let createAccessTokenCallback authMethod =
    match authMethod with
    | User u ->
        fun (authority:string) (resource:string) scope ->
            async {
                let context = AuthenticationContext(authority, TokenCache.DefaultShared)
                let! result = context.AcquireTokenAsync(resource, u.clientId,  new UserCredential()) |> Async.AwaitTask
                return result.AccessToken
            } |> Async.StartAsTask

    | Principal p ->
        let clientCredential = ClientCredential(p.clientId, p.password())
        fun (authority:string) (resource:string) scope ->
            async {
                let context = AuthenticationContext(authority, TokenCache.DefaultShared)
                let! result = context.AcquireTokenAsync(resource, (clientCredential:ClientCredential)) |> Async.AwaitTask
                return result.AccessToken
            } |> Async.StartAsTask

/// Lookup a certificate from a thumbprint on the local machine user store
///
/// Instructions to make this work when running on an Azure web Role:
///     http://azure.microsoft.com/blog/2014/10/27/using-certificates-in-azure-websites-applications/
let findCert thumbPrint =
    let store = new X509Store(StoreName.My, StoreLocation.CurrentUser)
    store.Open(OpenFlags.ReadOnly)
    // lookup by thumbprint
    let collection = store.Certificates.Find(X509FindType.FindByThumbprint, thumbPrint, false)
    store.Close()

    // Check to see if our certificate was added to the collection. If no, throw an error, if yes, create a certificate using it.
    if collection.Count = 0 then
        invalidOp (sprintf "Could not find certificate for the provided thumbprint: %s" thumbPrint)

    collection.Item 0
