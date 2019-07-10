/// F# helpers to the Azure Vault API
/// Copyright (c) Microsoft Corporation.
module Microsoft.FSharpLu.Azure.Vault

open System.Linq
open Microsoft.FSharpLu.Azure
open Microsoft.FSharpLu.Async
open Microsoft.FSharpLu.Logging
open Microsoft.FSharpLu.ErrorHandling
open Microsoft.FSharpLu
open Microsoft.Azure.Management.KeyVault
open Microsoft.Azure.Management.KeyVault.Models
open Microsoft.FSharpLu.Azure.Request.ErrorHandling

/// Settings needed to access a vault secret
type VaultSecretAccessContext =
    {
        resourceGroupName : string
        vaultName : string
        secretName : string
        /// Logging tags to be logged for every call to TraceTags.info/warning/error from the Vault helper functions
        tags : (string * string) list
    }

/// Represents errors that may be thrown in case a key vault entry is not found.
type KeyVaultEntryNotFoundError =
    | VaultClientError of Microsoft.Azure.KeyVault.Models.Error
    | CloudError of Microsoft.Rest.Azure.CloudError

/// Match a key vault exception with the specified error code
let SomeKeyVaultException errorCode (e:System.Exception) =
    match e with
    | :? Microsoft.Azure.KeyVault.Models.KeyVaultErrorException as e when
            e.Body.Error.Code = errorCode -> Some (VaultClientError e.Body.Error)
    | :? Microsoft.Rest.Azure.CloudException as e when
            e.Body.Code = errorCode -> Some (CloudError e.Body)
    | _ -> None

let SomeKeyVaultEntryNotFoundException e = SomeKeyVaultException "SecretNotFound" e
let SomeKeyVaultForbiddenException e = SomeKeyVaultException "Forbidden" e

/// Gets an Azure Vault URL given the vault name.
let getVaultUrl vaultName =
    sprintf "https://%s.vault.azure.net/" vaultName

/// Gets the key vault ID given the subscription id, resource group name, and vault name
let getVaultId subscriptionId resourceGroupName vaultName =
    sprintf "/subscriptions/%s/resourceGroups/%s/providers/Microsoft.KeyVault/vaults/%s"
            subscriptionId resourceGroupName vaultName

/// Parse a KeyVault secret URL of the form "https://{vaultName}.vault.azure.net[:443]/secrets/{secretName}/{secretVersion}"
/// and return the keyvault vault base URL, secret name and secret version.
let parseKeyvaultSecretUrl secretUrl =
    let secretUri = System.Uri(secretUrl)
    let trimTrailingSlash (s:System.String) = s.TrimEnd('/')

    let keyvaultUrl =
        sprintf "%s://%s:%d/" secretUri.Scheme secretUri.Authority secretUri.Port

    match secretUri.Segments with
    | [|"/"; "secrets/"; name; version|] ->
        keyvaultUrl, trimTrailingSlash name, trimTrailingSlash version
    | [|"/"; "secrets/"; name; |] ->
        keyvaultUrl, trimTrailingSlash name, ""
    | _ ->
        TraceTags.failwith "Could not parse Azure KeyVault secret name and version from specified URL" [ "secretUrl", secretUrl ]

/// Return the URL to the specified secret
let getSecretUrl (azure:Auth.Subscription) vaultName (secretName:string) =
    async {
        use c = Context.createVaultContext azure.Authentication
        let vaultUrl = getVaultUrl vaultName
        let! secret = c.GetSecretWithHttpMessagesAsync(vaultUrl, secretName, "").AsAsync

        return secret.Body.Id
    }

/// Gets a secret from the specified vault if it exists.
let tryGetSecret (azure:Auth.Subscription) (secretContext:VaultSecretAccessContext) =
    async {
        use c = Context.createVaultContext azure.Authentication
        let vaultUrl = getVaultUrl secretContext.vaultName

        try
            let! secret = c.GetSecretWithHttpMessagesAsync(vaultUrl, secretContext.secretName, "").AsAsync
            return Some secret
        with
        | IsAggregateOf SomeKeyVaultEntryNotFoundException e ->
            return None
    }

/// Set a secret value in the specified vault. Create the secret if it does not already exist.
let setSecret (azure:Auth.Subscription) (secretContext:VaultSecretAccessContext) (secretValue:string) =
    async {
        use c = Context.createVaultContext azure.Authentication
        let vaultUrl = getVaultUrl secretContext.vaultName

        let! secret = c.SetSecretWithHttpMessagesAsync(vaultUrl, secretContext.secretName, secretValue).AsAsync

        TraceTags.info "Secret value set"
                    (secretContext.tags @
                      [ "resourceGroup", secretContext.resourceGroupName
                        "vaultName", secretContext.vaultName
                        "secretName", secretContext.secretName
                        "secretId", secret.Body.Id])

        return secret.Body.Id
    }

/// Inserts a new secret into the specified vault.  Throws if the secret already exists.
let insertSecret (azure:Auth.Subscription) (secretContext:VaultSecretAccessContext) (secretValue:string) =
    async {
        let! existingSecret = tryGetSecret azure secretContext
        if existingSecret.IsSome then
            TraceTags.failwith "Secret with this name already exists in specified keyvault"
                               (secretContext.tags @
                                 [ "resourceGroup", secretContext.resourceGroupName
                                   "vaultName", secretContext.vaultName
                                   "secretName", secretContext.secretName ])

        use c = Context.createVaultContext azure.Authentication
        let vaultUrl = getVaultUrl secretContext.vaultName

        let! secret = c.SetSecretWithHttpMessagesAsync(vaultUrl, secretContext.secretName, secretValue).AsAsync

        TraceTags.info "Added new secret to KeyVault"
                    (secretContext.tags @
                      [ "resourceGroup", secretContext.resourceGroupName
                        "vaultName", secretContext.vaultName
                        "secretName", secretContext.secretName
                        "secretId", secret.Body.Id])

        return secret.Body.Id
    }

/// Deletes the specified secret from the vault.
let deleteSecret (azure:Auth.Subscription) (secretContext:VaultSecretAccessContext) =
    async {

        use c = Context.createVaultContext azure.Authentication
        let vaultUrl = getVaultUrl secretContext.vaultName

        let! secret = c.DeleteSecretWithHttpMessagesAsync(vaultUrl, secretContext.secretName).AsAsync

        return secret
    }

/// Update KeyVault access policies for the specified application and object ID and specified keyvault
let setAccessPolicy
                (authenticationToken:string)
                subscriptionId
                (resourceGroup:string)
                (vault:Vault)
                (applicationId:string option, objectId:string)
                (permissionsToKeys:(string[]) option)
                (permissionsToSecrets:(string[]) option)
                (permissionsToCertificates:(string[]) option)
                (permissionsToStorage:(string[]) option)
                =
    async {
        if  permissionsToKeys.IsNone
            && permissionsToSecrets.IsNone
            && permissionsToCertificates.IsNone
            && permissionsToStorage.IsNone then
                raise <| new System.ArgumentException("No permissions specified")

        use c = new Microsoft.Azure.Management.KeyVault.KeyVaultManagementClient(
                        Microsoft.Rest.TokenCredentials(authenticationToken),
                        SubscriptionId = subscriptionId)

        if isNull vault then
            raise <| new System.NullReferenceException("Vault parameter is null: vault")

        let emptyPermissions (s:(string []) option) =
            s.IsNone || Seq.isEmpty s.Value

        let shouldDeleteEntry =
            emptyPermissions permissionsToKeys
         && emptyPermissions permissionsToSecrets
         && emptyPermissions permissionsToCertificates
         && emptyPermissions permissionsToStorage

        let appId = applicationId|> Option.map System.Guid.Parse |> Option.toNullable

        let matchObjectAppId (entry:AccessPolicyEntry) =
            entry.ApplicationId = appId
            && System.String.Equals(entry.ObjectId, objectId, System.StringComparison.OrdinalIgnoreCase)

        let existingEntry = vault.Properties.AccessPolicies |> Seq.tryFind matchObjectAppId

        let entryAlreadyExists = existingEntry.IsSome

        let comparePermissions existingPermissions newPermissions =
            match newPermissions with
            | None -> 0 // permissions not updated
            | Some p ->
                Seq.compareWith
                    (fun x y -> System.StringComparer.InvariantCultureIgnoreCase.Compare(x,y))
                    (Set.ofSeq existingPermissions)
                    (Set.ofSeq p)

        let shouldUpdate =
            match existingEntry with
            | None ->
                // Add new entry
                not shouldDeleteEntry
            | Some existingEntry ->
                // Delete existing entry
                shouldDeleteEntry
                // Update existing entry
                || comparePermissions existingEntry.Permissions.Keys permissionsToKeys <> 0
                || comparePermissions existingEntry.Permissions.Secrets permissionsToSecrets <> 0
                || comparePermissions existingEntry.Permissions.Certificates permissionsToCertificates <> 0
                || comparePermissions existingEntry.Permissions.Storage permissionsToStorage <> 0

        if shouldUpdate then
            let mergeWithExistingEntry (existingPolicy:AccessPolicyEntry) =
                let mergePermissions existingPolicy (existingPermissions:System.Collections.Generic.IList<string>) =
                    function
                    | Some p -> p
                    | None ->
                        if not <| isNull existingPolicy && not <| isNull existingPermissions then
                            existingPermissions.ToArray()
                        else
                            null
                AccessPolicyEntry(
                    TenantId = vault.Properties.TenantId,
                    ObjectId = objectId,
                    ApplicationId = appId,
                    Permissions =
                        Permissions
                            (
                                Keys = mergePermissions existingPolicy existingPolicy.Permissions.Keys permissionsToKeys,
                                Secrets = mergePermissions existingPolicy existingPolicy.Permissions.Secrets permissionsToSecrets,
                                Certificates = mergePermissions existingPolicy existingPolicy.Permissions.Certificates permissionsToCertificates,
                                Storage = mergePermissions existingPolicy existingPolicy.Permissions.Storage permissionsToStorage
                            )
                )

            let updatedAccessPolicies : AccessPolicyEntry[] =
                if shouldDeleteEntry then
                    vault.Properties.AccessPolicies
                    |> Seq.filter (matchObjectAppId >> not)
                    |> Seq.toArray
                else
                    if entryAlreadyExists then
                        vault.Properties.AccessPolicies
                        |> Seq.map
                            (fun ap ->
                                if matchObjectAppId ap then
                                    mergeWithExistingEntry ap
                                else
                                    ap
                            )
                        |> Seq.toArray
                    else
                        vault.Properties.AccessPolicies
                        |> Seq.append [
                            AccessPolicyEntry(
                                TenantId = vault.Properties.TenantId,
                                ObjectId = objectId,
                                ApplicationId = appId,
                                Permissions = Permissions
                                                    (
                                                        Keys = (permissionsToKeys |> Option.defaultValue null),
                                                        Secrets = (permissionsToSecrets |> Option.defaultValue null),
                                                        Certificates = (permissionsToCertificates |> Option.defaultValue null),
                                                        Storage = (permissionsToStorage |> Option.defaultValue null)
                                                    )
                            )]
                        |> Seq.toArray

            let! updatedVault =
                c.Vaults.UpdateAsync(
                    resourceGroup,
                    vault.Name,
                    VaultPatchParameters(
                        Properties = VaultPatchProperties(AccessPolicies = updatedAccessPolicies),
                        Tags = vault.Tags
                    )).AsAsync

            TraceTags.info "Vault access policy updated" [ "vaultId", updatedVault.Id ]
        else
            TraceTags.info "Vault access already granted, no need to update access policy" [ "vaultId", vault.Id ]
     }

/// Try updating KeyVault access policies for the specified application and object ID and specified keyvault
/// Returns Result.OK () if succeeded or Result.Error (isTransientError, errorMessage, errorTags) if a failure occurs.
let trySetAccessPolicy
            (authenticationToken:string)
            subscriptionId
            resourceGroupName
            vaultName
            (applicationId:string option, objectId:string)
            (permissionsToKeys:(string[]) option)
            (permissionsToSecrets:(string[]) option)
            (permissionsToCertificates:(string[]) option)
            (permissionsToStorage:(string[]) option)
            =
    async {
         if  permissionsToKeys.IsNone
             && permissionsToSecrets.IsNone
             && permissionsToCertificates.IsNone
             && permissionsToStorage.IsNone then
                 raise <| new System.ArgumentException("No permissions specified")

         use c = new Microsoft.Azure.Management.KeyVault.KeyVaultManagementClient(
                         Microsoft.Rest.TokenCredentials(authenticationToken),
                         SubscriptionId = subscriptionId)

         let! vault =
            async {
                try
                    let! v = c.Vaults.GetAsync(resourceGroupName, vaultName).AsAsync
                    return Result.Ok v
                with
                | IsAggregateOf SomeNotFoundException _
                | IsAggregateOf SomeResourceNotFoundException _ ->
                    return Result.Error (false, sprintf "Keyvault does not exist %s" vaultName, [])
                | IsAggregateOf SomeResourceGroupNotFoundException _ ->
                    return Result.Error (false, sprintf "Resource group does not exist %s" resourceGroupName, [])
            }

         match vault with
         | Result.Error (transient, errorMessage, tags) as e ->
            return Result.Error (transient, errorMessage, tags)
         | Result.Ok vault ->
            return!
                async {
                    try
                        do! setAccessPolicy authenticationToken
                                    subscriptionId
                                    resourceGroupName
                                    vault
                                    (applicationId, objectId)
                                    permissionsToKeys
                                    permissionsToSecrets
                                    permissionsToCertificates
                                    permissionsToStorage
                        return Result.Ok ()
                    with
                    | IsAggregateOf (SomeCloudExceptionStatusCode System.Net.HttpStatusCode.Conflict) e ->
                        return Result.Error (true, "Could not set access policy",
                                                [ "keyvaultResourceGroupName", resourceGroupName
                                                  "vaultUri", vault.Id
                                                  "response", e.Content
                                                  "reasonPhrase", e.ReasonPhrase ])
                    | IsAggregateOf SomeConflictErrorException e ->
                        return Result.Error (true, "Could not set access policy",
                                            [   "keyvaultResourceGroupName", resourceGroupName
                                                "vaultUri", vault.Id
                                                "responseMessage", e.Message
                                                "additionalInfo", e.AdditionalInfo.ToString()
                                                "details", e.Details.ToString()
                                            ])
                }
        }

/// Deletes the specified secret from the vault if it exists.
let deleteSecretIfExists (azure:Auth.Subscription) (secretContext:VaultSecretAccessContext) =
    async {
        try
            let! secret = deleteSecret azure secretContext
            return Some secret
        with
        | IsAggregateOf SomeKeyVaultEntryNotFoundException _ ->
            TraceTags.info "Secret does not exist, nothing to delete"
                            (secretContext.tags @
                              [ "resourceGroup", secretContext.resourceGroupName
                                "vaultName", secretContext.vaultName
                                "secretName", secretContext.secretName ])
            return None

        | IsAggregateOf SomeNameResolutionFailure e ->
            TraceTags.info "Keyvault dot not exist, nothing to delete"
                            (secretContext.tags @
                              [ "resourceGroup", secretContext.resourceGroupName
                                "vaultName", secretContext.vaultName
                                "secretName", secretContext.secretName
                                "exception", e.ToString() ])
            return None

         | IsAggregateOf SomeKeyVaultForbiddenException e ->
            TraceTags.error "Keyvault could not be deleted due to missing access permissions"
                            (secretContext.tags @
                              [ "resourceGroup", secretContext.resourceGroupName
                                "vaultName", secretContext.vaultName
                                "secretName", secretContext.secretName
                                "exception", e.ToString()
                                "authentication", sprintf "%A" azure.Authentication])
            return None
    }

/// Copy a secret from one vault to another
/// If overwrite is true then replace existing secret with new value, otherwise
/// leave existing value intact.
let copySecret azure sourceVaultSecret targetVaultSecret overwrite =
    async {
        let! targetExistingSecret =
            if overwrite then
                async.Return None
            else
                tryGetSecret azure targetVaultSecret

        match targetExistingSecret with
        | Some existingSecretId when (not overwrite) ->
            return existingSecretId.Body.Id
        | _ ->
            let! secret = tryGetSecret azure sourceVaultSecret
            let sourceSecretContent =
                match secret with
                | Some s -> s.Body.Value
                | None ->
                    TraceTags.failwith "Could not located secret with given name in specified KeyVault"
                                    (sourceVaultSecret.tags @
                                      [ "resourceGroup", sourceVaultSecret.resourceGroupName
                                        "vaultName", sourceVaultSecret.vaultName
                                        "secretName", sourceVaultSecret.secretName ])

            return! setSecret azure targetVaultSecret sourceSecretContent
    }