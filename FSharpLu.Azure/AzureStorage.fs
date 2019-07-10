/// F# helpers to the Azure Storage API
/// Copyright (c) Microsoft Corporation.
module Microsoft.FSharpLu.Azure.Storage

open AppInsights
open Microsoft.Azure.Storage
open Microsoft.Azure.Management.Storage
open Microsoft.FSharpLu.Async
open Microsoft.FSharpLu.ErrorHandling
open Microsoft.FSharpLu.Azure.Request
open System

type ResourceGroupName = string
type StorageAccountName = string
type StorageAccountKey = string
type Region = string

/// Cached info for a storage account
type CachedStorageAccountInfo =
    {
        key : StorageAccountKey
        endpoints : Models.Endpoints
    }

module Impl =
    open System.Text
    open System.Threading

    type TagKey = string
    type TagValue = string
    type ListKeys = string * string -> Models.StorageAccountListKeysResult
    type ListResources = Auth.Subscription -> ResourceGroupName -> Async<Models.StorageAccount[] * ListKeys>

    /// Returns an IDisposable storage context
    let newStorageContext (azure:Auth.Subscription) =
        async {
            let! token = Auth.getAuthorizationToken azure.Authentication
            return new StorageManagementClient(Microsoft.Rest.TokenCredentials(token), SubscriptionId = azure.SubscriptionId)
        }

    let getStorageAccountsByTag (listByResourceGroup:ListResources) azure resourceGroupName (tagKey:TagKey, tagValue:TagValue) =
        async{
            let! s, listKeys = listByResourceGroup azure resourceGroupName
            return s
                   |> Seq.filter(fun x -> x.Tags.ContainsKey(tagKey) && (x.Tags.[tagKey] = tagValue))
                   |> Seq.map(fun x -> x, listKeys(resourceGroupName, x.Name))
        }

    let tryGetStorageAccountByName (listByResourceGroup:ListResources) azure resourceGroupName storageAccountName =
        async{
            let! s, listKeys = listByResourceGroup azure resourceGroupName
            return s
                   |> Seq.tryFind(fun x -> x.Name.Equals(storageAccountName))
        }

    let getStorageEnpointsByTag (listByResourceGroup:ListResources) azure resourceGroupName (tagKey:TagKey, tagValue:TagValue) =
        async {
            let! xs = getStorageAccountsByTag listByResourceGroup azure resourceGroupName (tagKey, tagValue)
            return xs |> Seq.map(fun (x, key) -> x.Name, key.Keys.[0].Value, x.PrimaryEndpoints)
        }

    /// Extracts the name of an Azure Storage account from the connection string.
    let getStorageAccountNameFromConnectionString (storageAccountConnectionString:string) =
        let storageAccount = CloudStorageAccount.Parse storageAccountConnectionString
        storageAccount.QueueEndpoint.Host.Replace(".queue.core.windows.net", String.Empty)

    let tryGetStorageEndpointByName (listByResourceGroup:ListResources) azure resourceGroupName (name:string)
        : Async<option<StorageAccountName * StorageAccountKey * Models.Endpoints>> =
        async{
            let! s, listKeys = listByResourceGroup azure resourceGroupName
            return
                s |> Seq.tryFind(fun x -> x.Name = name)
                  |> Option.map(fun y ->
                         let keys = listKeys(resourceGroupName, y.Name)
                         y.Name, keys.Keys.[0].Value, y.PrimaryEndpoints)
        }

    let tryGetStorageNameKey (listByResourceGroup:ListResources) azure resourceGroupName (name:StorageAccountName) =
        async {
            let! accounts = tryGetStorageEndpointByName listByResourceGroup azure resourceGroupName name
            return accounts |> Option.map(fun (name, key, endpoints) -> (name, key))
        }

    let mapStorageEndpointsByName (listByResourceGroup:ListResources) (azure:Auth.Subscription) (resourceGroupName:ResourceGroupName) (names:StorageAccountName Set) =
        async {
            let! s, listKeys = listByResourceGroup azure resourceGroupName
            let map =
                (Map.empty, names)
                ||> Seq.fold (fun m name ->
                    let value =
                        s
                        |> Seq.tryFind(fun x -> x.Name = name)
                        |> Option.map(fun y ->
                                        let keys = listKeys(resourceGroupName, y.Name)
                                        {
                                            CachedStorageAccountInfo.key = keys.Keys.[0].Value
                                            CachedStorageAccountInfo.endpoints = y.PrimaryEndpoints
                                        })
                    m.Add(name, value)
                )
            return map
        }

    let getAllAccountsWithKeys (listByResourceGroup:ListResources) azure resourceGroupName
        : Async<seq<ResourceGroupName * StorageAccountName * Region * StorageAccountKey>> =
        async {
            let! s, listKeys = listByResourceGroup azure resourceGroupName
            return s |> Seq.map (fun x -> resourceGroupName, x.Name, x.Location, listKeys(resourceGroupName, x.Name).Keys.[0].Value)
        }

    let listStorageAccountsByResourceGroupAsync (storage:StorageManagementClient) resourceGroupName =
        async {
            let! s = storage.StorageAccounts.ListByResourceGroupAsync(resourceGroupName).AsAsync
            return s |> Seq.toArray
        }

    let listStorageAccountKeysByResourceGroup (storage:StorageManagementClient) resourceGroupName storageAccountName =
        storage.StorageAccounts.ListKeys(resourceGroupName, storageAccountName)

    let setStorageAccountNetworkRuleDefaultAction (listbyResourceGroup:ListResources) azure
        resourceGroupName storageAccountName defaultAction =
        async {
            use! storage = newStorageContext azure
            let! storageAccount = tryGetStorageAccountByName
                                      listbyResourceGroup
                                      azure
                                      resourceGroupName
                                      storageAccountName
            match storageAccount with
            | None ->
                return TraceTags.failwith "Could not find requested storage account in Azure"
                        [ "storageAccount", storageAccountName
                          "resourceGroup", resourceGroupName ]
            | Some storageAccount ->
                let networkRules = Models.NetworkRuleSet(
                                       bypass = storageAccount.NetworkRuleSet.Bypass,
                                       defaultAction = defaultAction,
                                       virtualNetworkRules = storageAccount.NetworkRuleSet.VirtualNetworkRules,
                                       ipRules = storageAccount.NetworkRuleSet.IpRules)
                let update = Models.StorageAccountUpdateParameters(
                                 networkRuleSet = networkRules)
                let! result = storage.StorageAccounts.UpdateWithHttpMessagesAsync(resourceGroupName, storageAccountName, update).AsAsync
                return result.Body
        }

    /// Cleans up virtual network connections to Azure Storage from virtual networks that no longer exist.
    let cleanUpDeletedVirtualNetworksFromAzureStorageFirewall (listByResourceGroup:ListResources) azure storageResourceGroupName storageAccountName tags =
        async {
            let tags = tags |> (List.filter (fun t -> fst t <> "currentMethod"))
            let tags = tags @ ["currentMethod", "cleanUpDeletedVirtualNetworksFromAzureStorageFirewall"]

            // Clean up any rules that have State = 'NetworkSourceDeleted'... this means that the virtual network was deleted,
            // but the virtual network connection to Azure Storage wasn't. Because we delete the connections after the resource groups
            // are deleted, this can happen.
            use! storage = newStorageContext azure
            let! storageAccount = tryGetStorageAccountByName listByResourceGroup azure storageResourceGroupName storageAccountName

            match storageAccount with
            | None ->
                TraceTags.info "The specified storage account could not be found in Azure." tags
                return None
            | Some storageAccount ->
                TraceTags.info "Checking for virtual network rules with state NetworkSourceDeleted." (tags @ ["numberOfRulesLeft", storageAccount.NetworkRuleSet.VirtualNetworkRules.Count.ToString()])
                while (storageAccount.NetworkRuleSet.VirtualNetworkRules |> Seq.exists (fun rule -> rule.State.HasValue && rule.State.Value = Models.State.NetworkSourceDeleted)) do
                    let ruleToRemove =
                        storageAccount.NetworkRuleSet.VirtualNetworkRules
                        |> Seq.find (fun rule -> rule.State.HasValue && rule.State.Value = Models.State.NetworkSourceDeleted)
                    TraceTags.info "Removing virtual network rule with state NetworkSourceDeleted."
                        (tags @ [ "ruleToRemove", sprintf "%A" (ruleToRemove.VirtualNetworkResourceId)])
                    storageAccount.NetworkRuleSet.VirtualNetworkRules.Remove(ruleToRemove) |> ignore
                TraceTags.info "Done removing virtual network rules." tags

                let update = Models.StorageAccountUpdateParameters(networkRuleSet = storageAccount.NetworkRuleSet)

                let updatedRules = StringBuilder()
                for rule in storageAccount.NetworkRuleSet.VirtualNetworkRules do
                    updatedRules.AppendLine(sprintf "%s: %s" (if rule.State.HasValue then rule.State.Value.ToString() else "State not found") rule.VirtualNetworkResourceId) |> ignore
                    updatedRules.AppendLine("-------------------------------") |> ignore
                let tags = tags @ [ "updatedVirtualNetworkRules", updatedRules.ToString()
                                    "updatedVirtualNetworkRuleCount", storageAccount.NetworkRuleSet.VirtualNetworkRules.Count.ToString() ]

                TraceTags.info "Listing updated virtual network connections." tags
                let! result = storage.StorageAccounts.UpdateWithHttpMessagesAsync(storageResourceGroupName, storageAccountName, update).AsAsync
                return Some result.Body
        }

    /// Adds a virtual network to an Azure Storage account firewall exception list.
    let tryAddStorageAccountVnetRule (listByResourceGroup:ListResources) azure
        resourceGroupName storageAccountName vnetResourceId tags =
        async {
            let tags = tags |> (List.filter (fun t -> fst t <> "currentMethod"))
            let tags = tags @ [ "currentMethod", "tryAddStorageAccountVnetRule"
                                "telemetryClient", if String.IsNullOrEmpty(telemetryClient.InstrumentationKey) then "Blank InstrumentationKey" else telemetryClient.InstrumentationKey ]

            use! storage = newStorageContext azure
            let! storageAccount = tryGetStorageAccountByName
                                        listByResourceGroup
                                        azure
                                        resourceGroupName
                                        storageAccountName

            match storageAccount with
            | None ->
                TraceTags.info "Could not bridge virtual network to storage account. The specified storage account could not be found in Azure." tags
                return None
            | Some storageAccount ->
                let existingRules:StringBuilder = StringBuilder()
                for rule in storageAccount.NetworkRuleSet.VirtualNetworkRules do
                    existingRules.AppendLine(sprintf "%s: %s" (if rule.State.HasValue then rule.State.Value.ToString() else "State not found") rule.VirtualNetworkResourceId) |> ignore
                    existingRules.AppendLine("-------------------------------") |> ignore
                let tags = tags @ [ "existingVirtualNetworkRules", existingRules.ToString()
                                    "existingVirtualNetworkRuleCount", storageAccount.NetworkRuleSet.VirtualNetworkRules.Count.ToString() ]

                TraceTags.info "Found Azure Storage account; checking for existing virtual network connection before adding this one." tags

                let ruleExists =
                    storageAccount.NetworkRuleSet.VirtualNetworkRules
                    |> Seq.exists (fun r -> r.VirtualNetworkResourceId.Equals(vnetResourceId))

                if ruleExists then
                    TraceTags.info "Storage account firewall already has a rule for this virtual network. No action required." tags
                    return Some storageAccount
                else
                    let vnetRule = Models.VirtualNetworkRule(vnetResourceId, System.Nullable(Models.Action.Allow))

                    try
                        vnetRule.Validate()
                        TraceTags.info "New virtual network rule is validated." tags
                        let update = Models.StorageAccountUpdateParameters(
                                            networkRuleSet = storageAccount.NetworkRuleSet)
                        update.NetworkRuleSet.VirtualNetworkRules.Add(vnetRule)

                        let updatedRules:StringBuilder = StringBuilder()
                        for rule in storageAccount.NetworkRuleSet.VirtualNetworkRules do
                            updatedRules.AppendLine(sprintf "%s: %s" (if rule.State.HasValue then rule.State.Value.ToString() else "State not found") rule.VirtualNetworkResourceId) |> ignore
                            updatedRules.AppendLine("-------------------------------") |> ignore
                        let tags = tags @ [ "updatedVirtualNetworkRules", updatedRules.ToString()
                                            "updatedVirtualNetworkRuleCount", storageAccount.NetworkRuleSet.VirtualNetworkRules.Count.ToString() ]

                        TraceTags.info "Adding new virtual network connection to Azure Storage account firewall." tags
                        let! result = storage.StorageAccounts.UpdateWithHttpMessagesAsync(resourceGroupName, storageAccountName, update).AsAsync
                        let afterUpdateRules:StringBuilder = StringBuilder()
                        for rule in result.Body.NetworkRuleSet.VirtualNetworkRules do
                            afterUpdateRules.AppendLine(sprintf "%s: %s" (if rule.State.HasValue then rule.State.Value.ToString() else "State not found") rule.VirtualNetworkResourceId) |> ignore
                            afterUpdateRules.AppendLine("-------------------------------") |> ignore
                        let tags = tags @ [ "afterUpdateVirtualNetworkRules", afterUpdateRules.ToString()
                                            "afterUpdateVirtualNetworkRuleCount", result.Body.NetworkRuleSet.VirtualNetworkRules.Count.ToString() ]
                        TraceTags.info "After updating virtual network connections to Azure Storage firewall." tags
                        return Some result.Body
                    with
                    // This is expected to occur if we cannot find the VNET/Subnet that we are trying to bridge.
                    | IsAggregateOf SomeNetworkAclsValidationFailure e ->
                        TraceTags.info "Could not bridge virtual network to storage account. The operation failed Network ACL validation." tags
                        return None
        }

    /// Removes a virtual network from an Azure Storage account firewall exception list.
    let tryRemoveStorageAccountVnetRule (listByResourceGroup:ListResources) azure
        storageResourceGroupName storageAccountName vnetResourceId tags =
        async {
            let tags = tags |> List.filter (fun t -> fst t <> "currentMethod")
            let tags = tags @ ["currentMethod", "tryRemoveStorageAccountVnetRule"]

            try
                use! storage = newStorageContext azure
                let! storageAccount = tryGetStorageAccountByName listByResourceGroup azure
                                            storageResourceGroupName storageAccountName

                match storageAccount with
                | None ->
                    TraceTags.error "Could not remove virtual network rule: specified storage account was not found in Azure." tags
                    return None
                | Some storageAccount ->
                    let existingRules:StringBuilder = StringBuilder()
                    for rule in storageAccount.NetworkRuleSet.VirtualNetworkRules do
                        existingRules.AppendLine(sprintf "%s: %s" (if rule.State.HasValue then rule.State.Value.ToString() else "State not found") rule.VirtualNetworkResourceId) |> ignore
                        existingRules.AppendLine("-------------------------------") |> ignore
                    let tags = tags @ [ "existingVirtualNetworkRules", existingRules.ToString()
                                        "existingVirtualNetworkRuleCount", storageAccount.NetworkRuleSet.VirtualNetworkRules.Count.ToString()]

                    TraceTags.info "Found Azure Storage account; checking for existing virtual network connection before removing this one." tags
                    let ruleIndexOption =
                        storageAccount.NetworkRuleSet.VirtualNetworkRules
                        |> Seq.tryFindIndex (fun r -> r.VirtualNetworkResourceId.Equals(vnetResourceId))

                    match ruleIndexOption with
                    | None ->
                        TraceTags.warning "Did not find virtual network connection to Azure Storage account." tags
                        return None
                    | Some ruleIndex ->
                        TraceTags.info "Found virtual network connection to Azure Storage account. About to delete it."
                            (tags @ [   "ruleIndex", ruleIndex.ToString()
                                        "ruleToDelete", sprintf "%A" (storageAccount.NetworkRuleSet.VirtualNetworkRules.Item(ruleIndex).VirtualNetworkResourceId) ])
                        storageAccount.NetworkRuleSet.VirtualNetworkRules.RemoveAt(ruleIndex)

                        let update = Models.StorageAccountUpdateParameters(networkRuleSet = storageAccount.NetworkRuleSet)

                        let updatedRules:StringBuilder = StringBuilder()
                        for rule in storageAccount.NetworkRuleSet.VirtualNetworkRules do
                            updatedRules.AppendLine(sprintf "%s: %s" (if rule.State.HasValue then rule.State.Value.ToString() else "State not found") rule.VirtualNetworkResourceId) |> ignore
                            updatedRules.AppendLine("-------------------------------") |> ignore
                        let tags = tags @ [ "updatedVirtualNetworkRules", updatedRules.ToString()
                                            "updatedVirtualNetworkRuleCount", storageAccount.NetworkRuleSet.VirtualNetworkRules.Count.ToString()]

                        TraceTags.info "Listing updated virtual network connections before deleting one." tags
                        let! result = storage.StorageAccounts.UpdateWithHttpMessagesAsync(storageResourceGroupName, storageAccountName, update).AsAsync
                        return Some result.Body
            with ex ->
                let tags = tags @ [ "stackTrace", Environment.StackTrace
                                    "customMessage", "Exception while removing virtual network connection to Azure Storage firewall." ]
                TraceTags.trackException ex tags
                return None
        }

    let tryAddStorageAccountIpRule (listByResourceGroup:ListResources) azure
        resourceGroupName storageAccountName ipAddressOrRange =
        async {
            let tags = ["ipAddressOrRange", ipAddressOrRange
                        "storageAccountResourceGroup", resourceGroupName
                        "storageAccountName", storageAccountName
                        "currentMethod", "tryAddStorageAccountIpRule"]

            use! storage = newStorageContext azure
            let! storageAccount = tryGetStorageAccountByName listByResourceGroup azure
                                        resourceGroupName storageAccountName

            match storageAccount with
            | None ->
                return TraceTags.failwith "Could not add storage account IP rule: specified storage account was not found in Azure" tags
            | Some storageAccount ->
                let ruleExists =
                    storageAccount.NetworkRuleSet.IpRules
                    |> Seq.exists (fun r -> r.IPAddressOrRange.Equals(ipAddressOrRange))

                if ruleExists then
                    return Some storageAccount
                else
                    let ipRule = Models.IPRule(ipAddressOrRange, System.Nullable(Models.Action.Allow))

                    try
                        ipRule.Validate()
                        let update = Models.StorageAccountUpdateParameters(
                                            networkRuleSet = storageAccount.NetworkRuleSet)
                        update.NetworkRuleSet.IpRules.Add(ipRule)
                        let! result = storage.StorageAccounts.UpdateWithHttpMessagesAsync(resourceGroupName, storageAccountName, update).AsAsync
                        return Some result.Body
                    with
                    // This is expected to occur if we cannot find the VNET/Subnet that we are trying to bridge.
                    | IsAggregateOf SomeNetworkAclsValidationFailure e ->
                        TraceTags.info "Could not bridge ip to storage account. The operation failed Network ACL validation" tags
                        return None
            }

    /// Does the heavy lifting of looking up key mappings definitions in Azure
    let azureKeysLookup azure resourceGroupName =
        async {
            use! azureStorage = newStorageContext azure

            let! s = listStorageAccountsByResourceGroupAsync azureStorage resourceGroupName

            let keys =
                (Map.empty, s)
                ||> Seq.fold (fun map x ->
                    let keys = listStorageAccountKeysByResourceGroup azureStorage resourceGroupName x.Name
                    map.Add((resourceGroupName, x.Name), keys))
            return s, keys
        }

    /// This is used to lookup in case we cannot use the cache because the definition may change over time
    let noCacheKeysLookup =
        let lookup azure resourceGroupName =
            async {
                let! s, keys = azureKeysLookup azure resourceGroupName
                return s, keys.get_Item
            }
        lookup

    /// Cache resource group info if it is not cached already. Otherwise get resource group info from the cache.
    let cacheKeysLookup =
        // Note: only gets disposed when the program ends!
        let cache = new Microsoft.FSharpLu.Collections.CacheMap<string, Models.StorageAccount[] * Map<(string*string), Models.StorageAccountListKeysResult>>()

        let lookup azure resourceGroupName invalidateCache =
            async {
                match invalidateCache, cache.TryGetValue resourceGroupName with
                | false, Some (s, keyCache) ->
                    return s, keyCache.get_Item
                | _, _ ->
                    let! s, keys = azureKeysLookup azure resourceGroupName

                    cache.Add(resourceGroupName, (s, keys))
                    return s, keys.get_Item
            }
        lookup

/// List all storage accounts under a resource group and return the resource group, name, location and key
let getAllAccountsWithKeys  =
    Impl.getAllAccountsWithKeys (fun azure resourceGroupName -> Impl.cacheKeysLookup azure resourceGroupName false)

/// List storage accounts under a resource group with a specific tag-value pair
let getStorageAccountsByTag =
    Impl.getStorageAccountsByTag (fun azure resourceGroupName -> Impl.cacheKeysLookup azure resourceGroupName false)

/// Get the storage account in resourceGroup which matches the given name, throws exception if not found
let tryGetStorageAccountByName =
    Impl.tryGetStorageAccountByName (fun azure resourceGroupName -> Impl.cacheKeysLookup azure resourceGroupName false)

/// List storage accounts endpoints under a resource group with a specific tag-value pair
let getStorageEnpointsByTag =
    Impl.getStorageEnpointsByTag (fun azure resourceGroupName -> Impl.cacheKeysLookup azure resourceGroupName false)

/// Try to get the storage accounts key and endpoints under a resource group from the specifed account name
let tryGetStorageEndpointByName =
    Impl.tryGetStorageEndpointByName (fun azure resourceGroupName -> Impl.cacheKeysLookup azure resourceGroupName false)

/// Try to get the storage accounts key under a resource group from the specifed account name
let tryGetStorageNameKey =
    Impl.tryGetStorageNameKey (fun azure resourceGroupName -> Impl.cacheKeysLookup azure resourceGroupName false)

/// Try to get the storage accounts key under a resource group from the specifed account name
/// This method will force the refresh of the cache
let tryGetStorageNameKeyAndUpdateCacheEntry =
    Impl.tryGetStorageNameKey (fun azure resourceGroupName -> Impl.cacheKeysLookup azure resourceGroupName true)

/// Extracts the name of an Azure Storage account from the connection string.
let getStorageAccountNameFromConnectionString =
    Impl.getStorageAccountNameFromConnectionString

// Returns a map that associate storage account names to corresponding endpoint and key
let mapStorageEndpointsByName =
    Impl.mapStorageEndpointsByName (fun azure resourceGroupName -> Impl.cacheKeysLookup azure resourceGroupName false)

/// Updates the Default Network Rule for accessing the storage account (either allow by default or deny by default)
let setStorageAccountNetworkRuleDefaultAction =
    Impl.setStorageAccountNetworkRuleDefaultAction Impl.noCacheKeysLookup

/// Cleans up virtual network connections to Azure Storage from virtual networks that no longer exist.
let cleanUpDeletedVirtualNetworksFromAzureStorageFirewall =
    Impl.cleanUpDeletedVirtualNetworksFromAzureStorageFirewall Impl.noCacheKeysLookup

/// Add a Virtual Network rule to allow access to a storage account. Returns None on failure,
/// or Some StorageAccount (with updated rules) if it was successfully added
let tryAddStorageAccountVnetRule =
    Impl.tryAddStorageAccountVnetRule Impl.noCacheKeysLookup

/// Remove a Virtual Network rule to allow access to a storage account. Returns None if rule doesn't exist
/// or the updated StorageAccount if it existed.
let tryRemoveStorageAccountVnetRule =
    Impl.tryRemoveStorageAccountVnetRule Impl.noCacheKeysLookup

/// Adds a IP Rule to allow access to a storage account. Returns None on failure,
/// or Some StorageAccount (with updated rules) if it was successfully added
let tryAddStorageAccountIpRule =
    Impl.tryAddStorageAccountIpRule Impl.noCacheKeysLookup