namespace Microsoft.FSharpLu.Azure.Test.Storage

open System
open FsCheck
open Xunit

open Microsoft.Azure.Cosmos
open Microsoft.FSharpLu.StateMachineAgent

module Tests =
    open System.Collections.Generic
    open Microsoft.FSharpLu.Azure.Storage.Impl
    open Microsoft.FSharpLu.Azure.Test.Generators
    open Microsoft.Azure.Management.Storage

    let accountsByTag (listResourcesWithTag:ListResourcesWithTag<ListResources>) azure resourceGroupName =
        let withTag, tagged =
            async{
                let (ListResourcesWithTag (listByResourceGroup, (tagKey, tagValue), tagged)) = listResourcesWithTag

                let! withTag =
                    getStorageAccountsByTag listByResourceGroup azure resourceGroupName (tagKey, tagValue)

                return withTag, tagged
            } |> Async.RunSynchronously

        (fun () -> Seq.length withTag = Seq.length tagged)
        |> Prop.classify (Seq.isEmpty withTag) "Empty"
        |> Prop.classify (Seq.length withTag = Seq.length tagged) "Same length"
        |> Prop.classify (Seq.length withTag <> Seq.length tagged) "Lengths do not match"


    let endpointsByTag (listResourcesWithTag:ListResourcesWithTag<ListResources>) azure resourceGroupName =
        let endpointsWithTag, tagged, sameEndpoints =
            async{
                let (ListResourcesWithTag (listByResourceGroup, (tagKey, tagValue), tagged)) = listResourcesWithTag

                let! endpointsWithTag =
                    getStorageEnpointsByTag listByResourceGroup azure resourceGroupName (tagKey, tagValue)

                let sameEndpoints = (tagged, endpointsWithTag)
                                    ||> Seq.forall2 (fun acc (name, _, ep) -> acc.Name = name && acc.PrimaryEndpoints = ep)

                return endpointsWithTag, tagged, sameEndpoints
            } |> Async.RunSynchronously
        (fun () ->
            Seq.length endpointsWithTag = Seq.length tagged && sameEndpoints)
        |> Prop.classify (Seq.isEmpty endpointsWithTag) "Empty"
        |> Prop.classify (sameEndpoints) "Same Endpoints"
        |> Prop.classify (not sameEndpoints) "Endpoints do not match"
        |> Prop.classify (Seq.length endpointsWithTag = Seq.length tagged) "Same length"
        |> Prop.classify (Seq.length endpointsWithTag <> Seq.length tagged) "Different lengths"


    let accountsByKeys (resources: Resources<ListResources * Models.StorageAccount[] * Dictionary<string, Models.StorageAccountListKeysResult>>) azure resourceGroupName =
        let sameResourceGroup, xs, accounts, sameKey1s =
            async {
                let (Resources (listResources, accounts, keys)) = resources

                let! xs = getAllAccountsWithKeys listResources azure resourceGroupName
                let sameResourceGroup = xs |> Seq.forall(fun (g, _, _, _) -> g = resourceGroupName)
                let sameKey1s = xs |> Seq.forall(fun (_, name, _, key) -> keys.[name].Keys.[0].Value = key)
                return sameResourceGroup, xs, accounts, sameKey1s
            } |> Async.RunSynchronously

        (fun () -> Seq.length xs = Seq.length accounts && sameResourceGroup && sameKey1s)
        |> Prop.classify(Seq.isEmpty xs) "Empty"
        |> Prop.classify(Seq.length xs = Seq.length accounts) "Same lengths"
        |> Prop.classify(sameResourceGroup) "Same resource group"
        |> Prop.classify(sameKey1s) "Same keys"
        |> Prop.classify(Seq.length xs <> Seq.length accounts) "Lengths do not match"
        |> Prop.classify(not sameResourceGroup) "Resource Group does not match"
        |> Prop.classify(not sameKey1s) "Keys do not match"


    let endpointByName (resources: Resources<ListResources * Models.StorageAccount[] * Dictionary<string, Models.StorageAccountListKeysResult> * string Set>) azure resourceGroupName =
        let (Resources (listResources, accounts, keys, names)) = resources
        let name = if Set.isEmpty names then String.Empty else Seq.head names
        let x, y =
            async {
                let! s, listKeys = listResources azure resourceGroupName
                let! xs = tryGetStorageEndpointByName listResources azure resourceGroupName name
                return s |> Seq.tryFind(fun x -> x.Name = name), xs
            } |> Async.RunSynchronously
        (fun () ->
            match x, y with
            | None, None | Some _, Some _ -> true
            | Some _, None | None, Some _ -> false)
        |> Prop.classify (match x, y with None, None -> true | _ -> false) "Expected not to find"
        |> Prop.classify (match x, y with Some n, Some(m, _, _) when n.Name = m -> true | _ -> false) "Found"
        |> Prop.classify (match x, y with Some n, Some(m, _, _) when n.Name <> m -> true | _ -> false) "Found wrong one"
        |> Prop.classify (match x, y with Some _, None -> true | _ -> false) "Not found when expected to find"
        |> Prop.classify (match x, y with None, Some _ -> true | _ -> false) "Found when not supposed to find"

    /// TODO This test is broken. It had a bug that made it pass, the fix breaks the test. disabling it for now.
    let mapEndpointsByName (resources: Resources<ListResources * Models.StorageAccount[] * Dictionary<string, Models.StorageAccountListKeysResult> * string Set>) azure resourceGroupName =
        let names, map =
            async {
                let (Resources (listResources, accounts, keys, names)) = resources
                let! s, listKeys = listResources azure resourceGroupName
                let! xs = mapStorageEndpointsByName listResources azure resourceGroupName names
                return names, xs
            } |> Async.RunSynchronously
        (fun () ->
            names.Count = map.Count &&
            (Seq.forall(fun name ->
                (Map.tryFind name map |> Option.bind id).IsSome) names)
                )
        |> Prop.classify (names.IsEmpty) "Empty"
        |> Prop.classify (names.Count > map.Count) "Final map does not have all the names"
        |> Prop.classify (names.Count < map.Count) "Final map has more names than supplied"
        |> Prop.classify (names.Count = map.Count) "Same length"


open Tests

[<Trait("TestCategory", "Azure Storage")>]
type AzureStorageTests () =
    let generatorRegistration = Arb.register<Microsoft.FSharpLu.Azure.Test.Generators.Generators>()

    [<Fact>]
    member __.StorageAccountsWithKeys() =
        Check.QuickThrowOnFailure accountsByKeys

    [<Fact>]
    member x.StorageAccountsByTag() =
        Check.QuickThrowOnFailure accountsByTag

    [<Fact>]
    member x.StorageEndpointsByTag() =
        Check.QuickThrowOnFailure endpointsByTag

    [<Fact>]
    member x.StorageEndpointByName() =
        Check.QuickThrowOnFailure endpointByName

    [<Fact(Skip="Needs Azure Storage key configuration")>]
    member x.``Azure Table implementation of Agent Storage interface is atomic``() =
        async {
            let storageAccountConnectionString = 
                failwithf "Please set azure storage account connectiong string before running this test"
            
            let tableName = "AgentJoinStorageTable"

            let joinId = 
                {
                    guid = System.Guid.NewGuid()
                    timestamp = System.DateTimeOffset.UtcNow
                }

            let storageAccount = Table.CloudStorageAccount.Parse(storageAccountConnectionString)

            let! storage =
                AzureTableJoinStorage.newStorage
                                        storageAccount
                                        tableName
                                        (System.TimeSpan.FromSeconds(1.0))
                                        (System.TimeSpan.FromSeconds(10.0))
                                        "testAgent"

            let entry: Agent.Join.Entry<int> =
                {
                    whenAllSubscribers = []
                    whenAnySubscribers = []
                    status = Agent.Join.Status.Requested
                    childrenStatuses = Map.empty
                    parent = None
                    created = System.DateTimeOffset.UtcNow
                    modified = System.DateTimeOffset.UtcNow
                }

            do! storage.add joinId entry
            let! v = storage.get joinId

            let expectedEntry =
                {
                    entry with
                        whenAllSubscribers = [7;8;9]
                        whenAnySubscribers = [9;8;7]
                        parent = Some joinId
                        status = Agent.Join.Status.Completed
                }

            // records will be updated in random order, and there will be one record that contains
            // all updates. That recrod has to have same data as expectedEntry
            let! results =
                Async.Parallel [
                    storage.update joinId (fun entry -> {entry with whenAllSubscribers = [7;8;9]; whenAnySubscribers = [9;8;7]})
                    storage.update joinId (fun entry -> {entry with parent = Some joinId})
                    storage.update joinId (fun entry -> {entry with status = Agent.Join.Status.Completed})
                ]

            let areTheSame = results |> Seq.exists(fun r -> r = expectedEntry)

            Assert.True(areTheSame)
        } |> Async.RunSynchronously


    /// TODO: 2680 Broken test
    // [<Fact>]
    //member __.MapStorageEndpointsByName() =
    //    Check.QuickThrowOnFailure mapEndpointsByName