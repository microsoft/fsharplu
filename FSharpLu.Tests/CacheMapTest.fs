namespace Microsoft.FSharpLu.CacheMap

open FsCheck
open Microsoft.FSharpLu.Collections

module CacheMapTest =

    type private Cmd<'Model, 'Actual>(name: string, runActual: 'Actual->'Actual, runModel: 'Model -> 'Model, check: 'Actual * 'Model -> Property) =
        inherit Command<'Actual, 'Model>()

        override this.RunActual(actual) = runActual actual
        override this.RunModel(model) = runModel model
        override this.Post(actual, model) = check (actual, model)
        override this.ToString() = name

    let cmd (commandName: string) (runActual: 'Actual -> 'Actual) (runModel: 'Model -> 'Model) (check: 'Actual * 'Model -> Property) =
        Cmd (commandName, runActual, runModel, check) :> Command<'Actual, 'Model>

    let errMsg (expected: Map<_, _>) (actual: CacheMap<_, _>) =
        let br = "\n============================\n"
        sprintf "%sExpected: %A%sActual: %A%s" br expected br actual.Snapshot br

    let add k v =
        let s = sprintf "Add: %O, %O" k v
        cmd s
            (fun (cacheMap: CacheMap<_, _>) -> cacheMap.Add(k, v); cacheMap)
            (fun (map: Map<_, _>) -> map.Add(k, v))
            (fun (cacheMap, map) ->
                (cacheMap.Snapshot = map)

                |@ s + errMsg map cacheMap)

    let clear() =
        let s = "Clear"
        cmd s
            (fun (cacheMap: CacheMap<_, _>) -> cacheMap.Clear(); cacheMap)
            (fun _ -> Map.empty)
            (fun (cacheMap, map) ->
                (cacheMap.Snapshot = map)

                |@ s + errMsg map cacheMap)

    let contains k v =
        let s = sprintf "Contains: %O, %O" k v
        cmd s id id
            (fun (cacheMap: CacheMap<_, _>, map: Map<_, _>) ->
                let b = if map.ContainsKey k then map.[k] = v else false
                cacheMap.Contains(k, v) = b

                |@ s + errMsg map cacheMap)

    let containsKey k =
        let s = sprintf "ContainsKey: %O" k
        cmd s id id
            (fun (cacheMap: CacheMap<_, _>, map: Map<_, _>) ->
                cacheMap.ContainsKey k = map.ContainsKey k

                |@ s + errMsg map cacheMap)

    let count() =
        let s = "Count"
        cmd s id id
            (fun (cacheMap: CacheMap<_, _>, map: Map<_, _>) ->
                cacheMap.Count = map.Count

                |@ s + errMsg map cacheMap)

    let itemGetter k =
        let s = sprintf "Item.get: %O" k

        cmd s id id
            (fun (cacheMap: CacheMap<_, _>, map: Map<_, _>) ->
                let model =
                    try
                        Some map.[k]
                    with
                    | _ -> None

                let actual =
                    try
                        Some cacheMap.[k]
                    with
                    | _ -> None

                model = actual

                |@ s + errMsg map cacheMap)

    let itemSetter k v =
        let s = sprintf "Item.set: %O, %O" k v
        cmd s
            (fun (cacheMap: CacheMap<_, _>) -> cacheMap.[k] <- v; cacheMap)
            (fun (map: Map<_, _>) -> map.Add(k, v))
            (fun (cacheMap, map) ->
                cacheMap.Snapshot = map

                |@ s + errMsg map cacheMap)


    let removeByKey (k: 'Key) =
        let s = sprintf "RemoveByKey: %O" k
        let mutable expectedRemoved, actualRemoved = false, false
        cmd s
            (fun (cacheMap: CacheMap<_, _>) ->
                actualRemoved <- cacheMap.Remove k
                cacheMap
            )
            (fun (map: Map<_, _>) ->
                expectedRemoved <- map.ContainsKey k
                map.Remove k)
            (fun (cacheMap: CacheMap<_, _>, map: Map<_, _>) ->
                (expectedRemoved = actualRemoved && map = cacheMap.Snapshot)

                |@ s + errMsg map cacheMap)

    let remove k v =
        let s = sprintf "Remove: %O, %O" k v
        let mutable expectedRemoved, actualRemoved = false, false
        cmd s
            (fun (cacheMap: CacheMap<_, _>) ->
                actualRemoved <- cacheMap.Remove(k, v)
                cacheMap
            )
            (fun (map: Map<_, _>) ->
                expectedRemoved <- if map.ContainsKey k then map.[k] = v else false
                if expectedRemoved then
                    map.Remove(k)
                else
                    map
            )
            (fun (cacheMap: CacheMap<_, _>, map: Map<_, _>) ->
                (expectedRemoved = actualRemoved && map = cacheMap.Snapshot)

                |@ s + errMsg map cacheMap)

    let tryGetValue k =
        let s = sprintf "tryGetValue: %O" k
        cmd s id id
            (fun (cacheMap: CacheMap<_, _>, map: Map<_, _>) ->
                match cacheMap.TryGetValue k, map.TryFind k with
                | Some v, Some u -> v = u
                | None, None -> true
                | _, _ -> false

                |@ s + errMsg map cacheMap)

    let spec =
        { new ICommandGenerator<CacheMap<_, _>, Map<_, _>> with
            member this.InitialActual = new CacheMap<_, _>()
            member this.InitialModel = Map.empty
            member this.Next model =
                gen{
                    let! kv = Arb.Default.KeyValuePair<string, _>().Generator
                    return! Gen.oneof [
                                        gen{return add kv.Key kv.Value}
                                        gen{return clear()}
                                        gen{return contains kv.Key kv.Value}
                                        gen{return containsKey kv.Key}
                                        gen{return count()}
                                        gen{return itemGetter kv.Key}
                                        gen{return itemSetter kv.Key kv.Value}
                                        gen{return removeByKey kv.Key}
                                        gen{return remove kv.Key kv.Value}
                                        gen{return tryGetValue kv.Key}
                                      ]
                }
        }


open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type CacheMapTests() =

    [<ClassInitialize>]
    static member init(context : TestContext) =
        ()

    [<TestMethod>]
    [<Description("Fuzz Collections.CacheMap")>]
    [<TestCategory("Utilities")>]
    member this.CacheMap() =
        Check.QuickThrowOnFailure (Command.toProperty CacheMapTest.spec)


