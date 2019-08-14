namespace Microsoft.FSharpLu.StateMachine.Agent
open System
open Xunit
open Microsoft.FSharpLu.Actor.StateMachine
open Microsoft.FSharpLu.Actor.StateMachine.Agent

[<Trait("TestCategory", "StateMachineAgent")>]
module StateMachineAgentTests =

    type TestStates =
        | State1 of int
        | State2 of string
        | State3 of int * string
        | State4
        | Join1 of JoinId
        | Join2 of JoinId * string
        | Join3 of JoinId * int * string
        | Exit

        | Fork1
        | Fork2 of JoinId
        | WaitForFork2 of JoinId * JoinId
        | WaitForFork1 of JoinId

    let rec newAgent(storage) : Agent.Agent<_, _, _> =
        let spawn (metadata: Agent.RequestMetadata, state: TestStates) =
            async {
                printfn "Operations.post %A" (metadata, state)
                let agent = newAgent(storage)
                let! _ = Agent.executeWithResult state metadata agent
                return ()
            }
        {
            title = "Agent based persisting state in-memory"

            //for now just wing it
            logger = fun s tags -> printfn "%s %A" s tags
            tags = ["AgentType", "InMemory"]

            transition = fun (s: TestStates) ->
                            async {
                                match s with
                                | State1 n ->
                                    return ForkAndGoto([State4; State4], fun joinId -> Join1 joinId)
                                | State2 s  ->
                                    return ForkAndGoto ([], fun joinId -> Join2(joinId, s))
                                | State3(n,s) ->
                                    return ForkAndGoto ([State4; Exit], fun joinId -> Join3(joinId, n, s))
                                | State4 ->
                                    return Return 12

                                | Exit ->
                                    printfn "Exit state"
                                    return Return 10
                                | Join1 joinId ->
                                    printfn "Got Join1: %A" joinId
                                    return WhenAll(joinId, Exit)
                                | Join2(joinId, s) ->
                                    printfn "Got Join2: %A" joinId
                                    return WhenAll(joinId, State4)
                                | Join3(joinId, n, s) ->
                                    return WhenAny(joinId, State4)

                                // Double fork
                                | Fork1 ->
                                    return ForkAndGoto([State4; Exit], fun joinId -> Fork2 joinId)
                                | Fork2(fork1JoinId) ->
                                    return ForkAndGoto([State4; State4], fun joinId -> WaitForFork2(fork1JoinId, joinId))
                                | WaitForFork2(fork1JoinId, fork2JoinId) ->
                                    return WhenAny(fork2JoinId, WaitForFork1 fork1JoinId)
                                | WaitForFork1(fork1JoinId) ->
                                    return WhenAll(fork1JoinId, State4)

                            }

            maximumInprocessSleep = TimeSpan.FromSeconds 1.0

            scheduler =  {
                joinStore = storage
                spawn = spawn
                onGoto = fun s -> async.Return ()
                onInProcessSleep = fun _ -> async.Return ()
                embed = fun m s -> (m, s)
                embedCallReturnValue = {
                    new ICallReturnEmbedder<_> with
                    member __.embed<'t> (t:'t) m = m // No Call instruction used in the state machine
                }
            }
        }
    
    type SpawnRequest = RequestMetadata * TestStates

    [<Fact>]
    let waitAllSuccessForkTest() =
        async {
            let storage = Collections.Concurrent.ConcurrentDictionary()
            Assert.Empty storage
            let agent = newAgent (InMemory.newJoinStorageOf storage)
            let! request = Agent.createRequest<SpawnRequest> agent.scheduler.joinStore
            let! result = Agent.executeWithResult (State1 23) request agent
            Assert.Equal(4, storage |> Seq.filter (fun x -> x.Value.status = Agent.Join.Status.Completed) |> Seq.length)
            Assert.Equal(4, storage.Count)
        } |> Async.RunSynchronously

    [<Fact>]
    let expectFailureOnEmptyForkSpawnList() =
        async {
            let storage = Collections.Concurrent.ConcurrentDictionary<JoinId, _>()
            Assert.Empty storage
            let! _ =
                Assert.ThrowsAsync<System.NotSupportedException>(
                    fun () ->
                        async {
                            let agent = newAgent(InMemory.newJoinStorageOf storage)
                            let! request = Agent.createRequest<SpawnRequest> agent.scheduler.joinStore
                            let! _ = Agent.executeWithResult (State2 "blah") request agent
                            return ()
                        } |> Async.StartAsTask :> System.Threading.Tasks.Task
                    ) |> Async.AwaitTask
            Assert.Equal(1, storage.Count)
            return ()
        } |> Async.RunSynchronously

    [<Fact>]
    let waitAnySuccessForkTest() =
        async {
            let storage = Collections.Concurrent.ConcurrentDictionary()
            Assert.Empty storage
            let agent = newAgent(InMemory.newJoinStorageOf storage)
            let! request = Agent.createRequest<SpawnRequest> agent.scheduler.joinStore
            let! _ =  Agent.executeWithResult (State3 (1, "blah")) request agent
            
            let completedChild =
                    storage
                    |> Seq.tryFind (fun x -> x.Value.status = Agent.Join.Status.Completed && x.Value.parent.IsSome)
            Assert.True(completedChild.IsSome)
            Assert.True(storage
                            |> Seq.exists (fun x ->
                                                x.Value.status = Agent.Join.Status.Completed &&
                                                not x.Value.childrenStatuses.IsEmpty &&
                                                x.Value.childrenStatuses.[completedChild.Value.Key.guid] = Agent.Join.Status.Completed
                                            )
                        )
            Assert.Equal(4, storage.Count)
            Assert.Equal(2, storage |> Seq.filter (fun x -> x.Value.parent.IsSome) |> Seq.length)
            Assert.Equal(4, storage |> Seq.filter (fun x -> x.Value.status = Agent.Join.Status.Completed) |> Seq.length)
            Assert.Equal(1, storage |> Seq.filter (fun x -> not x.Value.childrenStatuses.IsEmpty) |> Seq.length)
        } |> Async.RunSynchronously

    [<Fact>]
    let doubleFork() =
        async {
            let storage = Collections.Concurrent.ConcurrentDictionary()
            Assert.Empty storage
            let agent = newAgent(InMemory.newJoinStorageOf storage)
            let! request = Agent.createRequest<SpawnRequest> agent.scheduler.joinStore
            let! _ = Agent.executeWithResult (Fork1) request agent

            Assert.Equal(7, storage |> Seq.filter(fun x -> x.Value.status = Agent.Join.Status.Completed) |> Seq.length)
            Assert.Equal(4, storage |> Seq.filter(fun x -> x.Value.parent.IsSome) |> Seq.length)
            Assert.Equal(2, storage |> Seq.filter(fun x -> not x.Value.childrenStatuses.IsEmpty) |> Seq.length)
            Assert.Equal(7, storage.Count)
        } |> Async.RunSynchronously

    [<Fact>]
    let joinOnNonExistantJoinId() =
        async {
            let storage = Collections.Concurrent.ConcurrentDictionary()
            Assert.Empty storage
            let agent = newAgent(InMemory.newJoinStorageOf storage)
            let! request = Agent.createRequest<SpawnRequest> agent.scheduler.joinStore
            let! _ =
                Assert.ThrowsAnyAsync(
                    fun () ->
                         Agent.executeWithResult (Join1 { guid = System.Guid.Empty
                                                          timestamp = System.DateTimeOffset.UtcNow }) request agent
                         |> Async.StartAsTask :> System.Threading.Tasks.Task
                ) |> Async.AwaitTask
            return ()
        } |> Async.RunSynchronously
