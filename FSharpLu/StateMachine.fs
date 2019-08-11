/// State machine and agent
namespace Microsoft.FSharpLu.Actor.StateMachine

open FSharp.Control

/// ID used to identify joining points (either an agent request or a fork)
type JoinId =
    {
        /// Unique GUID identifying the join entry
        guid : System.Guid
        /// Timestamp when the join was created
        timestamp : System.DateTimeOffset
    }

/// A state machine transition with return type 't.
/// state type 's, and a set of callable state machines represented by type 'm
type Transition<'s, 't, 'm> =
    /// Sleep for the specified amount of type and come back to the same state
    | Sleep of System.TimeSpan
    /// Sleep for the specified amount of type and then transition to the specified state
    | SleepAndGoto of System.TimeSpan * 's
    /// Go to the specified state
    | Goto of 's
    /// Reached a final state
    | Return of 't
    /// Suspend the state machine and spawn a new state machine (type 'm)
    | Coreturn of 'm
    /// Fork the agent into multiple new instances of the same agent spawned at different states,
    /// then move the current agent instance to a new state without
    /// waiting for the spawned agents to complete.
    /// Takes:
    ///    - a list of states to spawn new agents at,
    ///    - a function that given a joinId, returns the next state the current agent should go to.
    | ForkAndGoto of 's list * (JoinId -> 's)
    /// Go to the specified state when all the child requests of the specified fork have completed
    | WhenAll of JoinId * 's
    /// Go to the specified state when any child request of the specified fork has completed
    | WhenAny of JoinId * 's
    /// Call another request 'm (with provided JoinId metadata) and when the callee returns, continue the 
    /// execution of the current agent at state 's
    | Call of (JoinId -> 'm) * 's 

/// A state machine with simple transitions (no support for Forking, Coroutines nor out-of-process pause/resume)
module SimpleStateMachine =

    /// Execute the state machine and call transitionCallback on each state transition.
    /// The call-back function can be used to report progress, send a heartbeat
    /// or persist the state (e.g. on an Azure Queue) to allow resuming processing
    /// at a later stage in case of process failure (crash or Azure migration).
    //
    /// This version of state machine execution implements 'sleep' using in-process asynchronous sleep (Async.Sleep).
    /// See Agent module below for a version implementing out-of-process asynchronous sleep.
    let rec execute (transitionFunction:'s -> Async<Transition<'s, 't, 'm>>) initialState transitionCallback =
        let rec goto currentState =
            async {
                let! operation = transitionFunction currentState
                match operation with
                | Sleep delay ->
                    // Pause asynchronously (in-process)
                    do! Async.Sleep(delay.TotalMilliseconds |> int)
                    return! goto currentState

                | SleepAndGoto (delay, state) ->
                    do! Async.Sleep(delay.TotalMilliseconds |> int)
                    return! goto state

                | Return result ->
                    return result

                | Coreturn _ ->
                    return failwith "StateMachine.execute does not support `Coreturn` since it only runs within a single state machine."

                | ForkAndGoto _ ->
                    return failwith "StateMachine.execute does not support `Fork` since it only runs within a single state machine."

                | WhenAll _ ->
                    return failwith "StateMachine.execute does not support `WhenAll` since it only runs within a single state machine."

                | WhenAny _ ->
                    return failwith "StateMachine.execute does not support `WhenAny` since it only runs within a single state machine."
        
                | Call _ ->
                    return failwith "StateMachine.execute does not support `Call` since it only runs within a single state machine."

                | Goto newState ->
                    do! transitionCallback newState
                    return! goto newState
            }
        goto initialState


/// An agent state machine that supports out-of-process sleeping,
/// spawning new agents, coreturning to another agent,
/// forking mulitple agents and joining on a fork.
///
/// An instance of an agent processes a __request__.
/// The `Agent.execute` function is responsible for advancing the state of the agent state machine
/// as per the trasnition system defines by the agent in `Agent.transition`.
/// A request __completes__ when the agent state machine reaches a `Return` transition, after which case
/// `Agent.execute` returns the result (of type 't) to the caller.
///
/// Requests to spawn new agents get offloaded to an external scheduler
/// implemented by the API user. This can be done simply by calling Agent.execute
/// in a background thread, or possibly by queuing up request on some queue (e.g. Azure Queue)
/// This gives flexibility to the API user in scheduling execution of the agent state machine.
module Agent =

    /// Interface defining operations required to "execute" an agent.
    ///
    /// This is the "assembly code" of the state machine. These are basic instructions
    /// that need to be implemented by any agent scheduler (responsibility of the API user).
    ///
    /// This gets returned by the `Agent.execute` function when the state machine
    /// returns (`Return` transition) or goes to sleep (out-of-process `Sleep` transition).
    /// The user of the API is responsible for providing, as part of its scheduling system,
    /// an implemention that conveys the semantic of the values defined below.
    /// It is up to the API user to decide when to execute the spawn agent
    /// (e.g. immediately in a background thread, queued up for later on some
    /// queuing device...)
    ///
    /// Type parameters:
    ///   'm represents a agent scheduling request.
    ///      It embeds the desired state of the agent and the associated request metadata.
    ///
    ///   't is the returned type of the agent
    ///
    type ExecutionInstruction<'s, 't, 'm> =
        /// The request has been processed and can be removed from the scheduling system
        | Completed of 't option

        /// The request has been suspended (e.g. waiting for another call to return)
        /// and should be removed from the scheduling system
        | Suspended

        /// The request completes and at the same time spawn another request
        | Coreturn of 'm

        /// The processing of the request must be paused for the specified amount of time,
        /// once it the time out expires, the scheduler is responsible for calling `Agent.execute`
        /// to resume processing at the same state as where it left off.
        | SleepAndResume of System.TimeSpan

        /// Processing must be paused for the specified amount of time.
        /// Once the timeout expires, the scheduler is responsible for calling `Agent.execute`
        /// to resume processing at state 's
        | SleepAndResumeAt of System.TimeSpan * 's


    /// Metadata associated with an agent request
    type RequestMetadata = JoinId

    /// Defines the storage interface required to support Join and Fork.
    module Join =
        /// Status of a join point
        type Status =
            | Requested
            | Waiting
            | Completed

        /// Status of a join's children
        type ChildrenStatus = Map<System.Guid, Status>

        /// List of subscriptions to a join condition.
        /// Encoded as requests to be posted via the Scheduler when the join condition is met.
        type Subscriptions<'m> = 'm list

        /// A persistable entry storing information about a join
        type Entry<'m> =
            {
                /// Subscribers to notify when all children completes
                whenAllSubscribers : Subscriptions<'m>
                /// Subscribers to notify when any child completes
                whenAnySubscribers : Subscriptions<'m>
                /// For a fork: status is Completed if all children status are completed
                /// For a request: status is Completed if the request as completed (Transition.Return was called)
                status : Status
                /// Cache the status of the children of the join
                childrenStatuses : ChildrenStatus
                /// Optional join parent if this entry is involved in a join
                parent : JoinId option
                /// Timestamp when the join entry was created
                created : System.DateTimeOffset
                /// Timestamp when the join entry was last modified
                modified : System.DateTimeOffset
            }

        /// Storage provider interface used to persist 
        /// spawning requests for joining points (either an agent request or a fork point)
        /// Requirement: tne underlying READ/WRITE opeations need to be atomic.
        type IStorage<'m> =
            /// An a new entry to track a request
            abstract add : JoinId -> Entry<'m> -> Async<unit>
            /// Update the state of a fork
            abstract update : JoinId -> (Entry<'m> -> Entry<'m>) -> Async<Entry<'m>>
            /// Get the state of a fork
            abstract get : JoinId -> Async<Entry<'m>>


    /// A scheduler responsible for scheduling execution of agents via calls to Agent.execute.
    ///
    /// In order to support:
    ///
    ///  - __pausing/resuming__ (and out-of-process asynchronous sleep). The scheduler must implement state persistence on an external device
    ///    (e.g. Azure Queue, Azure Service Bus...).
    ///    This is provided by function `Agent.persist` which gets called to persist the machine state and associated metadata in some external storage.
    ///
    ///  - __out-of-process asynchronous sleep__ the function calling .execute must handle the actions `RequestAction.PostponeFor` and `RequestAction.PostponeAndReplace`
    ///    For instance by posting the serialize state 'm onto some queue for later processing.
    ///
    ///  - __corountines__ and the `CoReturn` transition, the scheduler must handle spawning new agents
    ///    by implementing `Agent.operations.spawn`.
    ///
    ///  - __forking__ (Fork, WhenAny, WhenAll), the scheduler must provides an external storage interface `Agent.storage`
    ///    of type `JoinStorage` implementing atomic add/update storage functions
    ///
    /// Type parameters:
    ///   's is the type of states of the state machine
    ///   'm represents a agent scheduling request. It embeds the desired state of the agent and the associated request metadata.
    type Scheduler<'s, 'm> =
        {
            /// Called to notify the scheduler that the state machine
            /// execution is about to sleep in-process
            onInProcessSleep : System.TimeSpan -> Async<unit>

            /// Called to notify the scheduler that the state machine
            /// is about to transition to a new state
            onGoto : 's -> Async<unit>

            /// Embed the specified state and request metadata into a scheduling
            /// request spawning for a new instance of the same state machine agent
            embed : RequestMetadata -> 's -> 'm

            /// Request spawning of a new state machine agent (with embedded state and metadata)
            spawn : 'm -> Async<unit>

            /// Provides an implementation of join storage (e.g. using Azure Table)
            joinStore : Join.IStorage<'m>
        }

    /// An agent state machine with return type 't, underlying state type 's,
    /// and scheduling request of type 'm.
    ///
    /// Type parameters:
    ///   's is the type of states of the state machine
    ///
    ///   't is the returned type of the agent
    ///
    ///   'm represents an agent scheduling request. It embeds the desired state of the agent and the associated request metadata.
    ///
    [<NoEquality;NoComparison>]
    type Agent<'s, 't, 'm> =
        {
            /// Title of the state machine
            title : string

            /// Trace logging function
            logger : string -> (string * string) list ->  unit

            /// List of tags to associate to each logging message
            tags : (string * string) list

            /// Transition function: given a set of available operations and the current state returns the next transition.
            transition : 's -> Async<Transition<'s, 't, 'm>>

            /// Amount of time that is considered short enough to asynchronously sleep 'in-process'
            /// rather than out-of-process
            maximumInprocessSleep : System.TimeSpan

            /// The scheduler responsible for execution the agent
            scheduler : Scheduler<'s, 'm>
        }

    /// Create a new request attached to the specified parent JoinId
    let private createRequestWithParent<'m> (joinStore:Join.IStorage<'m>) parent =
        async {
            let requestId =
                {
                    guid = System.Guid.NewGuid()
                    timestamp = System.DateTimeOffset.UtcNow
               }

            // Create a join entry for the request
            do! joinStore.add
                    requestId
                    { status = Join.Status.Requested
                      whenAllSubscribers = []
                      whenAnySubscribers = []
                      childrenStatuses = Map.empty
                      parent = parent
                      created = requestId.timestamp
                      modified = requestId.timestamp }
            return requestId
        }

    /// Create a new request. Should be called by the API consumer to initialize a new request
    /// to be executed with an agent and passed to `Agent.execute`
    let public createRequest<'m> (joinStore:Join.IStorage<'m>) =
        createRequestWithParent<'m> joinStore None

    /// Return true if all children are marked as completed
    let private allCompleted childrenStatus =
        childrenStatus
        |> Map.forall (fun _ t -> t = Join.Status.Completed)

    /// Return true if at least one child is marked as completed
    let private atleastOneCompleted childrenStatus =
        childrenStatus
        |> Map.exists (fun _ t -> t = Join.Status.Completed)

    /// Mark a request as completed and recursively
    /// update status of (fork) parents
    let private markRequestAsCompleted (m:Agent<'s, 't, 'm>) (metadata:RequestMetadata) =
        async {
            let! updatedEntry =
                 m.scheduler.joinStore.update
                    metadata
                    (fun u -> { u with status = Join.Status.Completed
                                       modified = System.DateTimeOffset.UtcNow } )

            match updatedEntry.parent with
            | None -> return ()
            | Some joinId ->
                let mutable whenAnySubscriptions = []

                // Update child status under the request's parent entry
                let! joinEntry =
                    m.scheduler.joinStore.update joinId
                        (fun driftedJoinEntry ->
                            // We need to check status from the drifeted entry in case
                            // other siblings have completed sine the update started

                            // Mark the child request as completed
                            let newChildrenStatus = Map.add metadata.guid Join.Status.Completed driftedJoinEntry.childrenStatuses

                            let newStatus =
                                if allCompleted newChildrenStatus then
                                    // All siblings completed => we mark the fork as completed
                                    Join.Status.Completed
                                else
                                    // the join  "whenall" condition is not met yet.
                                    driftedJoinEntry.status

                            whenAnySubscriptions <- driftedJoinEntry.whenAnySubscribers
                            { driftedJoinEntry with
                                    status = newStatus
                                    childrenStatuses = newChildrenStatus
                                    whenAnySubscribers = []
                                    modified = System.DateTimeOffset.UtcNow }
                        )

                // Honor the "WhenAny" subscriptions
                for schedulerSpawnRequest in whenAnySubscriptions do
                    do! m.scheduler.spawn schedulerSpawnRequest

                // Honor the "WhenAll" subscriptions
                if joinEntry.status = Join.Status.Completed then
                    // We are necessarily the last one in the fork here since we held on to one of the fork's child
                    // while trying to update the fork data

                    // Signal the fork's subscribers
                    for schedulerSpawnRequest in joinEntry.whenAllSubscribers do
                        do! m.scheduler.spawn schedulerSpawnRequest
        }

    /// Advance execution of an agent state machine
    /// until reaching either completion or out-of-process sleep.
    /// The state gets persisted on every transition
    /// which allows resuming the execution in case of failure.
    let public executeWithResult (initialState:'s) requestMetadata (m:Agent<'s, 't, 'm>) : Async<ExecutionInstruction<'s, 't, 'm>> =

        let tags = m.tags@["requestGuid", requestMetadata.guid.ToString()]
        let rec sleepAndGoto delay (nextState:'s) =
            async {
                if delay < m.maximumInprocessSleep then
                    m.logger (sprintf "Agent '%s' sleeping for %O in proc" m.title delay) tags
                    do! m.scheduler.onInProcessSleep delay // currentState delay
                    // Short-enough to wait asynchronously in-process
                    do! Async.Sleep(delay.TotalMilliseconds |> int)
                    return! goto nextState
                else
                    m.logger (sprintf "Agent '%s' sleeping for %O out of proc" m.title delay) tags
                    // For longer waits we wait asynchronously out-of-process using the external storage device
                    // and postpone the request until later
                    return ExecutionInstruction.SleepAndResumeAt(delay, nextState)
            }

        and goto (state:'s) =
            async {
                do! m.scheduler.onGoto state
                return! runAt state
            }

        and runAt (currentState:'s) =
            async {
                let! operation = m.transition currentState
                match operation with
                | Transition.Sleep delay ->
                    return! sleepAndGoto delay currentState

                | Transition.SleepAndGoto (delay, nextState) ->
                    return! sleepAndGoto delay nextState

                | Transition.Return result ->
                    m.logger (sprintf "Agent '%s' reached final state and returned %A" m.title result) tags
                    do! markRequestAsCompleted m requestMetadata
                    return ExecutionInstruction.Completed (Some result)

                | Transition.Coreturn callee ->
                    m.logger (sprintf "Agent '%s' coreturning to another state machine" m.title) tags
                    //do! m.scheduler.spawn requestMetadata requestCallee
                    return ExecutionInstruction.Coreturn callee

                | Transition.Goto newState ->
                    m.logger (sprintf "Agent '%s' at state %O" m.title newState) tags
                    return! goto newState

                | Transition.ForkAndGoto (spawnedStates, newStateFromJoinId) ->
                    if List.isEmpty spawnedStates then
                        return raise (System.NotSupportedException("ForkAndGoto does not accept empty spawned states list"))

                    // spawn children state machines
                    m.logger (sprintf "Agent '%s' forking into %d state machines" m.title spawnedStates.Length) tags

                    let joinId =
                        {
                            guid = System.Guid.NewGuid()
                            timestamp = System.DateTimeOffset.UtcNow
                        }

                    // create a request storage entry for every child
                    let! childrenMetdata =
                        spawnedStates
                        |> AsyncSeq.ofSeq
                        |> AsyncSeq.foldAsync
                            (fun spawnChildrenSoFar spawnState ->
                                async {
                                    let! metadata = createRequestWithParent<'m> m.scheduler.joinStore (Some joinId)
                                    return Map.add metadata.guid (metadata, spawnState) spawnChildrenSoFar
                                }
                            )
                            Map.empty

                    // create the join entry
                    let now = System.DateTimeOffset.UtcNow
                    do! m.scheduler.joinStore.add joinId
                            {
                                status = Join.Status.Waiting
                                childrenStatuses = childrenMetdata |> Map.map (fun id metadata -> Join.Requested)
                                whenAllSubscribers = []
                                whenAnySubscribers = []
                                parent = None
                                created = now
                                modified = now
                            }

                    // spawn the child processes (Note: this cannot be done before the above join entry is created)
                    let! childrenIds =
                        childrenMetdata
                        |> Map.toSeq
                        |> AsyncSeq.ofSeq
                        |> AsyncSeq.iterAsync
                            (fun (childId, (childMetadata, spawnState)) ->
                                async {
                                    do! m.scheduler.spawn (m.scheduler.embed childMetadata spawnState)
                                }
                            )

                    return! goto (newStateFromJoinId joinId)

                /// Call another state machine agent and move to the specified state when it completes
                | Transition.Call (calleeRequestBuilder, returnState) ->
                    
                    // allocate an ID for the caller's join entry
                    let callerJoinId =
                        {
                            guid = System.Guid.NewGuid()
                            timestamp = System.DateTimeOffset.UtcNow
                        }

                    // create a join entry for the callee
                    let! calleeMetdata = createRequestWithParent<'m> m.scheduler.joinStore (Some callerJoinId)

                    // create the join entry implementing the return call
                    let now = System.DateTimeOffset.UtcNow
                    do! m.scheduler.joinStore.add callerJoinId
                            {
                                status = Join.Status.Waiting
                                childrenStatuses = [ calleeMetdata.guid, Join.Requested ] |> Map.ofList
                                whenAllSubscribers = [ m.scheduler.embed requestMetadata returnState ]
                                whenAnySubscribers = []
                                parent = None
                                created = now
                                modified = now
                            }

                    // spawn the callee's request
                    do! m.scheduler.spawn <| calleeRequestBuilder calleeMetdata

                    // The request is suspended and will be resumed when the callee completes
                    return ExecutionInstruction.Suspended

                
                /// Join on a forked transition specified by its fork Id
                | Transition.WhenAll (joinId, newState) ->
                    let! updatedJoinEntry =
                        m.scheduler.joinStore.update joinId
                            (fun driftedJoin ->
                                if allCompleted driftedJoin.childrenStatuses then
                                    driftedJoin
                                else
                                    // Subscribe to the 'WhenAll' event
                                    let subscriber = m.scheduler.embed requestMetadata newState
                                    { driftedJoin with whenAllSubscribers = subscriber::driftedJoin.whenAllSubscribers
                                                       modified = System.DateTimeOffset.UtcNow } )

                    if allCompleted updatedJoinEntry.childrenStatuses then
                        // The 'WhenAll' condition is already met
                        return! goto newState
                    else
                        return ExecutionInstruction.Suspended

                /// 'WhenAny' joining
                | Transition.WhenAny (joinId, newState) ->
                    let! updatedJoinEntry =
                        m.scheduler.joinStore.update joinId
                            (fun driftedJoin ->
                                if atleastOneCompleted driftedJoin.childrenStatuses then
                                    driftedJoin
                                else
                                  // Subscribe to the 'WhenAny' event
                                  let subscriber = m.scheduler.embed requestMetadata newState
                                  { driftedJoin with whenAnySubscribers = subscriber::driftedJoin.whenAnySubscribers
                                                     modified = System.DateTimeOffset.UtcNow } )

                    if atleastOneCompleted updatedJoinEntry.childrenStatuses then
                        // The 'WhenAny' condition is already met
                        return! goto newState
                    else
                        return ExecutionInstruction.Suspended
            }
        in
        runAt initialState

/// In-memory implementation of the storage structures required to execute an Agent
module InMemory =
    open Agent

    /// Implements a JoinEntry storage in-memory using the provided ConcurrentDictionary
    let newJoinStorageOf (storage:System.Collections.Concurrent.ConcurrentDictionary<JoinId, Join.Entry<'m>>) =
        { new Agent.Join.IStorage<'m> with
            member __.add joinId (joinEntry:Join.Entry<'m>) =
                async {
                    if not <| storage.TryAdd(joinId, joinEntry) then
                        failwithf "Add: Failed to add %A" (joinId, joinEntry)
                }

            member __.update joinId performEntryUpdate =
                lock(storage) (fun () ->
                    async {
                        match storage.TryGetValue(joinId) with
                        | true, entry ->
                            let newEntry = performEntryUpdate entry
                            storage.[joinId] <- newEntry
                            return newEntry

                        | false, _ -> return failwithf "Update: Failed to retrieve data with id: %A" joinId
                    }
                )

            member __.get joinId =
                async {
                    match storage.TryGetValue(joinId) with
                    | false, _ -> return failwithf "Get: There is no request with id : %A" joinId
                    | true, entry -> return entry
                }

        }

    /// Implements a JoinEntry storage in-memory using a ConcurrentDictionary
    let newJoinStorage<'m> () : Agent.Join.IStorage<'m> =
        let storage = System.Collections.Concurrent.ConcurrentDictionary<JoinId, Join.Entry<'m>>()
        newJoinStorageOf storage
