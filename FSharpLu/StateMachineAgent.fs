/// State machine and agent
namespace Microsoft.FSharpLu.StateMachineAgent

open FSharp.Control

/// ID used to identify joining points (either an agent request or a fork)
type JoinId =
    {
        /// Unique GUID identifying the join entry
        guid : System.Guid
        /// Timestamp when the join was created
        timestamp : System.DateTimeOffset
    }

/// A state machine transition with
/// state type 's and return type 't,
/// and a set of callable state machines represented by type 'm
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

/// A state machine with simple transitions (no support for Forking, Coroutines nor out-of-process pause/resume)
module StateMachine =

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

    /// Interface defining operations supported by an agent.
    /// The consumer of the API is responsible for implementing the spawning logic
    /// to execute a new agent in the specified state.
    /// It is up to the API user to decide when to execute the spawn agent
    /// (e.g. immediately in a background thread, queued up for later on some
    /// queuing device...)
    /// The type 'm represents a spawning request. It embeds the desired state of the agent to spawn, and the associated request metadata.
    ///
    /// The API consumer provides the embedding function to create a request of type 'm
    /// from a state of type 's and associated metadata (see `Agent.embedState` below).
    [<NoEquality;NoComparison>]
    type Operations<'m> =
        {
            /// Request spawning of a new state machine with the specified state and request metadata
            spawn : 'm -> Async<unit>
            /// Request spawning of a new state machine with the specified state and request metadata
            /// in the specified amount of time
            spawnIn : 'm -> System.TimeSpan -> Async<unit>
        }

    /// Possible actions (to be implemented by the API user) that can be performed on a request
    /// each time a state machine transition is executed.
    /// This gets returned by the `Agent.execute` function when the state machine
    /// returns (`Return` transition) or goes to sleep (out-of-process `Sleep` transition).
    /// The user of the API is responsible for implementing those
    /// action as part of its request scheduling system (each time the `Agent.execute` function is called).
    type RequestAction<'m> =
        /// The request has been processed and can be deleted from the request scheduling system
        | Delete
        /// The request processing is being paused for the specified amount of time,
        /// once it the time out expires, the scheduler is responsible for calling `Agent.execute` to resume processing
        /// of the request at the same state where it left off.
        | PostponeFor of System.TimeSpan
        /// Postpone processing of the request for the specified amount of time.
        /// once the timeout expires, the scheduler is responsible for calling `Agent.execute` to resume processing
        /// at the specified (embedded) state 'm
        | PostponeAndReplace of 'm * System.TimeSpan

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

        /// Subscriptions to a join condition: encoded as a list of request messages to be posted when the join condition is met
        type Subscriptions<'m> = 'm list

        /// A persistable entry storing information about a join
        type Entry<'m> =
            {
                /// Subscriber to notifyu when all children completes
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

        /// Storage provider interface used to persist state of
        /// joining points (either an agent request or a fork point)
        /// Requirement: underlying READ/WRITE opeations need to be atomic.
        type StorageInterface<'m> =
            {
                /// An a new entry to track a request
                add : JoinId -> Entry<'m> -> Async<unit>
                /// Update the state of a fork
                update : JoinId -> (Entry<'m> -> Entry<'m>) -> Async<Entry<'m>>
                /// Get the state of a fork
                get : JoinId -> Async<Entry<'m>>
            }

    /// An agent state machine with return type 't and underlying state type 's
    /// embeddable into request type 'm.
    ///
    /// An agent instance must implement `Agent.embedState`: a function to embed the agent state and associated `RequestMetadata` into a type 'm.
    ///
    /// In order to support:
    /// __pausing/resuming__ (and out-of-process asynchronous sleep). The agent must implement state persistence on an external device
    /// (e.g. Azure Queue, Azure Service Bus...).
    /// This is provided by function `Agent.persist` which gets called to persist the machine state and associated metadata in some external storage.
    ///
    /// __out-of-process asynchronous sleep__ the function calling .execute must handle the actions `RequestAction.PostponeFor` and `RequestAction.PostponeAndReplace`
    /// For instance by posting the serialize state 'm onto some queue for later processing.
    ///
    /// __corountines__ and the `CoReturn` transition, the agent must handle spawning new agents
    /// by implementing `Agent.operations.spawn` and `Agent.operations.spawnIn`.
    ///
    /// __forking__ (Fork, WhenAny, WhenAll), the agent must provides an external storage interface `Agent.storage`
    /// of type `JoinStorage` implementing atomic add/update storage functions
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

            /// Maximum expected time to process each transition of the state machine
            maximumExpectedStateTransitionTime : System.TimeSpan

            /// Amount of time that is short enough to implement asynchronous sleep 'in-process' rather than out-of-process
            maximumInprocessSleep : System.TimeSpan

            /// A function embedding a given state machine internal state 's
            /// and associated metadata into a custom type 'm
            embedState : RequestMetadata -> 's -> 'm

            /// Persist a state-request 'm on some external storage device
            /// and schedule processing of the request in the specified amount of time
            persist : 'm -> System.TimeSpan -> Async<unit>

            /// External operations available to the agent
            operations : Operations<'m>

            /// Provides an implementation of join storage (e.g. using Azure Table)
            storage : Join.StorageInterface<'m>
        }

    /// Create a new request attached to the specified parent JoinId
    let private createRequestWithParent (m:Agent<'s, 't, 'm>) parent =
        async {
            let requestId =
                {
                    guid = System.Guid.NewGuid()
                    timestamp = System.DateTimeOffset.UtcNow
               }

            // Create a join entry for the request
            do! m.storage.add
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
    let public createRequest (m:Agent<'s, 't, 'm>) =
        createRequestWithParent m None

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
                 m.storage.update
                    metadata
                    (fun u -> { u with status = Join.Status.Completed
                                       modified = System.DateTimeOffset.UtcNow } )

            match updatedEntry.parent with
            | None -> return ()
            | Some joinId ->
                let mutable whenAnySubscriptions = []

                // Update child status under the request's parent entry
                let! joinEntry =
                    m.storage.update joinId
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
                for subscriber in whenAnySubscriptions do
                    do! m.operations.spawn subscriber

                // Honor the "WhenAll" subscriptions
                if joinEntry.status = Join.Status.Completed then
                    // We are necessarily the last one in the fork here since we held on to one of the fork's child
                    // while trying to update the fork data

                    // Signal the fork's subscribers
                    for subscriber in joinEntry.whenAllSubscribers do
                        do! m.operations.spawn subscriber
        }

    /// Advance execution of an agent state machine.
    /// The state gets persisted on every transition to allow 
    /// resuming the execution in case of failure.
    let public executeWithResult (initialState:'s) requestMetadata (m:Agent<'s, 't, 'm>) : Async<RequestAction<'s> * 't option> =

        /// Keep-alive function called to notify external device (e.g. Azure queue)
        /// that more time is needed to finish executing the state machine
        let notifyMoreTimeIsNeeded newState delay =
            // Serialize and persist progress in external device (e.g. Azure queue message)
            // so that we can resume where we left off if the process crashes, is killed, upgraded...
            // Specify a timeout after which the persisted state can be deserialized if not already deleted.
            let timeoutPeriod = delay + m.maximumExpectedStateTransitionTime
            m.persist (m.embedState requestMetadata newState) timeoutPeriod

        let rec sleepAndGoto delay (currentState:'s) (nextState:'s) =
            async {
                if delay < m.maximumInprocessSleep then
                    m.logger (sprintf "Agent '%s' sleeping for %O in proc" m.title delay) m.tags
                    do! notifyMoreTimeIsNeeded currentState delay
                    // Short-enough to wait asynchronously in-process
                    do! Async.Sleep(delay.TotalMilliseconds |> int)
                    return! goto nextState
                else
                    m.logger (sprintf "Agent '%s' sleeping for %O out of proc" m.title delay) m.tags
                    // For longer waits we wait asynchronously out-of-process using the external storage device
                    // and postpone the request until later
                    return (PostponeAndReplace (nextState, delay)), None
            }

        and goto (currentState:'s) =
            async {
                let! operation = m.transition currentState
                match operation with
                | Transition.Sleep delay ->
                    return! sleepAndGoto delay currentState currentState

                | Transition.SleepAndGoto (delay, nextState) ->
                    return! sleepAndGoto delay currentState nextState

                | Transition.Return result ->
                    m.logger (sprintf "Agent '%s' reached final state and returned %A" m.title result) m.tags
                    do! markRequestAsCompleted m requestMetadata
                    return RequestAction.Delete, Some result

                | Transition.Coreturn requestCallee ->
                    m.logger (sprintf "Agent '%s' coreturning to another state machine" m.title) m.tags
                    do! m.operations.spawn requestCallee
                    return RequestAction.Delete, None

                | Transition.Goto newState ->
                    m.logger (sprintf "Agent '%s' at state %O" m.title newState) m.tags
                    do! notifyMoreTimeIsNeeded newState System.TimeSpan.Zero
                    return! goto newState

                | Transition.ForkAndGoto (spawnedStates, newStateFromJoinId) ->
                    if List.isEmpty spawnedStates then
                        return raise (System.NotSupportedException("ForkAndGoto does not accept empty spawned states list"))

                    // spawn children state machines
                    m.logger (sprintf "Agent '%s' forking into %d state machines" m.title spawnedStates.Length) m.tags

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
                                    let! metadata = createRequestWithParent m (Some joinId)
                                    return Map.add metadata.guid (metadata, spawnState) spawnChildrenSoFar
                                }
                            )
                            Map.empty

                    // create the join entry
                    let now = System.DateTimeOffset.UtcNow
                    do! m.storage.add joinId
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
                                    let spawningRequest = m.embedState childMetadata spawnState
                                    do! m.operations.spawn spawningRequest
                                }
                            )

                    return! goto (newStateFromJoinId joinId)

                /// Join on a forked transition specified by its fork Id
                | Transition.WhenAll (joinId, newState) ->
                    let! updatedJoinEntry =
                        m.storage.update joinId
                            (fun driftedJoin ->
                                if allCompleted driftedJoin.childrenStatuses then
                                    driftedJoin
                                else
                                    // Subscribe to the 'WhenAll' event
                                    let subscriber = m.embedState requestMetadata newState
                                    { driftedJoin with whenAllSubscribers = subscriber::driftedJoin.whenAllSubscribers
                                                       modified = System.DateTimeOffset.UtcNow } )

                    if allCompleted updatedJoinEntry.childrenStatuses then
                        // The 'WhenAll' condition is already met
                        return! goto newState
                    else
                        return RequestAction.Delete, None

                /// 'WhenAny' joining
                | Transition.WhenAny (joinId, newState) ->
                    let! updatedJoinEntry =
                        m.storage.update joinId
                            (fun driftedJoin ->
                                if atleastOneCompleted driftedJoin.childrenStatuses then
                                    driftedJoin
                                else
                                  // Subscribe to the 'WhenAny' event
                                  let subscriber = m.embedState requestMetadata newState
                                  { driftedJoin with whenAnySubscribers = subscriber::driftedJoin.whenAnySubscribers
                                                     modified = System.DateTimeOffset.UtcNow } )

                    if atleastOneCompleted updatedJoinEntry.childrenStatuses then
                        // The 'WhenAny' condition is already met
                        return! goto newState
                    else
                        return RequestAction.Delete, None
            }
        in
        goto initialState

    /// Advance execution of an agent state machine.
    /// Same as executeWithResult but return only the action in serialized format
    /// and throw away the state machine result.
    let public execute initialState requestMetadata (m:Agent<'s, 't, 'm>) =
        async {
            let! action, finalResult = executeWithResult initialState requestMetadata m
            let serializedAction =
                match action with
                | RequestAction.Delete ->
                    RequestAction.Delete
                | RequestAction.PostponeFor t ->
                    RequestAction.PostponeFor t
                | RequestAction.PostponeAndReplace (s, d) ->
                    RequestAction.PostponeAndReplace (m.embedState requestMetadata s, d)
            return serializedAction
        }
