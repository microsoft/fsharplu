/// State machine and agent
namespace Microsoft.FSharpLu.StateMachineAgent

module StateMachine =

    /// State machine transition with state type 's
    /// return type 't,
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
        /// Suspend the state machine and spawn a new state machine 'm
        | Coreturn of 'm

    /// Execute the state machine and call transitionCallback on each state transition.
    /// The call-back function can be used to report progress, send a heartbeat
    /// or persist the state (e.g. on an Azure Queue) to allow resuming processing
    /// at a later stage in case of process failure (crash or Azure migration).
    //
    /// This version of state machine execution implements 'sleep' using in-process asynchronous sleep (Async.Sleep).
    /// See Agent module below for a version implementing out-of-process asynchronous sleep.
    let rec execute transitionFunction initialState transitionCallback =
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

                | Coreturn requestCallee ->
                    return failwith "StateMachine.execute does not support Coreturn since only one state machine is involved."

                | Goto newState ->
                    do! transitionCallback newState
                    return! goto newState
            }

        goto initialState


/// Agent to process requests using a state machine.
/// The agent can spawn requests to an external queuing device such as Azure Queue;
/// and those requests can themselves be processed by another agent (state machine).
module Agent =

    /// Defines effectful operations that can be performed by the agent
    [<NoEquality;NoComparison>]
    type Operations<'m> =
        {
            /// Post a request message of type 'm onto the request queue
            post : 'm -> Async<unit>

            /// After the specified amount of time, post a request message of type 'm onto the request queue
            postIn : 'm -> System.TimeSpan -> Async<unit>
        }

    /// Define action to be taken in response to a processed request
    type RequestAction<'m> =
        /// Delete the request from the request queue
        | Delete
        /// Keep the request on the queue, and make it invisible for the specified amount of time
        /// once it the visibility time out expires, another worker will pick up the request
        | PostponeFor of System.TimeSpan
        /// Replace the request with a new request and postpone its processing for the specified amount of time
        | PostponeAndReplace of 'm * System.TimeSpan

    /// An agent with state machine of state type 's, serialized as type 'm, and return type 't.
    /// The agent relies on an external device (e.g. Azure Queue, Azure Service Bus...) to
    ///   1. persist state of the state machine;
    ///   2. post and queue up additional requests (that will spwan more agents)
    ///   3. sleep asynchronously out-of-process
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
            transition : Operations<'m> -> 's -> Async<StateMachine.Transition<'s, 't, 'm>>

            /// Maximum expected time to process each transition of the state machine
            maximumExpectedStateTransitionTime : System.TimeSpan

            /// Amount of time that is short enough to implement asynchronous sleep 'in-process' rather than out-of-process
            maximumInprocessSleep : System.TimeSpan

            /// Embed the given state 's into a serializable request message of type 'm
            serializeState : 's -> 'm

            /// Persist a serialized state and schedule deserialization in the specified amount of time
            persist : 'm -> System.TimeSpan -> Async<unit>

            /// External effectful operations available to the agent
            operations : Operations<'m>
        }

    /// Execute a state machine using out-of-process asynchronous sleep
    /// based on the external serialization and scheduling device (e.g. Azure Queue)
    /// The state gets persisted on every transition (after a Goto or SleepAndGoto)
    let executeWithResult (initialState:'s) (m:Agent<'s, 't, 'm>) : Async<RequestAction<'s> * 't option> =

        /// Keep-alive function called to notify external device (e.g. Azure queue)
        /// that more time is needed to finish executing the state machine
        let notifyMoreTimeIsNeeded newState delay =
            // Serialize and persist progress in external device (e.g. Azure queue message)
            // so that we can resume where we left off if the process crashes, is killed, upgraded...
            // Specify a timeout after which the persisted state can be deserialized if not already deleted.
            let timeoutPeriod = delay + m.maximumExpectedStateTransitionTime
            m.persist (m.serializeState newState) timeoutPeriod

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
                let! operation = m.transition m.operations currentState
                match operation with
                | StateMachine.Transition.Sleep delay ->
                    return! sleepAndGoto delay currentState currentState

                | StateMachine.Transition.SleepAndGoto (delay, nextState) ->
                    return! sleepAndGoto delay currentState nextState

                | StateMachine.Transition.Return result ->
                    m.logger (sprintf "Agent '%s' reached final state and returned %A" m.title result) m.tags
                    return RequestAction.Delete, Some result

                | StateMachine.Transition.Coreturn requestCallee ->
                    m.logger (sprintf "Agent '%s' coreturning to another state machine" m.title) m.tags
                    do! m.operations.post requestCallee
                    return RequestAction.Delete, None

                | StateMachine.Transition.Goto newState ->
                    m.logger (sprintf "Agent '%s' at state %O" m.title newState) m.tags
                    do! notifyMoreTimeIsNeeded newState System.TimeSpan.Zero
                    return! goto newState
            }
        in
        goto initialState

    /// Same as executeWithResult but return only the action in serialized format
    /// and throw away the returned state machine result
    let execute initialState m =
        async {
            let! action, finalResult = executeWithResult initialState m
            let serializedAction =
                match action with
                | RequestAction.Delete -> RequestAction.Delete
                | RequestAction.PostponeFor d -> RequestAction.PostponeFor d
                | RequestAction.PostponeAndReplace (s,d) -> RequestAction.PostponeAndReplace (m.serializeState s, d)
            return serializedAction
        }