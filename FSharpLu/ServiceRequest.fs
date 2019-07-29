/// QueueProcessing based on state machine agent
module Microsoft.FSharpLu.Actor.ServiceRequests

open FSharp.Control
open FSharp.Reflection

/// A stateless request with header of type 'Header
/// and input of type 'Input
type StatelessRequest<'Input> =
    {
        input : 'Input
    }

/// A stateful request with header of type 'Header
/// input of type 'Input and state 'State
type StatefulRequest<'Input, 'State> =
    {
        input : 'Input
        state : 'State
    }

/// Implementing dynamic casting of a service request type into StatefulRequest<_,_>
/// using precomputeed reflective operations on type 'Request
type CachedReflection<'Request> () =
    static let caseInfo =
        FSharpType.GetUnionCases(typeof< 'Request>)

    static let caseConstructors =
        caseInfo
        |> Array.map FSharpValue.PreComputeUnionConstructor

    static let caseInfoReaders =
        caseInfo
        |> Array.map FSharpValue.PreComputeUnionReader

    static let readCaseTag =
        FSharpValue.PreComputeUnionTagReader(typeof< 'Request>)

    /// Cast a service request as StatefulRequest<_,_>
    static member tryCastAsStatefulRequest<'Input, 'State> (r: 'Request) =
        let tagNumber = readCaseTag r
        let fields = caseInfoReaders.[tagNumber] (r:>obj)
        let firstField = fields.[0]
        let isStatefulRequest = firstField.GetType().GetGenericTypeDefinition() = typeof<StatefulRequest<'Input,'State>>.GetGenericTypeDefinition()
        if isStatefulRequest then
            Some <| (firstField :?> StatefulRequest<'Input,'State>)
        else
            None

    static member castAsStatefulRequest<'Input, 'State> (r:'Request) =
        match CachedReflection<'Request>.tryCastAsStatefulRequest<'Input, 'State> r with
        | Some v -> v
        | None -> failwith "Invalid request type: expecting a service request of type StatefulRequest<_,_,_>"


    /// Return the Discriminated Union constructor that was used to create the specified request
    static member getStatefulRequestConstructor<'Input, 'State> (r: 'Request) =
        let tagNumber = readCaseTag r
        let constructor = caseConstructors.[tagNumber]
        let cons (statefulRequest:StatefulRequest<'Input,'State>) : 'Request =
            constructor [| statefulRequest:>obj |] :?> 'Request
        cons

    /// Extract the state of a request using type reflection (not ideal but best we can do given lack of higher-order type polymorphism in F#)
    static member tryExtractState<'Input, 'State, 'Request> (r:'Request) =
        CachedReflection< 'Request>.tryCastAsStatefulRequest<'Input, 'State> r
        |> Option.map (fun y -> y.state)

module TestExtract =

    /// Example of service request type
    type ServiceRequests =
    | Request1 of StatelessRequest<int>
    | Request2 of StatefulRequest<unit, string>

    let aRequest = ServiceRequests.Request2 { input = (); state = "foo"}
    let anotherRequest = ServiceRequests.Request1 { StatelessRequest.input = 1 }
    let x = CachedReflection<_>.tryExtractState<unit, string, ServiceRequests> aRequest
    let shouldThrow = CachedReflection<_>.tryExtractState<unit, string, ServiceRequests> anotherRequest


open Microsoft.FSharpLu.Actor.StateMachine
open Microsoft.FSharpLu.Actor.StateMachine.Agent
open Microsoft.FSharpLu.Actor.QueueScheduler

/// Each request in wrapped inside an envelope prior to be pushed on a queue
/// Requirement: 'Request must be either of type
///  StatelessRequest<'Input>  or StatefulRequest<'Input, 'State>
/// F# weak type system does not allow us to statically enforce this restriction,
/// so unfortunately we can only validate this requirement dynamically via reflection and casting...
type Envelope<'Header, 'Request> =
    {
        metadata : RequestMetadata option
        header : 'Header
        request : 'Request // Must be StatelessRequest<'Input> or StatefulRequest<'Input, 'State> for some 'Input and  'State
    } 
    with  
    /// Create a new envelop with same request but updated state
    /// Requirement: 'Request must be of type  StatefulRequest<'Input, 'State>
    member envelope.updateState<'Input, 'State> (newState:'State) =
        let statefulRequest = CachedReflection< 'Request>.castAsStatefulRequest<'Input, 'State> envelope.request
        let cons = CachedReflection<'Request>.getStatefulRequestConstructor<'Input, 'State> envelope.request
        {
            envelope with request = cons { statefulRequest with state = newState }
        }

    /// Create a new envelop with same request but updated state and metadata
    /// Requirement: 'Request must be of type StatefulRequest<'Input, 'State>
    member envelope.updateMetadataAndState<'Input, 'State> (metadata:RequestMetadata) (newState:'State) =
        let statefulRequest = CachedReflection<'Request>.castAsStatefulRequest<'Input, 'State> envelope.request
        let cons = CachedReflection<'Request>.getStatefulRequestConstructor<'Input, 'State> envelope.request
        {
            envelope with
                request = cons { statefulRequest with state = newState }
                metadata = Some metadata
        }

/// Operations that can be performed by a request handler
type Operations<'QueueMessage, 'Header, 'Request>
    (
        /// The queue scheduling system
        queue:QueueingAPI<'QueueMessage,Envelope<'Header,'Request>>,

        /// The envelope of the current request being processed by the handler
        envelope:Envelope<'Header,'Request>
    ) =
    /// Spawn a new request with a new request metadata but preserving the same header
    member __.spawnNewRequest (r: 'Request) =
        queue.post { metadata = None; request = r; header = envelope.header }
    
    /// Spawn a request with the specific request metadata and preserving the same header
    member __.spawnRequest metadata (r: 'Request) =
        queue.post { metadata = metadata; request = r; header = envelope.header }

    /// Spawn a new request already wrapped in an envelope
    member __.spawnRequestEnvelop (e:Envelope<'Header,'Request>) =
        queue.post e

/// Context parameters passed to every request handler
/// - 'QueueMessage is the underlying queueing system message type
/// - 'Header and 'Request and 'CustomContext are types provided by the API consumer.
///    The first two define how requests get encoded, while 'CustomContext defines customer fields that gets passed to every request handler
[<NoEquality;NoComparison>]
type QueueingContext<'QueueMessage, 'Header, 'Request, 'CustomContext> =
    {
        /// the underlying queing system.
        queue : QueueingAPI<'QueueMessage, Envelope<'Header, 'Request>>
        /// the message that has just been dequeued and need to be processed
        queuedMessage : 'QueueMessage
        /// the storage system used to record join/fork points
        joinStore : Join.IStorage
        /// Custom context defined by the QueueProcessing API user
        customContext : 'CustomContext
    }

/// A scheduler factory instanciating a scheduling API for any possible state type 'State
type ISchedulerFactory<'QueueMessage, 'Header, 'Request, 'CustomContext> =
    abstract create<'Input, 'State> : 
                        QueueingContext<'QueueMessage, 'Header, 'Request, 'CustomContext>
                    -> Envelope<'Header, 'Request>
                    -> Scheduler<'State, Envelope<'Header, 'Request>>

/// A scheduler factory based on in-memory queueing system
type InMemorySchedulerFactory<'QueueMessage, 'Header, 'Request, 'CustomContext> () =
    interface ISchedulerFactory<'QueueMessage, 'Header, 'Request, 'CustomContext> with
        member __.create<'Input, 'State> context (envelope:Envelope<'Header, 'Request>) : Scheduler<'State, Envelope<'Header, 'Request>>=
            {
                joinStore = context.joinStore
                onInProcessSleep = fun delay -> async.Return ()
                onGoto = fun state -> async.Return ()
                embed = fun metadata state -> envelope.updateMetadataAndState<'Input, 'State> metadata state
                spawn = fun request -> context.queue.post request
            }

/// Type of a request handler
type Handler<'QueueMessage, 'Header, 'Request, 'CustomContext, 'Result>
        = ISchedulerFactory<'QueueMessage, 'Header, 'Request, 'CustomContext> 
            -> QueueingContext<'QueueMessage, 'Header, 'Request, 'CustomContext> 
            -> Envelope<'Header,'Request> 
            -> Async<RequestStatus<Envelope<'Header,'Request>,'Result>> 

/// Process a service request by executing a state machine agent via Agent.execute.
/// The function processes a request wrapped in an envelope via a state machine agent
/// built  from the provided transition function.
///
/// This helper function can be used to define state-machine handlers for each service requests.
/// 
/// Requirement 'Request must be a type of the form:
///   type 'Request = ... | SomeRequest of StatefulRequest<'Input, 'State> | ...
/// for any possible value `SomeRequest { input = ...; state = ...}`
/// of `envelope.Request`.
let inline run
        title
        tags
        /// Ghost parameter: helps infer that the request type
        /// is of kind `StatefulRequest<'Input, 'State>`
        /// in handlers defined with `run`.
        (requestConstructor: StatefulRequest<'Input, 'State> -> 'Request) 
        (transition: Operations<_,_,_> -> 'Input -> 'State -> Async<Transition<'State, 'Result, Envelope<'Header, 'Request>>>)
        : Handler<'QueueMessage, 'Header, 'Request, 'CustomContext, 'Result>
    = fun schedulerFactory context envelope ->
    async {
        let statefulRequest = CachedReflection<'Request>.castAsStatefulRequest<'Input, 'State> envelope.request
        let operations = Operations<_,_,_>(context.queue, envelope)
        let agent :Agent<'State, 'Result, Envelope<'Header, 'Request>>= {
            Agent.title = title
            logger = Microsoft.FSharpLu.Logging.TraceTags.info
            tags = tags
            transition = transition operations statefulRequest.input
            maximumInprocessSleep = System.TimeSpan.FromMinutes(5.0)
            scheduler = schedulerFactory.create<'Input, 'State> context envelope
        }

        let! metadata =
            match envelope.metadata with
            | None ->Agent.createRequest context.joinStore
            | Some m -> async.Return m
        
        let! r = Agent.executeWithResult statefulRequest.state metadata agent
        return
            match r with
            | ExecutionInstruction.Completed result ->
                RequestStatus.Completed result
            | ExecutionInstruction.Coreturn request ->
                RequestStatus.Coreturn request
            | ExecutionInstruction.SleepAndResume sleepTime ->
                RequestStatus.SleepAndResume sleepTime
            | ExecutionInstruction.SleepAndResumeAt (sleepTime, s) ->
                RequestStatus.SleepAndResumeWith (sleepTime, envelope.updateState<'Input, 'State> s)
    }