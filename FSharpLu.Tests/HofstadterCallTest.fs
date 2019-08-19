/// Hofstadter Female and Male sequences test
/// See https://en.wikipedia.org/wiki/Hofstadter_sequence#Hofstadter_Female_and_Male_sequences
module Microsoft.FSharpLu.Actor.Call.Test

open Xunit
open Microsoft.FSharpLu.Logging
open Microsoft.FSharpLu.Actor.StateMachine
open Microsoft.FSharpLu.Actor.StateMachine.Agent
open Microsoft.FSharpLu.Actor.QueueScheduler
open Microsoft.FSharpLu.Actor.ServiceRequests
open Microsoft.FSharpLu.Actor

open System.Collections.Generic

type States =
    | Start
    | FirstCallReturn
    | SecondCallReturn
    | ReturnResult of int

type Gender =
 | Male 
 | Female
 with
 member x.Opposite =
    match x with
    | Male -> Female
    | Female -> Male 

type Message  =
    | Male of StatelessRequest<int>
    | Female of StatelessRequest<int>
    | Calculate of StatefulRequest<Gender * int, States>
    | Shutdown

type Header = unit
type CustomContext = unit

/// Store the results produced by calling the male and female agents
let femaleAndMaleSequence =
    [
        Gender.Female, System.Collections.Concurrent.ConcurrentDictionary<int, int>([KeyValuePair(0,1)]) // F(0) = 1
        Gender.Male, System.Collections.Concurrent.ConcurrentDictionary<int, int>([KeyValuePair(0,0)]) // M(0) = 0
    ] |> dict

/// Expected values of the Hofstadter sequence
let expected =
    [
        Gender.Female, [ 1; 1; 2; 2; 3; 3; 4; 5; 5; 6; 6; 7; 8; 8; 9; 9; 10; 11; 11; 12; 13 ] // female sequence https://oeis.org/A005378
        Gender.Male, [ 0; 0; 1; 2; 2; 3; 4; 4; 5; 6; 6; 7; 7; 8; 9; 9; 10; 11; 11; 12; 12 ] // male sequence https://oeis.org/A005379
    ] |> dict

/// Transition function for state machine agent calculating the
/// Hofstadter Female and Male sequences
let transition<'QueueMessage> (operations:Operations<'QueueMessage, Header, Message>) (gender:Gender, n:int) =
    function
    | States.Start -> async {
        Trace.info "%A agent starting" gender
        match femaleAndMaleSequence.[gender].TryGetValue(n) with
        | true, v ->
            return Transition.Goto (States.ReturnResult v)
        | false, _ ->
            match n with
            | 0 ->
                return Transition.Goto (States.ReturnResult femaleAndMaleSequence.[gender].[0])
            | n ->
                // Assuming gender is Female then we have 
                // F(n) = n - M(F(n-1)), n>0  (resp. M(n) = n - M(F(n-1)) if gender is Male)
                // so we first calculated F(n-1) (resp. M(n-1))
                return Transition.Call
                        ((fun m -> {
                            metadata = Some m
                            header = ()
                            calleeReturnResult = None
                            request = Calculate {
                                state = States.Start
                                input = gender, n-1
                            }
                            }),
                            States.FirstCallReturn)
       }

    | States.FirstCallReturn -> async {
        let samegender_Of_n_minus_1 = operations.ReturnResult<int>()
        return Transition.Call
                            ((fun m ->
                                { 
                                    metadata = Some m
                                    header = ()
                                    calleeReturnResult = None
                                    request = Calculate { 
                                        state = States.Start
                                        input = gender.Opposite, samegender_Of_n_minus_1
                                    }
                                }),
                            States.SecondCallReturn)
      }

    | States.SecondCallReturn -> async {
        let oppositeGender_Of_sameGender_Of_n_minus_1 = operations.ReturnResult<int>()

        // F(n) = n - M(F(N-1)) and M(n) = n - M(F(N-1)) for n>0
        let r = n - oppositeGender_Of_sameGender_Of_n_minus_1 
        return Transition.Goto <| ReturnResult r
      }

    | States.ReturnResult result -> async {
        Trace.info "%A agent returning %d for n=%d" gender result n
        let _ = femaleAndMaleSequence.[gender].AddOrUpdate(n, result, (fun k v -> printfn "Value already cached: %A(%d) = %d" gender k v; v))
        
        if femaleAndMaleSequence.[Gender.Female].Count = expected.[Gender.Female].Length then
          do! operations.spawnNewRequest Message.Shutdown
        else
            Trace.info "female results: %d/%d - male results: %d/%d"
                femaleAndMaleSequence.[Gender.Female].Count
                expected.[Gender.Female].Length
                femaleAndMaleSequence.[Gender.Male].Count
                expected.[Gender.Male].Length
                    
        return Transition.Return result
      }

let startTest (queue:Map<Gender, IQueueingAPI<_, Envelope<Header,Message>>>) =
    async {
        let n = expected.[Gender.Female].Length-1
        do! queue.[Gender.Female].post { 
            metadata = None
            header = ()
            calleeReturnResult = None
            request = Message.Female { input = n } }
    }

let endTest () = 
    async {
        let validate gender =
            expected.[gender]
            |> Seq.iteri (fun i expectedValue ->
                Assert.Equal(femaleAndMaleSequence.[gender].[i], expectedValue))

        validate Gender.Female
    }
       
[<Fact>]
let ``Hofstadter sequence - mutually recursive calls`` () =
    async {

        let agentQueues = 
            [ yield Gender.Female, InMemoryQueue.InMemoryQueueProcessor<int,Envelope<Header, Message>>("Hofstadter-female", 0) :> IQueueingAPI<_,_>
              yield Gender.Male, InMemoryQueue.InMemoryQueueProcessor<int,Envelope<Header, Message>>("Hofstadter-male", 1) :> IQueueingAPI<_,_>
            ] |> Map.ofSeq
                    
        let shutdownSource = new System.Threading.CancellationTokenSource()
        let schedulerFactory = InMemorySchedulerFactory()
        let joinStore = InMemory.newJoinStorage()

        do! startTest agentQueues
        try
            do! QueueScheduler.processingLoopMultipleQueues<Gender, Envelope<Header, Message>, _, System.Guid>
                    (Microsoft.FSharpLu.Logging.Interfaces.fromTraceTag<System.Diagnostics.TagsTracer>)
                    {
                        SleepDurationWhenAllQueuesAreEmpty = System.TimeSpan.FromMilliseconds(0.0)
                        HeartBeatIntervals = System.TimeSpan.FromSeconds(1.0)
                        ConcurrentRequestWorkers = 100
                        WorkerReplacementTimeout = System.TimeSpan.FromHours(1.0)
                    }
                    [ for g in [Gender.Female; Gender.Male] do
                        yield {
                            queueId = g
                            handler = fun (c:QueueingContext<_, Header, Message, CustomContext>)
                                            (k:IContinuation<Envelope<Header, Message>>)
                                            envelope ->
                                            match envelope.request with
                                            | Message.Male r ->
                                                k.k (async.Return <| Coreturn
                                                            {   header = envelope.header
                                                                metadata = envelope.metadata
                                                                calleeReturnResult = None
                                                                request = Message.Calculate { 
                                                                    input = Gender.Male, r.input
                                                                    state = States.Start }
                                                            })

                                            | Message.Female r ->
                                                k.k (async.Return <| Coreturn
                                                            {   header = envelope.header
                                                                metadata = envelope.metadata
                                                                calleeReturnResult = None
                                                                request = Message.Calculate {
                                                                    input = Gender.Female, r.input
                                                                    state = States.Start }
                                                            })

                                            | Message.Calculate r ->
                                                let gender, _ = r.input
                                                k.k (run (sprintf "%A" gender) [] (Message.Calculate) transition schedulerFactory c envelope)

                                            | Message.Shutdown ->
                                                k.k (async {
                                                    Trace.info "Shutdown request received"
                                                    shutdownSource.Cancel()
                                                    Trace.info "Token cancelled"
                                                    return RequestStatus.Completed None
                                                })
                            maxProcessTime = System.TimeSpan.FromSeconds(5.0)
                            messageBatchSize = 100
                        }
                    ]
                    (fun queueId -> Map.find queueId agentQueues)
                    (fun queue message -> {
                        queue = queue
                        queuedMessage = message
                        joinStore = joinStore
                        customContext = ()
                    })
                    ignore // no heartbeat
                    shutdownSource.Token
                    (fun _ -> []) // no tags
                    {new IOutcomeLogger<_, _> with member __.log _ = ()} // no logger
        with
        | ProcessingLoopCancelled ->
            Trace.warning "Processing loop terminated by Azure."
        

        do! endTest ()
    }