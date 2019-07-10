namespace Microsoft.FSharpLu.HttpCommunication.Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open System
open Microsoft.FSharpLu.HttpCommunication

[<AutoOpen>]
module CommunicationTests =

    let randomSeed = (new Random()).Next()
    let random = new Random(randomSeed)
    let testResponseText = "a response"

    let getOpenPort startPort endPort =
        let properties = System.Net.NetworkInformation.IPGlobalProperties.GetIPGlobalProperties()
        let usedPortsWithinRange =
            properties.GetActiveTcpListeners()
            |> Seq.map (fun p -> p.Port)
            |> Seq.filter (fun p -> p >= startPort && p <= endPort)
            |> Set.ofSeq
        let openPort = set [startPort .. endPort] - usedPortsWithinRange |> Seq.tryHead
        match openPort with
        | Some port -> port
        | None -> failwith (sprintf "Error getting an open port, all ports within range from %i to %i are busy. Try increasing the range." startPort endPort)

    let rec startListener cancellationTokenSource =
        try
            let serverUri = Uri (sprintf "http://localhost:%d" (getOpenPort 8980 9000))
            Server.listener serverUri cancellationTokenSource (fun req resp ->
                async {
                    printfn "Listener start on %s" serverUri.AbsoluteUri
                    let relativeUri = Server.getRelativeUri serverUri req

                    printfn "URI: %s" relativeUri
                    let statusCode =
                        match relativeUri with
                        | "SuccessTest" ->
                            async {
                                printfn "Running success test."
                                let messageText = Server.getRequestContent req

                                match messageText with
                                | Ok messageText ->
                                    printfn "Found message: %s" messageText
                                    return System.Net.HttpStatusCode.OK
                                | Error exn ->
                                    printfn "Error reading request content: %O" exn
                                    return System.Net.HttpStatusCode.BadRequest
                            } |> Async.RunSynchronously
                        | unknownUri ->
                            printfn "Unknown request type: %A" unknownUri
                            System.Net.HttpStatusCode.BadRequest

                    printfn "Returning code: %A" statusCode

                    let dataToReturn = if random.NextDouble() > 0.5 then None else (Some testResponseText)
                    Server.sendResponse resp statusCode dataToReturn
                }), serverUri
        with
        | :? System.Net.HttpListenerException as e ->
            printfn "An exception has occurred: %s. Retrying start of the listener." e.Message
            startListener cancellationTokenSource

    let sendRequest serverUri requestContent relativeUri =
        Client.sendRequest serverUri requestContent relativeUri

[<TestClass>]
type UtilitiesCommunicationTests() =

    static let cancellationTokenSource = new System.Threading.CancellationTokenSource()
    static let httpListener, serverUri = startListener cancellationTokenSource

    [<ClassInitialize>]
    static member init(context : TestContext) =
        printfn "Using random seed %d" randomSeed

    [<ClassCleanup>]
    static member cleanup() =
        (httpListener :> IDisposable).Dispose()

    [<TestMethod>]
    [<Description("Test 'ServerRequestSuccess' behavior")>]
    [<TestCategory("Utilities")>]
    member this.ServerRequestSuccess() =

        let response = sendRequest serverUri "hello" "SuccessTest"
        printfn "Received response text: %s" response
        if not (String.IsNullOrEmpty response || response.Equals(testResponseText)) then
            invalidOp (sprintf "test failed, response text %s does not match expected %s" response testResponseText)

    [<TestMethod>]
    [<Description("Test 'ServerRequestError' behavior")>]
    [<TestCategory("Utilities")>]
    member this.ServerRequestError() =
        let requestSucceeded =
            try
                sendRequest serverUri "hello" "NonExistentPath" |> ignore
                true
            with
                e ->
                    printfn "Got %s" e.Message
                    false
        if requestSucceeded then
            invalidOp "test failed, request should fail"


    [<TestMethod>]
    [<Description("Ping Google and Bing")>]
    [<TestCategory("Utilities")>]
    member this.InternetAccess() =
        let urlsToPing = ["http://www.google.com"; "http://www.bing.com"]
        let access = urlsToPing |> Utils.hasInternetAccess |> Async.Parallel |> Async.RunSynchronously
        if access |> Array.exists (fun x -> match x with Utils.InternetAccessResult.Access _ -> false | _ -> true) then
            List.zip urlsToPing (List.ofArray access)
            |> invalidOp "Failed to ping at least one URL: %A"

