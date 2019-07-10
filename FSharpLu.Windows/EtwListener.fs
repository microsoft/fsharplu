// Copyright (c) Microsoft Corporation.
module Microsoft.FSharpLu.Windows.EtwListener

open System
open Microsoft.Diagnostics.Tracing
open Microsoft.FSharpLu.Logging
open Microsoft.Diagnostics.Tracing.Session

type Handlers = Map<System.Guid, TraceEvent -> unit>

/// Start a separate thread that listens to ETW events emitted by the specified provider
/// Returns the trace event session handler which can be used to stop the thread (with session.Stop)
let listen sessionName (handlers:Handlers) =
    let session = new TraceEventSession(sessionName, null, StopOnDispose = true)  // the null second parameter means 'real time session'
    let processingLoop =
        async {
            use session = session
            #if WIN8
            // prepare to read from the session, connect the ETWTraceEventSource to the session
            use source = new ETWTraceEventSource(sessionName, TraceEventSourceType.Session)

            // Hook up the parser that knows about Any EventSources registered with windows.
            let registeredParser = Parsers.KernelTraceEventParser(source)
            registeredParser.add_ProcessStart(
                new Action<Kernel.ProcessTraceData>(
                    fun (data:Kernel.ProcessTraceData) ->
                        //Trace.info "GOT EVENT: %O" data
                        Trace.info "%s" <| System.String.Format("{0:HH:mm:ss.fff}: {1,-12}: {2} ID: {3} ParentID: {4}", data.TimeStamp, data.TaskName, data.ImageFileName, data.ProcessID, data.ParentID)
                        ))

            registeredParser.add_ProcessStop(
                new Action<Kernel.ProcessTraceData>(
                    fun (data:Kernel.ProcessTraceData) ->
                        //Trace.info "GOT EVENT: %O" data
                        Trace.info "%s" <| System.String.Format("{0:HH:mm:ss.fff}: {1,-12}: {2} ID: {3} EXIT: {4}", data.TimeStamp, data.TaskName, data.ImageFileName, data.ProcessID, data.ExitStatus)))


            let providerGuidName = "Microsoft-Windows-Kernel-Process"
            let OSProcessProviderGuid = TraceEventProviders.GetProviderGuidByName(providerGuidName)
            if OSProcessProviderGuid = Guid.Empty then
                failwithf "Error could not find %s etw provider." providerGuidName
            let r = session.EnableKernelProvider(flags = KernelTraceEventParser.Keywords.Process)
            #endif

            let lastReportedLostMessages = ref 0

            // Hook up the parser for dynamic tracelogging events
            session.Source.Dynamic.add_All(
                new Action<TraceEvent>(
                    fun (data:TraceEvent) ->
                            let lost = session.EventsLost
                            if lost <> !lastReportedLostMessages then
                                Trace.warning "ETW events were lost. Total events lost so far: %d" lost
                                lastReportedLostMessages := lost

                            match Map.tryFind data.ProviderGuid handlers with
                            | None -> Trace.error "Unhandled ETW provider: %O. Available handlers are: %A for the event: %A" data.ProviderGuid handlers data
                            | Some h -> h data))

            // Enable all handled ETW Tracelogging providers
            handlers
            |> Map.iter(fun p _ -> let r = session.EnableProvider(p, providerLevel = TraceEventLevel.Informational)
                                   if r then
                                     Trace.warning "The session for ETW provider %O already existed and was restarted" p)

            Trace.info "Starting Listening for events"

            // Start the event processing loop (blocking). The loop terminates when another thread calls 'session.Stop'.
            let result = session.Source.Process()

            Trace.info "Stopping Listening for events"
        }
    processingLoop |> Async.Start

    {
        new System.IDisposable with
           member x.Dispose() =
            session.Dispose()
    }

let stop (session:TraceEventSession) =
    session.Stop() |> ignore