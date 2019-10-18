module Microsoft.FSharpLu.Azure.AppInsights

open Microsoft.ApplicationInsights
open Microsoft.FSharpLu

/// TelemetryClient is thread-safe. Ok to have global instance.
(* https://azure.microsoft.com/en-us/documentation/articles/app-insights-api-custom-events-metrics/
    We recommend you use an instance of TelemetryClient for each module of your app. For instance, you may have one TelemetryClient in your web service to report
    incoming http requests, and another in a middleware class to report business logic events. You can set properties such as TelemetryClient.Context.User.Id to track
    users and sessions, or TelemetryClient.Context.Device.Id to identify the machine. This information is attached to all events sent by the instance.
*)
let telemetryClient =
    match System.Environment.OSVersion.Platform with
    | System.PlatformID.Win32Windows
    | System.PlatformID.Win32NT ->
        let config = Extensibility.TelemetryConfiguration.CreateDefault()
        new TelemetryClient(config)
    | _ ->
        Logging.TraceTags.info "Setting telemetry platform to in-memory telemetry channel" [ "OSVersion", System.Environment.OSVersion.ToString() ]
        let config = new Extensibility.TelemetryConfiguration(TelemetryChannel = new Channel.InMemoryChannel())
        TelemetryClient(config)


/// Set the AppInsight instrumentation key obtained from the Azure portal
let setInstrumentationKey key =
    telemetryClient.Context.InstrumentationKey <- key

/// Set name of component for which we are collecting data
let setComponent componentName =
    telemetryClient.Context.GlobalProperties.Add("Component", componentName)

/// Set trace detail level for logging to AppInsights
let setTraceLevel (traceLevel: System.Diagnostics.TraceLevel) =
    telemetryClient.Context.GlobalProperties.Add("TraceLevel", traceLevel.ToString())

/// Get trace level detail for logging to AppInsights. If it was never set, then assume Verbose level.
let inline getTraceLevel() =
    if telemetryClient.Context.GlobalProperties.ContainsKey("TraceLevel") then
        match System.Enum.TryParse(telemetryClient.Context.GlobalProperties.["TraceLevel"]) with
        | false, _ -> failwithf "Failed to parse TraceLevel defined in telemetryClient.Context.Properties: %s" telemetryClient.Context.GlobalProperties.["TraceLevel"]
        | true, traceLevel -> traceLevel
    else
        System.Diagnostics.TraceLevel.Verbose

/// Tracer implemented using AppInsights
/// Mirrors tracer defined in Microsoft.FSharpLu.Logging
/// Here the tracer sends information to AppInsights rather than System.Diagnostics
type Tracer =
    static member inline info message =
        if getTraceLevel() >= System.Diagnostics.TraceLevel.Info then
            telemetryClient.TrackTrace(message, DataContracts.SeverityLevel.Information)

    static member inline warning message =
        if getTraceLevel() >= System.Diagnostics.TraceLevel.Warning then
            telemetryClient.TrackTrace(message, DataContracts.SeverityLevel.Warning)

    static member inline error message =
        if getTraceLevel() >= System.Diagnostics.TraceLevel.Error then
            telemetryClient.TrackTrace(message, DataContracts.SeverityLevel.Error)

    static member inline verbose message =
        if getTraceLevel() >= System.Diagnostics.TraceLevel.Verbose then
            telemetryClient.TrackTrace(message, DataContracts.SeverityLevel.Verbose)

    static member inline writeLine message =
        if getTraceLevel() > System.Diagnostics.TraceLevel.Off then
            Tracer.info message

    static member inline critical message =
        if getTraceLevel() >= System.Diagnostics.TraceLevel.Error then
            telemetryClient.TrackTrace(message, DataContracts.SeverityLevel.Critical)

    static member inline flush () = telemetryClient.Flush()
    static member inline indent () = ()
    static member inline unindent () = ()

/// Strongly-typed AppInsight tracer
type Trace = Microsoft.FSharpLu.Logging.StronglyTypedTracer<Tracer>

/// Trace logging with additional tags parameters
type TagsTracer =
    static member inline info (message, tags) =
        if getTraceLevel() >= System.Diagnostics.TraceLevel.Info then
            telemetryClient.TrackTrace(message, DataContracts.SeverityLevel.Information, dict tags)

    static member inline writeLine (message, tags) =
        if getTraceLevel() >= System.Diagnostics.TraceLevel.Info then
            telemetryClient.TrackTrace(message, DataContracts.SeverityLevel.Information, dict tags)

    static member inline warning (message, tags) =
        if getTraceLevel() >= System.Diagnostics.TraceLevel.Warning then
            telemetryClient.TrackTrace(message, DataContracts.SeverityLevel.Warning, dict tags)

    static member inline error (message, tags) =
        if getTraceLevel() >= System.Diagnostics.TraceLevel.Error then
            telemetryClient.TrackTrace(message, DataContracts.SeverityLevel.Error, dict tags)

    static member inline critical (message, tags) =
        if getTraceLevel() >= System.Diagnostics.TraceLevel.Error then
            telemetryClient.TrackTrace(message, DataContracts.SeverityLevel.Critical, dict tags)

    static member inline verbose (message, tags) =
        if getTraceLevel() >= System.Diagnostics.TraceLevel.Verbose then
            telemetryClient.TrackTrace(message, DataContracts.SeverityLevel.Verbose, dict tags)

    static member inline failwith (message, tags) =
        if getTraceLevel() >= System.Diagnostics.TraceLevel.Error then
            telemetryClient.TrackTrace(message, DataContracts.SeverityLevel.Critical, dict tags)
        Operators.failwith message

    static member inline event (eventName, tags) =
        if getTraceLevel() >= System.Diagnostics.TraceLevel.Error then
            telemetryClient.TrackEvent(eventName, dict tags)

    static member inline trackException (exn:System.Exception, tags) =
        if getTraceLevel() >= System.Diagnostics.TraceLevel.Error then
            telemetryClient.TrackException(exn, dict tags)

    static member inline flush () = ()
    static member inline indent () = ()
    static member inline unindent () = ()


 type TraceTags =
    static member inline info message tags = TagsTracer.info (message, tags)
    static member inline writeLine message tags = TagsTracer.writeLine (message, tags)
    static member inline warning message tags = TagsTracer.warning (message, tags)
    static member inline error message tags = TagsTracer.error (message, tags)
    static member inline critical message tags = TagsTracer.critical (message, tags)
    static member inline verbose message tags = TagsTracer.verbose (message, tags)
    static member inline failwith message tags = TagsTracer.failwith (message, tags)
    static member inline event eventName tags = TagsTracer.event (eventName, tags)
    static member inline trackException exn tags = TagsTracer.trackException (exn, tags)
    static member inline trackRequest (client:TelemetryClient) httpMethod uri startTime duration responseCode success properties =
        let requestTelemetry = new DataContracts.RequestTelemetry(Url = uri, HttpMethod = httpMethod, Name = uri.PathAndQuery, StartTime = startTime, Duration = duration, ResponseCode = responseCode, Success = System.Nullable<_> success)
        properties
        |> Seq.map(fun (key, value) -> System.Collections.Generic.KeyValuePair<_,_>(key, value) )
        |> Seq.iter requestTelemetry.Properties.Add
        client.TrackRequest requestTelemetry
    static member inline flush () = ()
    static member inline indent () = ()
    static member inline unindent () = ()
    static member inline environmentInfo () =
        TagsTracer.event
            ("EnvironmentInfo",
                [ "Operating system", System.Environment.OSVersion.ToString()
                  "Computer name", System.Environment.MachineName
                  "User name", System.Environment.UserName
                  "CLR runtime version", System.Environment.Version.ToString()
                  "Command line", System.Environment.CommandLine ])

/// Redirect tracing events to both System.Diagnostics and AppInsights
module DiagnosticsAndAppInsights =
    /// Trace to both System.Diagnostics and AppInsights
    type Trace = Logging.StronglyTypedTracer<Logging.Combine<Tracer, System.Diagnostics.DiagnosticsTracer>>

    /// Trace to both System.Diagnostics and AppInsights with additional custom parameters
    /// Tag tracing to both System.Diagnostics and DMZ
    type TraceTags =
        Microsoft.FSharpLu.Logging.TraceTags.Combine<
            TagsTracer,
            System.Diagnostics.TagsTracer>