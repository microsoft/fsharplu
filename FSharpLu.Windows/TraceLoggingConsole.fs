// Helper functions for System.Diagnostics
namespace System.Diagnostics
    open System

    /// Defines color scheme used to print each event type to the console
    type EventColoring = Map<TraceEventType, ConsoleColor>

    /// A System.Diagnostics trace listener redirecting messages to
    /// the Console with nice colors for errors and warnings.
    type ColorConsoleTraceListener(foregroundColors :EventColoring, backgroundColors :EventColoring) as this =
        inherit ConsoleTraceListener()

        let shouldTraceEvent (eventType:TraceEventType) =
            let isEnabled (e: TraceEventType) = eventType &&& e <> enum<TraceEventType> 0
            if this.Attributes.ContainsKey "traceLevel" then
                match TraceLevel.TryParse<TraceLevel>(this.Attributes.["traceLevel"]) with
                | true, TraceLevel.Off -> false
                | true, TraceLevel.Error ->
                    isEnabled (TraceEventType.Critical ||| TraceEventType.Error)
                | true, TraceLevel.Warning ->
                    isEnabled (TraceEventType.Critical ||| TraceEventType.Error ||| TraceEventType.Warning)
                | true, TraceLevel.Info ->
                    isEnabled (TraceEventType.Critical ||| TraceEventType.Error ||| TraceEventType.Warning ||| TraceEventType.Information)
                | true, TraceLevel.Verbose ->
                    isEnabled (TraceEventType.Critical ||| TraceEventType.Error ||| TraceEventType.Warning ||| TraceEventType.Information ||| TraceEventType.Verbose)
                | _ -> true
            else
                true

        override x.TraceEvent(eventCache:TraceEventCache, source:string, eventType:TraceEventType, id:int, message:string) =
            if shouldTraceEvent eventType then
                x.TraceEvent(eventCache, source, eventType, id, "{0}", message)

        override __.TraceEvent(eventCache:TraceEventCache, source:string, eventType:TraceEventType, id:int, format:string, [<ParamArray>] args:obj[]) =
            if shouldTraceEvent eventType then
                let originalForegroundColor, originalBackgroundColor = Console.ForegroundColor, Console.BackgroundColor
                Console.ForegroundColor <- defaultArg (foregroundColors.TryFind eventType) originalForegroundColor
                Console.BackgroundColor <- defaultArg (backgroundColors.TryFind eventType) originalBackgroundColor
                base.TraceEvent(eventCache, source, eventType, id, format, args)
                Console.ForegroundColor <- originalForegroundColor
                Console.BackgroundColor <- originalBackgroundColor

        /// Trace logging configuration passed to registerFileAndConsoleTracerWithConfiguration
        type ConsoleLoggingConfiguration =
            {
                /// Foreground event color scheme
                foregroundColors : EventColoring
                /// Background event  color scheme
                backgroundColors : EventColoring
            }

    module Listener =

        /// Default console coloring scheme for events
        let defaultForegroundColors =
            Map.ofSeq
              [ TraceEventType.Verbose,     ConsoleColor.DarkGray
                TraceEventType.Information, ConsoleColor.White
                TraceEventType.Warning,     ConsoleColor.Yellow
                TraceEventType.Error,       ConsoleColor.DarkRed
                TraceEventType.Critical,    ConsoleColor.Red
                TraceEventType.Start,       ConsoleColor.DarkCyan
                TraceEventType.Stop,        ConsoleColor.DarkCyan ]

        /// By default use the existing background color when printing events to the console
        let defaultBackgroundColors =
            Map.empty

        /// Register a System.Diagnostics trace listener that redirects tracing functions with specified TraceLevels
        /// to the console with specified coloring scheme, and to a log file on disk.
        /// Returns an IDisposable that, when disposed, unregisters the listners.
        let registerFileAndConsoleTracerWithConfiguration (parameters:System.Diagnostics.LoggingConfiguration<ConsoleLoggingConfiguration>) =
            let newConsoleLogger parameters =
                new ColorConsoleTraceListener(
                        parameters.auxiliaryConfiguration.foregroundColors,
                        parameters.auxiliaryConfiguration.backgroundColors,
                        Name = parameters.componentName,
                        TraceOutputOptions = parameters.traceOptions)
            System.Diagnostics.Listener.registerFileAndAuxiliaryTracerWithConfiguration<ColorConsoleTraceListener, ConsoleLoggingConfiguration> newConsoleLogger parameters
            
        /// Register a System.Diagnostics trace listener that redirects all tracing functions
        /// to the console and to a log file on disk.
        /// Returns an IDisposable that, when disposed, unregisters the listners.
        let registerFileAndConsoleTracer componentName directory =
            registerFileAndConsoleTracerWithConfiguration {
                title = componentName
                componentName = componentName
                auxiliaryTraceLevel = TraceLevel.Verbose
                auxiliaryConfiguration =
                    {
                        ConsoleLoggingConfiguration.foregroundColors = defaultForegroundColors
                        ConsoleLoggingConfiguration.backgroundColors = defaultBackgroundColors
                    }
                directory = directory
                traceOptions = TraceOptions.DateTime
            }