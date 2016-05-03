/// Copyright (c) Microsoft Corporation.
/// Functors and types used to create strongly-typed Printf-like wrappers 
/// for Trace logging and diagnostics function
module Microsoft.FSharpLu.Logging

open System.Diagnostics
open System

/// A trace listener redirecting messages to the Console with nice colors 
/// for errors and warnings
type ColorConsoleTraceListener()  =
    inherit ConsoleTraceListener()

    let colorByEvent =
        Map.ofSeq
                [ TraceEventType.Verbose,     ConsoleColor.DarkGray
                  TraceEventType.Information, ConsoleColor.White
                  TraceEventType.Warning,     ConsoleColor.Yellow
                  TraceEventType.Error,       ConsoleColor.DarkRed
                  TraceEventType.Critical,    ConsoleColor.Red
                  TraceEventType.Start,       ConsoleColor.DarkCyan
                  TraceEventType.Stop,        ConsoleColor.DarkCyan ]

    override x.TraceEvent(eventCache:TraceEventCache, source:string,  eventType:TraceEventType, id:int, message:string) =
        x.TraceEvent(eventCache, source, eventType, id, "{0}", message)

    override __.TraceEvent(eventCache:TraceEventCache, source:string, eventType:TraceEventType, id:int, format:string, [<ParamArray>] args:obj[]) =
        let originalColor = Console.ForegroundColor
        Console.ForegroundColor <- defaultArg (colorByEvent.TryFind eventType) originalColor
        base.TraceEvent(eventCache, source, eventType, id, format, args)
        Console.ForegroundColor <- originalColor

/// Combine two tracers into one
type Combine< ^T1, ^T2 when 
                        ^T1 : (static member writeLine : string -> unit)
                    and ^T1 : (static member info : string -> unit) 
                    and ^T1 : (static member warning : string -> unit) 
                    and ^T1 : (static member error : string -> unit)
                    and ^T1 : (static member critical : string -> unit)
                    and ^T1 : (static member verbose : string -> unit)
                    and ^T1 : (static member writeLine : string -> unit)
                    and ^T1 : (static member flush : unit -> unit)
                    and ^T1 : (static member indent : unit -> unit)
                    and ^T1 : (static member unindent : unit -> unit) 

                    and ^T2 : (static member writeLine : string -> unit)
                    and ^T2 : (static member info : string -> unit) 
                    and ^T2 : (static member warning : string -> unit) 
                    and ^T2 : (static member error : string -> unit)
                    and ^T2 : (static member critical : string -> unit)
                    and ^T2 : (static member verbose : string -> unit)
                    and ^T2 : (static member writeLine : string -> unit)
                    and ^T2 : (static member flush : unit -> unit)
                    and ^T2 : (static member indent : unit -> unit)
                    and ^T2 : (static member unindent : unit -> unit)> =
    static member inline info m = 
        (^T1:(static member info : string -> unit) m)
        (^T2:(static member info : string -> unit) m)
    static member inline warning m = 
        (^T1:(static member warning : string -> unit) m)
        (^T2:(static member warning : string -> unit) m)
    static member inline error m =
        (^T1:(static member error : string -> unit) m)
        (^T2:(static member error : string -> unit) m)
    static member inline critical m =
        (^T1:(static member critical : string -> unit) m)
        (^T2:(static member critical : string -> unit) m)
    static member inline verbose m =
        (^T1:(static member verbose : string -> unit) m)
        (^T2:(static member verbose : string -> unit) m)
    static member inline writeLine m =
        (^T1:(static member writeLine : string -> unit) m)
        (^T2:(static member writeLine : string -> unit) m)
    static member inline flush () =
        (^T1:(static member flush : unit -> unit) ())
        (^T2:(static member flush : unit -> unit) ())
    static member inline indent () =
        (^T1:(static member indent : unit -> unit) ())
        (^T2:(static member indent : unit -> unit) ())
    static member inline unindent () =
        (^T1:(static member unindent : unit -> unit) ())
        (^T2:(static member unindent : unit -> unit) ())

/// Functor used to create a strongly-typed event tracer 
/// from regular string tracing functions
type StronglyTypedLogger< ^T when ^T : (static member writeLine : string -> unit)
                    and ^T : (static member info : string -> unit) 
                    and ^T : (static member warning : string -> unit) 
                    and ^T : (static member error : string -> unit)
                    and ^T : (static member critical : string -> unit)
                    and ^T : (static member verbose : string -> unit)
                    and ^T : (static member writeLine : string -> unit)
                    and ^T : (static member flush : unit -> unit)
                    and ^T : (static member indent : unit -> unit)
                    and ^T : (static member unindent : unit -> unit) > =
    static member inline info format = Printf.kprintf (fun m -> (^T:(static member info : string -> unit) m)) format
    static member inline warning format = Printf.kprintf (fun m -> (^T:(static member warning : string -> unit) m)) format
    static member inline error format = Printf.kprintf (fun m -> (^T:(static member error : string -> unit) m)) format
    static member inline critical format = Printf.kprintf (fun m -> (^T:(static member critical : string -> unit) m)) format
    static member inline failwith format = Printf.kprintf (fun m -> (^T:(static member critical : string -> unit) m); Operators.failwith m) format
    static member inline verbose format = Printf.kprintf (fun m -> (^T:(static member verbose : string -> unit) m)) format
    static member inline writeLine format  = Printf.kprintf (fun m -> (^T:(static member writeLine : string -> unit) m)) format
    static member inline flush () = (^T:(static member flush : unit -> unit) ())
    static member inline indent () =  (^T:(static member indent : unit -> unit) ())
    static member inline unindent () = (^T:(static member unindent : unit -> unit) ())

/// A tracer implemented with System.Diagnostics event tracing
type DiagnosticsTracer =
    static member inline info m = Trace.TraceInformation m
    static member inline warning m = Trace.TraceWarning m
    static member inline error m = Trace.TraceError m
    static member inline verbose m = Debug.Write m
    static member inline writeLine m = Trace.WriteLine m
    static member inline critical m = Trace.TraceError m
    static member inline flush () = Trace.Flush()
    static member inline indent () =  Trace.Indent()
    static member inline unindent () = Trace.Unindent()
  
/// Strongly-typed wrappers for System.Diagnostics event tracing
type Trace = StronglyTypedLogger<DiagnosticsTracer>

/// Event tracings with additional custom tags
module TraceTags =
    let propertiesToString (properties:seq<string*string>) =
        properties
        |> Seq.map (fun (n,v) -> sprintf "%s: %s" n v)
        |> Text.join ", "

    let inline info name properties = Trace.info "%s: %s" name (propertiesToString properties)
    let inline warning name properties = Trace.warning "%s: %s" name (propertiesToString properties)
    let inline error name properties = Trace.error "%s: %s" name (propertiesToString properties)
    let inline verbose name properties = Trace.verbose "%s: %s" name (propertiesToString properties)
    let inline critical name properties = Trace.critical "%s: %s" name (propertiesToString properties)
    let inline failwith name properties = Trace.failwith "%s: %s" name (propertiesToString properties)
    let inline event name properties = Trace.writeLine "Event: %s: %s" name (propertiesToString properties)
    let inline trackException (exn:System.Exception) properties = Trace.critical "Exception: %O: %s" exn (propertiesToString properties) 

/// Trace environment information to a strongly-typed tracer
let inline traceEnvironmentInfo< ^T when ^T : (static member writeLine : string -> unit)
                                and ^T : (static member info : string -> unit) 
                                and ^T : (static member warning : string -> unit) 
                                and ^T : (static member error : string -> unit)
                                and ^T : (static member critical : string -> unit)
                                and ^T : (static member verbose : string -> unit)
                                and ^T : (static member writeLine : string -> unit)
                                and ^T : (static member flush : unit -> unit)
                                and ^T : (static member indent : unit -> unit)
                                and ^T : (static member unindent : unit -> unit)> () =
    StronglyTypedLogger< ^T>.indent()
    StronglyTypedLogger< ^T>.writeLine "Operating system: %O" System.Environment.OSVersion
    StronglyTypedLogger< ^T>.writeLine "Computer name: %s" System.Environment.MachineName
    StronglyTypedLogger< ^T>.writeLine "User name: %s" System.Environment.UserName
    StronglyTypedLogger< ^T>.writeLine "CLR runtime version: %O" System.Environment.Version
    StronglyTypedLogger< ^T>.writeLine "Command line: %s" System.Environment.CommandLine
    StronglyTypedLogger< ^T>.unindent()

/// Interface for anything that can be flushed (e.g., stream)
type IFlushable =
    abstract Flush : unit -> unit

/// Self-cleanup interface for objects returned by function
/// redirectToConsoleAndFile
type ILoggerRedirect =
    inherit IDisposable
    inherit IFlushable
    abstract LogFilePath : string

/// Redirect all Trace.* tracing functions to console and file logging.
/// Returns an IDisposable that unregisters the tracing functions when disposed.
let redirectToConsoleAndFile componentName directory =
    let directory = defaultArg directory <| System.IO.Directory.GetCurrentDirectory()
    let consoleTracer = new ColorConsoleTraceListener(Name = componentName, TraceOutputOptions = TraceOptions.DateTime)
    let logFilePath = System.IO.Path.Combine (System.IO.Directory.GetCurrentDirectory(), sprintf "%s-%s.log"  componentName (System.DateTime.Now.ToString("yyyyMMdd-hhmmss")))
    let fileTracer = new TextWriterTraceListener(logFilePath, Name = componentName, TraceOutputOptions = TraceOptions.DateTime)
    consoleTracer.WriteLine(sprintf "%O + [%s] - Starting output to trace listener." System.DateTime.Now consoleTracer.Name)
    System.Diagnostics.Trace.Listeners.Add(consoleTracer) |> ignore
    System.Diagnostics.Trace.Listeners.Add(fileTracer) |> ignore
    Trace.writeLine "%s - Copyright Microsoft 2015-2016" componentName
    traceEnvironmentInfo<DiagnosticsTracer> ()
    { 
      new ILoggerRedirect with
        member __.LogFilePath 
            with get () = logFilePath

      interface IFlushable with
        member __.Flush() =
            consoleTracer.Flush()
            fileTracer.Flush()

      interface IDisposable with
        member this.Dispose() =
            consoleTracer.Dispose()
            fileTracer.Flush()
            fileTracer.Dispose()
    }

/// Idempotent version of redirectToConsoleAndFile that
/// attaches a new listener only on the first call. Subsequent calls are no-op
/// (even if the component name and directory parameter change).
let ensureRedirectToConsoleAndFile =
    let alreadyRegistered = ref None
    fun componentName directory ->
         match !alreadyRegistered with
         | None ->
            let redir = redirectToConsoleAndFile componentName directory
            alreadyRegistered := Some redir 
            redir
         | Some redir ->
            { 
                new ILoggerRedirect with member __.LogFilePath with get () = redir.LogFilePath
                interface IFlushable with member __.Flush() = ()
                interface IDisposable with member __.Dispose() = ()
            }
