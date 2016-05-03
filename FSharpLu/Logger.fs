(* 

Copyright (c) Microsoft Corporation.

Description:

    A logger type to manage distributed diagnostic messages.
    The use case is for programs consisting of multiple distributed agents where global Trace 
    logging is not appropriate due to concurrency and synchronization issues.

Author:

    William Blum (WiBlum)

Revision history:
    Repackaged into FSharpLu on 2/18/2015

*)
module Microsoft.FSharpLu.Logger

/// Writer interface definining the action to be performed 
/// on incoming messages returning a value of type 'a, 
//  and the function returning the current state of type 's.
/// The writer state can be used for instance to accumulate all incoming messages 
/// in an internal buffer.
type Writer<'a,'s> =
    {
        /// Action to be performed on each incoming string
        action : string -> 'a
        /// "no-op" action on incoming strings
        nop : string -> 'a
        /// Return the current state of the logger
        state : unit -> 's
    }

/// A logger type for distributed logging with strongly-typed "printf"-like writing functions.
///
/// Loggers defined in this module are meant to be used locally in-context and passed around throughout
/// the calls made in your program. This is to support the distributed setting where you have 
/// multiple agents logging to separate output. In this setting a global logging mechanism like 
/// Trace logging is not desirable as it would interleave log messages coming from different agents.
///
/// Avoid sharing Logger globally: global states break compositionality, leads to concurrency issues 
/// and make your code less testable. Instead just pass around your logger object throughout your progam.
/// For global tracing prefer the TraceLogging F#Lu module based based on ETW and System.Diagnostics tracing.
type Logger<'a,'s>(output : Writer<'a,'s>, isVerbose : bool) =

    let lineAction m = output.action (m + System.Environment.NewLine)

    new(output : Writer<'a,'s>) =
        Logger<'a, 's>(output, false)

    /// strongly-type write function 
    member x.write format =
        Printf.kprintf lineAction format

    /// strongly-type write function witout end of line
    member x.writeNoEndLine format =
        Printf.kprintf output.action format

    /// strongly-type verbose function 
    member x.verbose format =
        if isVerbose then
            Printf.kprintf lineAction format
        else
            Printf.kprintf output.nop format

    /// return the current state of the writer
    member x.state () =
        output.state ()

    /// strongly-typed failure handler: pass the error message to the writer, write it to the console
    /// and throw an exception
    member x.failWith format =
        Printf.ksprintf (fun msg -> let oldColor = System.Console.ForegroundColor
                                    System.Console.ForegroundColor <- System.ConsoleColor.Red
                                    let msg = "FATAL: " + msg
                                    let loggerState = lineAction msg
                                    System.Console.ForegroundColor <- oldColor
                                    failwith msg) format
    
    /// A dummy string write function for our C# friends
    member x.Write message =
        x.write "%s" message
            
/// Define built-in logging actions and combinators
/// An action is a function of type string -> unit.
module Action =
    /// Sequential composition of two actions
    let (|*) f g x = f x; g x

    /// Print to console (standard output)
    let inline out line =
        printf "%s" line

    let inline private append filepath content =
        System.IO.File.AppendAllText(filepath, content)

    /// Print to both console and external file
    let inline outAndFile filepath =
        append filepath
        |* out

    /// Print to both screen and output file within a critical section
    let inline outAndFileWithLock filepath lockObject s =
        let taskId = System.Threading.Tasks.Task.CurrentId
        let line = 
            if taskId.HasValue then
                sprintf "[%d] %s" taskId.Value s
            else 
                s 
        lock lockObject <| fun () -> append filepath line; out line

    /// Given a file path returns an action that thread safely write content to a file and to stdout
    /// if no file is given (None) returns a regular stdout action.
    let inline outAndFileThreadSafe fileName =
        match fileName with
        | None | Some null | Some "" -> out
        | Some fileName ->
            let lockObject = new obj()
            outAndFileWithLock fileName lockObject

/// Operation to create and combine loggers
[<AutoOpen>]
module Operation =
    /// Create an in-memory logger that stores all the logged messages
    /// in a string builder.
    let public makeMemoryLogger () =
        let buff = new System.Text.StringBuilder()
        let memoryWriter  =
            {
                action = buff.Append >> ignore
                nop = ignore
                state = fun () -> buff.ToString()
            }
        new Logger<_,_>(memoryWriter)

    /// Logger-aware aggregate function. It's similar to Seq.fold
    /// except that each value in the input sequence 'source'
    /// is paired with an associated Logger.
    /// The function returns the aggregated value together with
    /// a StringBuilder with the aggregated Logger's states.
    let public aggregate aggregate seed source =
        let aggregatedLogState = new System.Text.StringBuilder()
        let aggregatedResult = 
            Seq.fold (fun results (result, log:Logger<_,string>) -> 
                            aggregatedLogState.Append(log.state()) |> ignore
                            aggregate results result)
                        seed
                        source
        aggregatedResult, aggregatedLogState

    /// Create a stateless logger performing a custom action.
    /// Examples:
    ///     makeCustomLogger Action.stdout
    ///         returns a logger that prints to stdout
    ///     makeCustomLogger Action.stdoutFile
    ///         returns a logger that print to stdout and output to file
    let inline makeCustomLogger (action:string -> unit) =
        new Logger<unit,unit>(
            {
                action = action
                nop = ignore
                state = fun () -> ()
            })

    /// Default Log timestamp format use by timestampify
    let public logTimeStampFormat = "MM/dd/yyyy-HH:mm:ss"

    /// Prefix a message with the current timestamp
    let inline public timestampify componentName message =
        sprintf "%s [%s] %s" (System.DateTime.Now.ToString(logTimeStampFormat)) componentName message

    /// Create a thread-safe file logger if a file path is specified otherwise create a stdout-only logger.
    let makeLogger componentName fileName =
        timestampify componentName >> Action.outAndFileThreadSafe fileName
        |> makeCustomLogger
            
    /// 'Logger'-aware memoization function. The 'compute' function is called at most once and its
    /// result is "memoized". The 'compute' takes the logger object specified to the memoize function 
    /// as its first parameter.
    let memoize (compute : Logger<'a,'b> -> 'c) = 
        let cache = ref None
        fun (log:Logger<'a,'b>) ->
            match !cache with 
            | Some v -> v
            | None -> let v = compute log
                      cache := Some v
                      v