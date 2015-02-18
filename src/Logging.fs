(* 

Copyright (c) Microsoft Corporation.

Description:

    Logging helpers

Author:

    William Blum (WiBlum)

Revision history:
    Repackaged into FSharpLu on 2/18/2015

*)
module Microsoft.FSharpLu.Logging

/// A writer logger
/// 'a represents the action return type
/// 's represents the writer current state
type Writer<'a,'s> =
    {
        action : string -> 'a
        nop : string -> 'a
        state : unit -> 's
    }

let inline append filepath content =
    System.IO.File.AppendAllText(filepath, content)
    
let mutable private verbose = false

let public setVerbose v = 
    if v then
        printfn "Verbose mode activated"
    verbose <- v

/// Logger type where
/// 'a represents the action return type,
/// 's represents the logger current state
type Logger<'a,'s>(output : Writer<'a,'s>) =
    let lineAction m = output.action (m + System.Environment.NewLine)
    member x.writeNoEndLine format =
        Printf.kprintf output.action format
    member x.write format =
        Printf.kprintf lineAction format
    member x.verbose format =
        if verbose then
            Printf.kprintf lineAction format
        else
            Printf.kprintf output.nop format
    member x.state () =
        output.state ()
    /// Fatal handler: print to global logger and throw an exception.
    member x.fatal format =
        Printf.ksprintf (fun msg -> let oldColor = System.Console.ForegroundColor
                                    System.Console.ForegroundColor <- System.ConsoleColor.Red                                       
                                    let msg = "FATAL: " + msg
                                    let loggerState = lineAction msg
                                    System.Console.ForegroundColor <- oldColor
                                    failwith msg) format
    /// For C# interop
    member x.Write message =
        x.write "%s" message
            
/// Possible logging actions
module Action =
        /// Sequential composition
    let (|*) f g x = f x; g x

    /// Print to console (standard output)
    let inline out line =
        printf "%s" line

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

/// Create an in-memory logger
let public makeMemoryLogger () =
    let buff = new System.Text.StringBuilder()
    let memoryWriter  =
        {
            action = buff.Append >> ignore
            nop = ignore
            state = fun () -> buff.ToString()
        }
    new Logger<_,_>(memoryWriter)

/// Aggregate (reduce) a sequence of values
/// together with their associated logs
let public aggregate aggregate seed source =
    let aggregatedLog = new System.Text.StringBuilder()
    let aggregatedResult = 
        Seq.fold (fun results (result, log:Logger<_,string>) -> 
                        aggregatedLog.Append(log.state()) |> ignore
                        aggregate results result)
                    seed
                    source
    aggregatedResult, aggregatedLog

let public logTimeStampFormat = "MM/dd/yyyy-HH:mm:ss"

let inline public timestampify componentName message =
    sprintf "%s [%s] %s" (System.DateTime.Now.ToString(logTimeStampFormat)) componentName message

/// Create a stateless logger performing a custom action.
/// Examples:
///     makeCustomLogger Action.stdout
///         returns a logger that prints to screen
///     makeCustomLogger Action.stdoutFile
///         returns a logger that print to both screen and output file
let inline makeCustomLogger (action:string -> unit) =
    new Logger<unit,unit>(
        {
            action = action
            nop = ignore
            state = fun () -> ()
        })

/// Create a thread-safe file logger if a file path is specified otherwise create a stdout-only logger.
let makeLogger componentName fileName =
    timestampify componentName >> Action.outAndFileThreadSafe fileName
    |> makeCustomLogger
            
/// Log-aware memoization. Memoize the specified function 'compute'; when the function
/// compute is called it is passed the given logger object as a first parameter.
let memoize (compute : Logger<'a,'b> -> 'c) = 
    let cache = ref None
    fun (log:Logger<'a,'b>) ->
        match !cache with 
        | Some v -> v
        | None -> let v = compute log
                  cache := Some v
                  v
    
/// A process-wide global logger. 
/// CAUTION: This should only be used to log process-wide global messages
/// (for instance when initializing global variables). Whenever possible 
/// one should use a context-dependent logger that is
/// passed around as a parameter to all functions. This guarantees
/// compositionality and allow concurrent workflow to be run in parallel
let mutable glog = makeLogger "[global]" None
