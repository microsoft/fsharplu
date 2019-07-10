open Microsoft.FSharpLu.Logging

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    use logger = System.Diagnostics.Listener.registerFileAndConsoleTracer "myComponentName" None
    Trace.info "Information: Number of arguments %d" argv.Length
    Trace.error "Printing an error"
    Trace.warning "Printing a warning"

    0 
