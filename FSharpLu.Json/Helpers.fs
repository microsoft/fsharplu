module Microsoft.FSharpLu.Json.Helpers

open Newtonsoft.Json

let inline unboxOrRaise x =
    match x with
    | Choice1Of2 v -> v
    | Choice2Of2 exn -> raise exn

let inline exceptionToString x =
    match x with
    | Choice1Of2 v -> Choice1Of2 <| v
    | Choice2Of2 (exn:#System.Exception) -> 
        Choice2Of2 <| (sprintf "An exception occurred during deserialization: %O" exn)

/// Try to run the specifed deserialization funciton and catch any exception that may occur
/// Return either the exception (not thrown) or the deserialized result
let inline tryCatchJsonSerializationException< ^T, ^V> 
            allowNull
            (deserialize : ^V -> ^T) 
            (v:^V) : Choice< ^T, System.Exception> =
    try
        let o = deserialize v
        if not allowNull && obj.ReferenceEquals(o, null) then
            Choice2Of2 <| (JsonReaderException("Json deserialization returned null") :> System.Exception)
        else
            Choice1Of2 o
    with
    | :? JsonReaderException
    | :? JsonSerializationException as exn ->
        Choice2Of2 <| exn
    | exn ->
        Choice2Of2 <| (JsonReaderException("Exception while deserializing stream: %O", exn) :> System.Exception)

/// Try deserialize with two deserializers sequentially
let inline tryDeserializeWithBoth< ^T , ^V >
        (firstDeserialize : ^V -> ^T)
        (secondDeserialize : ^V -> ^T)
        (rewind : unit -> unit)
        (v:^V)
        =
    
    match tryCatchJsonSerializationException true firstDeserialize v with
    | Choice1Of2 _ as parsedObject ->
        parsedObject
    
    | Choice2Of2 firstException ->
        rewind()
        match tryCatchJsonSerializationException true secondDeserialize v with
        | Choice1Of2 _ as parsedObject -> parsedObject
        | Choice2Of2 secondException ->
            Choice2Of2 <| (JsonSerializationException("Could not deserialize with any of the serializers. See inner exception for error reported by the first deserializer.", firstException) :> System.Exception)
