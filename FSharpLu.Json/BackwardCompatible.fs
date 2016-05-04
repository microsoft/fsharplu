namespace Microsoft.FSharpLu.Json

open Newtonsoft.Json
open Microsoft.FSharp.Reflection

/// Backward compatible version of the Compat F# datatype serializer.
/// It serializes using the Compact format but can deserialize Json produced 
/// in both Compact and Default format.
type BackwardCompatible =
    
    /// Serialize an object to Json with the specified converter
    static member inline public serialize = Compact.serialize

    /// Serialize an object to Json with the specified converter and save the result to a file
    static member inline public serializeToFile file (obj:^T) = Compact.serializeToFile

    /// Try to deserialize json to an object of type ^T
    static member inline tryDeserialize< ^T when ^T :equality> json =
        Helpers.tryDeserializeWithBoth< ^T, string>
            Compact.deserialize
            Default.deserialize
            ignore
            json
        |> Helpers.exceptionToString

    /// Try to read Json from a file and desrialized it to an object of type ^T
    static member inline tryDeserializeFile< ^T when ^T :equality> file =
        Helpers.tryDeserializeWithBoth< ^T, string>
            Compact.deserializeFile
            Default.deserializeFile
            ignore
            file
        |> Helpers.exceptionToString
            
    /// Try to deserialize a stream to an object of type ^T
    static member inline tryDeserializeStream< ^T when ^T :equality> (stream:System.IO.Stream) =
        if not <| stream.CanSeek then
            failwith "BackwardCompat.deserializeStream only works with stream supporting the Seek() operator."
        Helpers.tryDeserializeWithBoth< ^T, System.IO.Stream>
            Compact.deserializeStream
            Default.deserializeStream
            (fun () -> stream.Seek(0L, System.IO.SeekOrigin.Begin) |> ignore)
            stream 
        |> Helpers.exceptionToString

    /// Deserialize a Json to an object of type ^T
    static member inline deserialize< ^T when ^T :equality> (json:string) : ^T =
        Helpers.tryDeserializeWithBoth< ^T, string>
            Compact.deserialize
            Default.deserialize
            ignore
            json |> Helpers.unboxOrRaise

    /// Read Json from a file and desrialized it to an object of type ^T
    static member inline deserializeFile< ^T when ^T :equality> file =
        Helpers.tryDeserializeWithBoth< ^T, string>
            Compact.deserializeFile
            Default.deserializeFile
            ignore
            file |> Helpers.unboxOrRaise

    /// Deserialize a stream to an object of type ^T
    static member inline public deserializeStream< ^T when ^T :equality> (stream:System.IO.Stream) =
        if not <| stream.CanSeek then
            failwith "BackwardCompat.deserializeStream only works with stream supporting the Seek() operator."
        Helpers.tryDeserializeWithBoth< ^T, System.IO.Stream>
            Compact.deserializeStream
            Default.deserializeStream
            (fun () -> stream.Seek(0L, System.IO.SeekOrigin.Begin) |> ignore)
            stream |> Helpers.unboxOrRaise

