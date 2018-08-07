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
    static member inline tryDeserialize< ^T > json =
        Helpers.tryDeserializeWithBoth< ^T, string>
            Compact.deserialize
            Default.deserialize
            ignore
            json
        |> Helpers.exceptionToString

    /// Try to deserialize json to an object of targetType
    static member inline tryDeserializeToType targetType json =
        Helpers.tryDeserializeWithBoth<obj, string>
            (Compact.deserializeToType targetType)
            (Default.deserializeToType targetType)
            ignore
            json
        |> Helpers.exceptionToString

    /// Try to read Json from a file and desrialized it to an object of type ^T
    static member inline tryDeserializeFile< ^T > file =
        Helpers.tryDeserializeWithBoth< ^T, string>
            Compact.deserializeFile
            Default.deserializeFile
            ignore
            file
        |> Helpers.exceptionToString

    /// Try to read Json from a file and desrialized it to an object of targetType
    static member inline tryDeserializeFileToType targetType file =
        Helpers.tryDeserializeWithBoth<obj, string>
            (Compact.deserializeFileToType targetType)
            (Default.deserializeFileToType targetType)
            ignore
            file
        |> Helpers.exceptionToString
            
    /// Try to deserialize a stream to an object of type ^T
    static member inline tryDeserializeStream< ^T > (stream:System.IO.Stream) =
        if not <| stream.CanSeek then
            failwith "BackwardCompat.deserializeStream only works with stream supporting the Seek() operator."
        Helpers.tryDeserializeWithBoth< ^T, System.IO.Stream>
            Compact.deserializeStream
            Default.deserializeStream
            (fun () -> stream.Seek(0L, System.IO.SeekOrigin.Begin) |> ignore)
            stream 
        |> Helpers.exceptionToString
            
    /// Try to deserialize a stream to an object of targetType
    static member inline tryDeserializeStreamToType targetType (stream:System.IO.Stream) =
        if not <| stream.CanSeek then
            failwith "BackwardCompat.deserializeStream only works with stream supporting the Seek() operator."
        Helpers.tryDeserializeWithBoth<obj, System.IO.Stream>
            (Compact.deserializeStreamToType targetType)
            (Default.deserializeStreamToType targetType)
            (fun () -> stream.Seek(0L, System.IO.SeekOrigin.Begin) |> ignore)
            stream 
        |> Helpers.exceptionToString

    /// Deserialize a Json to an object of type ^T
    static member inline deserialize< ^T > (json:string) : ^T =
        Helpers.tryDeserializeWithBoth< ^T, string>
            Compact.deserialize
            Default.deserialize
            ignore
            json |> Helpers.unboxOrRaise
    
    /// Deserialize a Json to an object of targetType
    static member inline deserializeToType targetType (json:string) =
        Helpers.tryDeserializeWithBoth<obj, string>
            (Compact.deserializeToType targetType)
            (Default.deserializeToType targetType)
            ignore
            json |> Helpers.unboxOrRaise

    /// Read Json from a file and desrialized it to an object of type ^T
    static member inline deserializeFile< ^T > file =
        Helpers.tryDeserializeWithBoth< ^T, string>
            Compact.deserializeFile
            Default.deserializeFile
            ignore
            file |> Helpers.unboxOrRaise

    /// Read Json from a file and desrialized it to an object of targetType
    static member inline deserializeFileToType targetType file =
        Helpers.tryDeserializeWithBoth<obj, string>
            (Compact.deserializeFileToType targetType)
            (Default.deserializeFileToType targetType)
            ignore
            file |> Helpers.unboxOrRaise

    /// Deserialize a stream to an object of type ^T
    static member inline public deserializeStream< ^T > (stream:System.IO.Stream) =
        if not <| stream.CanSeek then
            failwith "BackwardCompat.deserializeStream only works with stream supporting the Seek() operator."
        Helpers.tryDeserializeWithBoth< ^T, System.IO.Stream>
            Compact.deserializeStream
            Default.deserializeStream
            (fun () -> stream.Seek(0L, System.IO.SeekOrigin.Begin) |> ignore)
            stream |> Helpers.unboxOrRaise

    /// Deserialize a stream to an object of targetType
    static member inline public deserializeStreamToType targetType (stream:System.IO.Stream) =
        if not <| stream.CanSeek then
            failwith "BackwardCompat.deserializeStream only works with stream supporting the Seek() operator."
        Helpers.tryDeserializeWithBoth<obj, System.IO.Stream>
            (Compact.deserializeStreamToType targetType)
            (Default.deserializeStreamToType targetType)
            (fun () -> stream.Seek(0L, System.IO.SeekOrigin.Begin) |> ignore)
            stream |> Helpers.unboxOrRaise
