namespace Microsoft.FSharpLu.Json

open Newtonsoft.Json
open System.Runtime.CompilerServices

/// Functor used to create Json serialization helpers for specific serializer settings
/// Warning: Because this functor depends on type JsonSerializerSettings defined in
/// NewtonSoft.Json any calling assembly using this type will
/// also need to add a direct reference to NewtonSoft.Json.
type With< ^S when ^S : (static member settings : JsonSerializerSettings)
                and ^S : (static member formatting : Formatting) > =

    static member inline public formatting () =
        (^S:(static member formatting : Formatting)())

    /// Serialize an object to Json with the specified converter
    static member inline public serialize (obj:^T) =
        let settings = (^S:(static member settings : JsonSerializerSettings)())
        let formatting = (^S:(static member formatting : Formatting)())
        JsonConvert.SerializeObject(obj, formatting, settings)

    /// Serialize an object to Json with the specified converter and save the result to a file
    static member inline public serializeToFile file (obj:^T) =
        let settings = (^S:(static member settings : JsonSerializerSettings)())
        let formatting = (^S:(static member formatting : Formatting)())
        let json = JsonConvert.SerializeObject(obj, formatting, settings)
        System.IO.File.WriteAllText(file, json)

    /// Serialize an object to Json with the specified converter and write the result to a stream
    static member inline public serializeToStream (stream:System.IO.Stream) (obj:^T) =
        let settings = (^S:(static member settings : JsonSerializerSettings)())
        let formatting = (^S:(static member formatting : Formatting)())
        let serializer = JsonSerializer.Create(settings)
        serializer.Formatting <- formatting
        // Leave stream open after writing
        use streamWriter = new System.IO.StreamWriter(stream, System.Text.UTF8Encoding.UTF8, 1024, true)
        use jsonTextWriter = new JsonTextWriter(streamWriter)
        serializer.Serialize(jsonTextWriter, obj)

    /// Deserialize a Json to an object of type 'T
    static member inline public deserialize< ^T> json :^T =
        let settings = (^S:(static member settings :  JsonSerializerSettings)())
        let formatting = (^S:(static member formatting : Formatting)())
        JsonConvert.DeserializeObject< ^T>(json, settings)

    /// Deserialize a stream to an object of type 'T
    static member inline public deserializeStream< ^T> (stream:System.IO.Stream) =
        let settings = (^S:(static member settings :  JsonSerializerSettings)())
        let serializer = JsonSerializer.Create(settings)
        use streamReader = new System.IO.StreamReader(stream)
        use jsonTextReader = new JsonTextReader(streamReader)
        serializer.Deserialize< ^T>(jsonTextReader)

    /// Read Json from a file and desrialized it to an object of type ^T
    static member inline deserializeFile< ^T> file :^T =
        System.IO.File.ReadAllText file |> With< ^S>.deserialize

    /// Try to deserialize a stream to an object of type ^T
    static member inline tryDeserializeStream< ^T> stream =
        Helpers.tryCatchJsonSerializationException< ^T, System.IO.Stream> false (With< ^S>.deserializeStream) stream
        |> Helpers.exceptionToString

    /// Try to deserialize json to an object of type ^T
    static member inline tryDeserialize< ^T> json =
        Helpers.tryCatchJsonSerializationException< ^T, string> false (With< ^S>.deserialize) json
        |> Helpers.exceptionToString

    /// Try to read Json from a file and desrialized it to an object of type 'T
    static member inline tryDeserializeFile< ^T> file =
        Helpers.tryCatchJsonSerializationException< ^T, string> false (With< ^S>.deserializeFile) file
        |> Helpers.exceptionToString
