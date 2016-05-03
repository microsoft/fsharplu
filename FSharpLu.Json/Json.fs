namespace Microsoft.FSharpLu.Json

open Newtonsoft.Json

/// Json serialization helpers for specific serializer settings
type With< ^S when ^S : (static member settings : JsonSerializerSettings) > =

    /// Serialize an object to Json with the specified converter
    static member inline public serialize (obj:^T) =
        let settings = (^S:(static member settings : JsonSerializerSettings)())
        JsonConvert.SerializeObject(obj, Formatting.Indented, settings)

    /// Serialize an object to Json with the specified converter and save the result to a file
    static member inline public serializeToFile file (obj:^T) =
        let settings = (^S:(static member settings : JsonSerializerSettings)())
        let json = JsonConvert.SerializeObject(obj, Formatting.Indented, settings)
        System.IO.File.WriteAllText(file, json)

    /// Deserialize a Json to an object of type 'T
    static member inline public deserialize< ^T> json  =
        let settings = (^S:(static member settings :  JsonSerializerSettings)())
        JsonConvert.DeserializeObject< ^T>(json, settings)

    /// Deserialize a stream to an object of type 'T
    static member inline public deserializeStream< ^T> (stream:System.IO.Stream) =
        let settings = (^S:(static member settings :  JsonSerializerSettings)())
        let serializer = JsonSerializer.Create(settings)
        use streamReader = new System.IO.StreamReader(stream)
        use jsonTextReader = new JsonTextReader(streamReader)
        serializer.Deserialize< ^T>(jsonTextReader)

    /// Read Json from a file and desrialized it to an object of type ^T
    static member inline deserializeFile< ^T> file =
        let settings = (^S:(static member settings :  JsonSerializerSettings)())
        System.IO.File.ReadAllText file |> With< ^S>.deserialize

    /// Try to deserialize a stream to an object of type ^T
    static member inline tryDeserializeStream< ^T> stream =
        let settings = (^S:(static member settings :  JsonSerializerSettings)())
        try
            let o = With< ^S>.deserializeStream< ^T> stream
            if obj.ReferenceEquals(o, null) then
                Choice2Of2 <| "Deserialization returned null"
            else
                Choice1Of2 o
        with
        | :? JsonReaderException
        | :? JsonSerializationException as exn ->
            Choice2Of2 <| sprintf "Json exception thrown while deserializing stream: %O" exn
        | exn ->
            Choice2Of2 <| sprintf "Exception while deserializing stream: %O" exn

    /// Try to deserialize json to an object of type ^T
    static member inline tryDeserialize< ^T> json =
        let settings = (^S:(static member settings :  JsonSerializerSettings)())
        try
            let o = With< ^S>.deserialize< ^T> json
            if obj.ReferenceEquals(o, null) then
                Choice2Of2 <| "Deserialization returned null"
            else
                Choice1Of2 o
        with
        | :? JsonReaderException
        | :? JsonSerializationException as exn ->
            Choice2Of2 <| sprintf "Json exception thrown while deserializing string: %O" exn
        | exn ->
            Choice2Of2 <| sprintf "Exception while deserializing string: %O" exn

    /// Try to read Json from a file and desrialized it to an object of type 'T
    static member inline tryDeserializeFile< ^T> file =
        let settings = (^S:(static member settings :  JsonSerializerSettings)())
        try
            let o = With< ^S>.deserializeFile< ^T> file
            if obj.ReferenceEquals(o, null) then
                Choice2Of2 <| sprintf "Deserialization of %s returned null" file
            else
                Choice1Of2 o
        with
        | :? JsonReaderException
        | :? JsonSerializationException as exn ->
            Choice2Of2 <| sprintf "Json exception thrown while deserializing file %s: %O" file exn
        | exn ->
            Choice2Of2 <| sprintf "Exception while deserializing file %s: %O" file exn

/// Default serialization settings
type DefaultSettings =
    static member settings =
        let s = JsonSerializerSettings(NullValueHandling = NullValueHandling.Ignore)
        s.Converters.Add(Converters.StringEnumConverter())
        s

type Default = With<DefaultSettings> 
