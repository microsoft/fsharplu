namespace Microsoft.FSharpLu.Json

/// Default NetwonSoft Json.Net serializer
module Default =
    open Newtonsoft.Json
    open System.Runtime.CompilerServices

    /// We don't want to expose type JsonSerializerSettings in the current namespace as it would
    /// force any calling assembly to unecessarily add a direct reference to Newtosoft.Json.
    /// Instead we nest it under an Internal module.
    /// (Cannot actually be marked 'internal' or 'private' due to restriction in F#
    /// with static type inlining.
    module Internal =
        /// Serialization settings for the default NetwonSoft Json.Net's serialization format
        type DefaultSettings =
            static member settings =
                let s = JsonSerializerSettings(NullValueHandling = NullValueHandling.Ignore)
                s.Converters.Add(Converters.StringEnumConverter())
                s

    type private S = With<Internal.DefaultSettings>

    // A side-effect of above comment is that it forces us to manually inline the type alias
    // and redefine all the method below manually. We also need to annotate the wrappers
    // with the NoInlining attribute to prevent the code to be inlined in caller assemblies.
       
    /// Serialize an object to Json with the specified converter
    [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
    let inline serialize< ^T> x = S.serialize x
    /// Serialize an object to Json with the specified converter and save the result to a file
    [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
    let inline serializeToFile< ^T> file obj = S.serializeToFile file obj
    /// Try to deserialize json to an object of type ^T
    [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
    let inline tryDeserialize< ^T> json = S.tryDeserialize< ^T> json
    /// Try to read Json from a file and desrialized it to an object of type ^T
    [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
    let inline tryDeserializeFile< ^T> file = S.tryDeserializeFile< ^T> file
    /// Try to deserialize a stream to an object of type ^T
    [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
    let inline tryDeserializeStream< ^T> stream = S.tryDeserializeStream< ^T> stream
    /// Deserialize a Json to an object of type ^T
    [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
    let inline deserialize< ^T> json : ^T = S.deserialize< ^T> json
    /// Read Json from a file and desrialized it to an object of type ^T
    [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
    let inline deserializeFile< ^T> file = S.deserializeFile< ^T> file
    /// Deserialize a stream to an object of type ^T
    [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
    let inline deserializeStream< ^T> stream = S.deserializeStream< ^T> stream

