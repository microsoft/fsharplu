namespace Microsoft.FSharpLu.Json

open Newtonsoft.Json

/// Default serialization settings with default NetwonSoft Json.Net's serialization
type DefaultSettings =
    static member settings =
        let s = JsonSerializerSettings(NullValueHandling = NullValueHandling.Ignore)
        s.Converters.Add(Converters.StringEnumConverter())
        s

/// Default NetwonSoft Json.Net serializer
type Default = With<DefaultSettings> 
