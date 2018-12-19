#r @"System.dll"
#r @"System.Core.dll"
// Replace %USERPROFILE% by your local user profile directory
// E.g. c:\Users\John\
#r @"%USERPROFILE%\.nuget\packages\Newtonsoft.Json\10.0.3\lib\net45\Newtonsoft.Json.dll"

#load "Helpers.fs"
#load "WithFunctor.fs"
#load "Default.fs"
#load "Compact.fs"
#load "BackwardCompatible.fs"
open Microsoft.FSharpLu.Json
open Newtonsoft.Json


module StrictDeserialization =
    type CamelCaseSettings =
        static member settings =
            let s =
                JsonSerializerSettings(
                    //NullValueHandling = NullValueHandling.Include,
                    MissingMemberHandling = MissingMemberHandling.Error,
                    ContractResolver = Serialization.CamelCasePropertyNamesContractResolver())
            s.Converters.Add(CompactUnionJsonConverter(true))
            s
        static member formatting = Formatting.Indented

    type CamelCaseSerializer = With<CamelCaseSettings>

    type C =
    | Ompa
    | Lompa

    type NumberSpecial = NumberSpecial of int

    type Ac = {
        number: NumberSpecial
        du: C
        testingOption: string option
        simpleInt : int
    }

    type Ab = {
        text: string
    }

    type B =
        | Ab of Ab
        | Ac of Ac

    [<JsonObject(ItemRequired=Required.Always)>]
    type A = {
        id: System.Guid
        context: B
        name: string
    }
    //des1 works fine
    let des1 ="""{ 
    "id":"f893e695-496d-4e30-8fc7-b2ff59725e6c",
    "context": {
        "ac": {
            "number": {
                "numberSpecial":2
            },
            "du":"ompa",
            "testingOption":"hejsvejs",
            "simpleInt":4
        }
    },
    "name":"hola"
    }"""

    CamelCaseSerializer.deserialize<A> des1

    //des2 does not throw error when obviously missing members...
    //its output is:
    // Choice1Of2 {id = 00000000-0000-0000-0000-000000000000;
    // context = Ac {number = null;
    // du = Ompa;
    // testingOption = None;
    // simpleInt = 0;};
    // name = "hola";}

    let des2 = """{
    "context": {
            "ac": {
                "du": "ompa"
            }
        },
    "name": "hola"
    }"""

    CamelCaseSerializer.tryDeserialize<A> des2

    let s2 =
        JsonSerializerSettings(
            //NullValueHandling = NullValueHandling.Include,
            MissingMemberHandling = MissingMemberHandling.Error
            //,
            //ContractResolver = Serialization.CamelCasePropertyNamesContractResolver()
            )
    //s2.Converters.Add(CompactUnionJsonConverter(true))

    [<JsonObject(ItemRequired=Required.Always)>] 
    type AA = {
        id: System.Guid
        name: string
    }
    let des3 ="""{ 
    "name":"hola"
    }"""
    let des4 ="""{ 
    "id":"f893e695-496d-4e30-8fc7-b2ff59725e6c",
    "name":"hola"
    }"""

    Newtonsoft.Json.JsonConvert.DeserializeObject<AA>(des3, s2)
    Newtonsoft.Json.JsonConvert.DeserializeObject<AA>(des4, s2)

//////
    type AAA = {
        id: System.Guid
        name: string
        f : int option
    }
    let des5 ="""{ 
    "name":"hola"
    }"""
    let des6 ="""{ 
    "id":"f893e695-496d-4e30-8fc7-b2ff59725e6c",
    "name":"hola"
    }"""

    /// A contract resolver that requires presence of all properties
    /// that are not of type option<_>
    type RequireNonOptionalPropertiesContractResolver() =
        inherit Newtonsoft.Json.Serialization.CamelCasePropertyNamesContractResolver()
        override __.CreateProperty(_member, memberSerialization) =
            let property = base.CreateProperty(_member, memberSerialization)
            let isRequired = not (property.PropertyType.IsGenericType 
                               && property.PropertyType.GetGenericTypeDefinition() = typedefof<option<_>>)
            if isRequired then 
                property.Required <- Required.Always
                property.NullValueHandling <- System.Nullable NullValueHandling.Ignore
            property

    let s3 =
        JsonSerializerSettings(
            ContractResolver = RequireNonOptionalPropertiesContractResolver(),
            Converters = [|CompactUnionJsonConverter(true)|]
            )
    Newtonsoft.Json.JsonConvert.DeserializeObject<AAA>(des5, s3)
    Newtonsoft.Json.JsonConvert.DeserializeObject<AAA>(des6, s3)

    Newtonsoft.Json.JsonConvert.DeserializeObject<AAA>(des5)
