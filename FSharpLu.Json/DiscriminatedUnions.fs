namespace Microsoft.FSharpLu.Json

/// Serializers for F# discriminated unions improving upon the stock implementation by JSon.Net
/// The default formatting used by Json.Net to serialize F# discriminated unions
/// and Option types is too verbose. This module implements a more succinct serialization
/// for those data types.

open Newtonsoft.Json
open Microsoft.FSharp.Reflection

/// Improved Json converter for discriminated unions and option types ('a option). 
type DiscriminatedUnionJsonConverter() =
    inherit Newtonsoft.Json.JsonConverter()
        
    override __.CanConvert(objectType:System.Type) = 
        // Include F# discriminated unions
        FSharpType.IsUnion objectType
        // and exclude the standard FSharp lists (which are implemented as discriminated unions)
        && not (objectType.IsGenericType && objectType.GetGenericTypeDefinition() = typedefof<_ list>)
           
    override __.WriteJson(writer:JsonWriter, value:obj, serializer:JsonSerializer) =
        let t = value.GetType()
        // Option type?
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>> then
            if isNull value then
                ()
            else 
                let _,fields = FSharpValue.GetUnionFields(value, t)
                serializer.Serialize(writer, fields.[0])
        // Discriminated union
        else
            let case, fields = FSharpValue.GetUnionFields(value, t)
            match fields with
            // Field-less union case
            | [||] -> 
                writer.WriteValue(sprintf "%A" value)
            // Case with single field
            | [|onefield|] ->
                writer.WriteStartObject()
                writer.WritePropertyName(case.Name)
                serializer.Serialize(writer, onefield)
                writer.WriteEndObject()
            // Case with list of fields
            | _ ->
                writer.WriteStartObject()
                writer.WritePropertyName(case.Name)
                serializer.Serialize(writer, fields)
                writer.WriteEndObject()

    override __.ReadJson(reader:JsonReader, objectType:System.Type, existingValue:obj, serializer:JsonSerializer) =
        let cases = FSharpType.GetUnionCases(objectType)
        // Option type?
        if objectType.IsGenericType && objectType.GetGenericTypeDefinition() = typedefof<option<_>> then
                let caseNone, caseSome = cases.[0], cases.[1]
                let jToken = Linq.JToken.ReadFrom(reader)
                if jToken.Type = Linq.JTokenType.Null then
                    FSharpValue.MakeUnion(caseNone, [||])
                else 
                    let nestedType = objectType.GetGenericArguments().[0] 
                    let nestedValue = jToken.ToObject(nestedType, serializer)
                    FSharpValue.MakeUnion(caseSome, [| nestedValue |])
                 
        // Discriminated union
        else
            // There are three types of union cases:
            //      | Case1 | Case2 of 'a | Case3 of 'a1 * 'a2 ... * 'an
            // Those are respectively serialized to Json as
            //    "Case1"
            //    { "Case2" : value }
            //    { "Case3" : [v1, v2, ... vn] }
                
            // Load JObject from stream
            let jToken = Linq.JToken.Load(reader)
                    
            if isNull jToken then
                null

            // Type1: field-less union case
            elif jToken.Type = Linq.JTokenType.String then
                let caseName = jToken.ToString()
                let matchingCase =
                    cases
                    |> Array.tryFind 
                        (fun case -> case.Name.Equals(caseName, System.StringComparison.InvariantCultureIgnoreCase)
                                        && (case.GetFields() |> Array.isEmpty))
                match matchingCase with
                | Some case -> FSharpValue.MakeUnion(case,[||])
                | None ->
                    failwithf "Cannot parse DU field-less value: %O. Expected names: %O" caseName (System.String.Join(", ", cases |> Seq.map(fun c->c.Name)))

            // Type 2 or 3: Case with fields
            elif jToken.Type = Linq.JTokenType.Object then
                let jObject = jToken :?> Linq.JObject
                let jObjectProperties = jObject.Properties()
                if Seq.length jObjectProperties <> 1 then
                    failwith "Incorrect Json format for discriminated union. A DU value with fields must be serialized to a Json object with a single Json attribute"

                let caseProperty = jObjectProperties |> Seq.head
                /// Lookup the DU case by name
                let matchingCase =
                    cases
                    |> Seq.tryFind (fun case -> case.Name.Equals(caseProperty.Name, System.StringComparison.InvariantCultureIgnoreCase))

                match matchingCase with
                | None ->
                    failwithf "Case %s with fields does not exist for discriminated union %s" caseProperty.Name objectType.Name

                // Type 2: A union case with a single field: Case2 of 'a
                | Some case when case.GetFields().Length = 1 ->
                    let fieldType = case.GetFields().[0].PropertyType
                    let field = caseProperty.Value.ToObject(fieldType, serializer)
                    FSharpValue.MakeUnion(case, [|field|])

                // Type 3: A union case with more than one field: Case3 of 'a1 * 'a2 ... * 'an
                | Some case -> 
                    // Here there could be an ambiguity: 
                    // the Json values are either the fields of the case
                    // or if the array is Use target type to resolve ambiguity
                    let fields =
                        case.GetFields()
                        |> Seq.zip caseProperty.Value
                        |> Seq.map (fun (v,t) -> v.ToObject(t.PropertyType, serializer))
                        |> Seq.toArray
                    FSharpValue.MakeUnion(case, fields)
            else
                failwithf "Unexpected Json token type %O: %O" jToken.Type jToken

/// Serialization settings for our custom Json converter
type UnionSettings =
    static member settings =
        let converter = DiscriminatedUnionJsonConverter()
        let s = JsonSerializerSettings(NullValueHandling = NullValueHandling.Ignore)
        s.Converters.Add(converter)
        s

/// Our Json serializer
type Union = With<UnionSettings>