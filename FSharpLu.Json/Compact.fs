namespace Microsoft.FSharpLu.Json

open Newtonsoft.Json
open Microsoft.FSharp.Reflection

/// Serializers for F# discriminated unions improving upon the stock implementation by JSon.Net
/// The default formatting used by Json.Net to serialize F# discriminated unions
/// and Option types is too verbose. This module implements a more succinct serialization
/// for those data types.
type CompactUnionJsonConverter() =
    inherit Newtonsoft.Json.JsonConverter()

    let isOptionType (t:System.Type) = 
        t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>
        
    override __.CanConvert(objectType:System.Type) = 
        // Include F# discriminated unions
        FSharpType.IsUnion objectType
        // and exclude the standard FSharp lists (which are implemented as discriminated unions)
        && not (objectType.IsGenericType && objectType.GetGenericTypeDefinition() = typedefof<_ list>)
           
    override __.WriteJson(writer:JsonWriter, value:obj, serializer:JsonSerializer) =
        let t = value.GetType()
        // Option type?
        if isOptionType t then
            let cases = FSharpType.GetUnionCases(t)
            let none, some = cases.[0], cases.[1]
    
            let case, fields = FSharpValue.GetUnionFields(value, t)
            
            if case = none then
                () // None is serialized as just null

            // Some _
            else
                // Handle cases `Some None` and `Some null`
                let innerType = some.GetFields().[0].PropertyType
                let innerValue = fields.[0]
                if isNull innerValue then
                    writer.WriteStartObject()
                    writer.WritePropertyName("Some")
                    writer.WriteNull()
                    writer.WriteEndObject()
                // Some v with v <> null && v <> None
                else
                    // Is it a nested option: `(e.g., "Some (Some ... Some ( ... )))"`?
                    if isOptionType <| innerType then
                        // Preserved the nested structure through boxing
                        writer.WriteStartObject()
                        writer.WritePropertyName("Some")
                        serializer.Serialize(writer, innerValue)
                        writer.WriteEndObject()
                    else
                        // Type is option<'a> where 'a is NOT an option type itself
                        // => we can simplify the Json by omitting the `Some` boxing
                        // and serializing the nested object directly
                        serializer.Serialize(writer, innerValue)

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
        if isOptionType objectType then
            let caseNone, caseSome = cases.[0], cases.[1]
            let jToken = Linq.JToken.ReadFrom(reader)
            // Json Null maps to `None`
            if jToken.Type = Linq.JTokenType.Null then
                FSharpValue.MakeUnion(caseNone, [||])
            
            // Json that is not null maps to `Some _`
            else
                let nestedType = objectType.GetGenericArguments().[0]
                let parseBox (jToken:Linq.JToken) =
                    if jToken.Type <> Linq.JTokenType.Object then
                        failwith "Nested option types must be serialized as boxed Json objects"

                    let jObject = jToken :?> Linq.JObject
                    match jObject.TryGetValue("Some") with
                    | false, _ ->
                        failwith "Nested option types must be serialized as boxed Json objects with attribute 'Some'."
                    | true, unboxedValue ->
                        if unboxedValue.Type = Linq.JTokenType.Null then
                            // Case of Json { "Some" : null } for type option<'a> where 'a is nullable
                            // => deserialized to `Some null`
                            null
                        else
                            // Case of Json { "Some" : <obj> } where <obj> is not null
                            // => we just deserialize the nested object recursively
                            unboxedValue.ToObject(nestedType, serializer)

                let nestedValue = 
                    // Is the nested type an option type itself?
                    // or is the Json to be deserialized an object Json?
                    if isOptionType nestedType || jToken.Type = Linq.JTokenType.Object then 
                        // Nested option type are always boxed in Json to prevent any ambguity
                        parseBox jToken
                    else
                        // type is option<'a> where 'a is not an option type
                        // => we can deserialize the object of type 'a directly without unboxing
                        jToken.ToObject(nestedType, serializer)
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

/// Serialization settings for our custom compact Json converter
type CompactSettings =
    static member settings =
        let s = JsonSerializerSettings(NullValueHandling = NullValueHandling.Ignore)
        s.Converters.Add(CompactUnionJsonConverter())
        s

/// Our Json serializer
type Compact = With<CompactSettings>
