namespace Microsoft.FSharpLu.Json

open Newtonsoft.Json
open Newtonsoft.Json.Serialization
open Microsoft.FSharp.Reflection  

module private ConverterHelpers =
    let inline stringEq (a:string) (b:string) =
        a.Equals(b, System.StringComparison.InvariantCultureIgnoreCase)

    let inline isOptionType (t:System.Type) =
       t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>

    let inline toCamel (name:string) =
        if System.Char.IsLower (name, 0) then name
        else string(System.Char.ToLower name.[0]) + name.Substring(1)

open ConverterHelpers

/// Serializers for F# discriminated unions improving upon the stock implementation by JSon.Net
/// The default formatting used by Json.Net to serialize F# discriminated unions
/// and Option types is too verbose. This module implements a more succinct serialization
/// for those data types.
type CompactUnionJsonConverter() =
    inherit Newtonsoft.Json.JsonConverter()

    let SomeFieldIdentifier = "Some"

    /// Determine if a given type has a field named 'Some' which would cause
    /// ambiguity if nested under an option type without being boxed
    let hasFieldNamedSome (t:System.Type) =
        isOptionType t // the option type itself has a 'Some' field
        || (FSharpType.IsRecord t && FSharpType.GetRecordFields t |> Seq.exists (fun r -> stringEq r.Name SomeFieldIdentifier))
        || (FSharpType.IsUnion t && FSharpType.GetUnionCases t |> Seq.exists (fun r -> stringEq r.Name SomeFieldIdentifier))

    override __.CanConvert(objectType:System.Type) =
        // Include F# discriminated unions
        FSharpType.IsUnion objectType
        // and exclude the standard FSharp lists (which are implemented as discriminated unions)
        && not (objectType.IsGenericType && objectType.GetGenericTypeDefinition() = typedefof<_ list>)

    override __.WriteJson(writer:JsonWriter, value:obj, serializer:JsonSerializer) =
        let convertName = 
            match serializer.ContractResolver with 
            | :? CamelCasePropertyNamesContractResolver -> toCamel
            | _ -> id
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
                    writer.WritePropertyName(convertName SomeFieldIdentifier)
                    writer.WriteNull()
                    writer.WriteEndObject()
                // Some v with v <> null && v <> None
                else
                    // Is it nesting another option: `(e.g., "Some (Some ... Some ( ... )))"`
                    // or any other type with a field named 'Some'?
                    if hasFieldNamedSome innerType then
                        // Preserved the nested structure through boxing
                        writer.WriteStartObject()
                        writer.WritePropertyName(convertName SomeFieldIdentifier)
                        serializer.Serialize(writer, innerValue)
                        writer.WriteEndObject()
                    else
                        // Type is option<'a> where 'a does not have a field named 'Some
                        // (and therfore in particular is NOT an option type itself)
                        // => we can simplify the Json by omitting the `Some` boxing
                        // and serializing the nested object directly
                        serializer.Serialize(writer, innerValue)

        // Discriminated union
        else
            let case, fields = FSharpValue.GetUnionFields(value, t)
            match fields with
            // Field-less union case
            | [||] ->
                writer.WriteValue(convertName (sprintf "%A" value))
            // Case with single field
            | [|onefield|] ->
                writer.WriteStartObject()
                writer.WritePropertyName(convertName case.Name)
                serializer.Serialize(writer, onefield)
                writer.WriteEndObject()
            // Case with list of fields
            | _ ->
                writer.WriteStartObject()
                writer.WritePropertyName(convertName case.Name)
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

            // Json that is not null must map to `Some _`
            else
                let nestedType = objectType.GetGenericArguments().[0]

                // Try to retrieve the 'Some' attribute:
                // if the specified Json an object of the form `{ "Some" = token }`
                // then return `Some token`, otherwise returns `None`.
                let tryGetSomeAttributeValue (jToken:Linq.JToken) =
                    if jToken.Type = Linq.JTokenType.Object then
                        let jObject = jToken :?> Linq.JObject
                        match jObject.TryGetValue (SomeFieldIdentifier, System.StringComparison.InvariantCultureIgnoreCase) with
                        | true, token -> Some token
                        | false, _ -> None
                    else
                        None

                let nestedValue =
                    match tryGetSomeAttributeValue jToken with
                    | Some someAttributeValue when someAttributeValue.Type = Linq.JTokenType.Null ->
                        // The Json object is { "Some" : null } for type option<'a>
                        // where 'a is nullable => deserialized to `Some null`
                        null

                    | Some someAttributeValue when hasFieldNamedSome nestedType ->
                        // Case of Json { "Some" : <obj> } where <obj> is not null
                        // => we just deserialize the nested object recursively
                        someAttributeValue.ToObject(nestedType, serializer)

                    | Some someAttributeValue ->
                        failwithf "Unexpected 'Some' Json attribute. Attribute value: %O" someAttributeValue

                    | None when hasFieldNamedSome nestedType ->
                        failwith "Types with a field named 'Some' and nested under an option type must be boxed under a 'Some' attribute when serialized to Json."

                    | None ->
                        // type is option<'a> where 'a is not an option type and not a
                        // type that would be serialized as a Json object.
                        // i.e. 'a is either a base Json type (e.g. integer or string) or
                        // a Json array.
                        // This means that the Json is not boxed under the `Some` attribute and we can therefore
                        // deserialize the object of type 'a directly without performing any unboxing.
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
                    |> Array.tryFind (fun case -> stringEq case.Name caseName && (case.GetFields() |> Array.isEmpty))
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
                    |> Seq.tryFind (fun case -> stringEq case.Name caseProperty.Name)

                match matchingCase with
                | None ->
                    failwithf "Case with fields '%s' does not exist for discriminated union %s" caseProperty.Name objectType.Name

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

/// Compact serializer
module Compact =
    open System.Runtime.CompilerServices

    module Internal =
        /// Serialization settings for our compact Json converter
        type Settings =
            static member settings =
                let s = JsonSerializerSettings(
                            NullValueHandling = NullValueHandling.Ignore,

                            // Strict deserialization (MissingMemberHandling) is not technically needed for
                            // compact serialization but because it avoids certain ambiguities it guarantees
                            // that the deserialization coincides with the default Json deserialization
                            // ('coincides' meaning 'if the deserialization succeeds they both return the same object')
                            // Subsequently this allows us to very easily define the BackwardCompatible serializer which
                            // deserializes Json in both Compact and Default format.
                            MissingMemberHandling = MissingMemberHandling.Error
                    )
                s.Converters.Add(CompactUnionJsonConverter())
                s

            static member formatting = Formatting.Indented

    type private S = With<Internal.Settings>

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
