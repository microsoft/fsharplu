namespace Microsoft.FSharpLu.Json

open Newtonsoft.Json
open Newtonsoft.Json.Serialization
open Microsoft.FSharp.Reflection
open System.Reflection
open System.Collections.Concurrent
open System

module private ConverterHelpers =
    let inline stringEq (a:string) (b:string) =
        a.Equals(b, System.StringComparison.OrdinalIgnoreCase)

    let inline isOptionType (t:System.Type) =
       t.GetTypeInfo().IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>

    let inline isTupleType (t:System.Type) =
       FSharpType.IsTuple t

    let inline isTupleItemProperty (prop:System.Reflection.PropertyInfo) =
        // Item1, Item2, etc. excluding Items[n] indexer. Valid only on tuple types.
        (prop.Name.StartsWith("Item") || prop.Name = "Rest") && (Seq.isEmpty <| prop.GetIndexParameters())

module Memorised = 
    let inline memorise (f: 'key -> 'result) =
        let d = ConcurrentDictionary<'key, 'result>()
        fun key -> d.GetOrAdd(key, f)

    let getUnionCaseFields = memorise FSharpValue.PreComputeUnionReader
    let getUnionTag = memorise FSharpValue.PreComputeUnionTagReader
    let getUnionCasesByTag = memorise (fun t -> FSharpType.GetUnionCases(t) |> Array.map (fun x -> x.Tag, x) |> dict)

    let getUnionCasesMemorised = memorise FSharpType.GetUnionCases

    let constructUnionCase = memorise FSharpValue.PreComputeUnionConstructor

    let getUnionCaseProperyInfoFields = memorise (fun (case: UnionCaseInfo) -> case.GetFields())

    let findNoFieldsMatchingUnionCaseByNameAndType  = 
        memorise <| fun (objectType, caseName) -> 
            let cases = getUnionCasesMemorised objectType
            cases |> Array.tryFind (fun case -> ConverterHelpers.stringEq case.Name caseName && (getUnionCaseProperyInfoFields case |> Array.isEmpty))

    let findMatchingUnionCaseByNameAndType  = 
        memorise <| fun (objectType, caseName) -> 
            let cases = getUnionCasesMemorised objectType
            cases |> Array.tryFind (fun case -> ConverterHelpers.stringEq case.Name caseName)

    let getUnionTagOfValue v =
        let t = v.GetType()
        getUnionTag t v

    let inline getUnionFields v =
        let cases = getUnionCasesByTag (v.GetType())
        let tag = getUnionTagOfValue v
        let case = cases.[tag]
        let unionReader = getUnionCaseFields case
        (case, unionReader v)

    let SomeFieldIdentifier = "Some"

    /// Determine if a given type has a field named 'Some' which would cause
    /// ambiguity if nested under an option type without being boxed
    let hasFieldNamedSome =
        memorise
            (fun (t:System.Type) ->
                ConverterHelpers.isOptionType t // the option type itself has a 'Some' field
                || (FSharpType.IsRecord t && FSharpType.GetRecordFields t |> Seq.exists (fun r -> ConverterHelpers.stringEq r.Name SomeFieldIdentifier))
                || (FSharpType.IsUnion t && FSharpType.GetUnionCases t |> Seq.exists (fun r -> ConverterHelpers.stringEq r.Name SomeFieldIdentifier)))

open ConverterHelpers
open Memorised

/// Serializers for F# discriminated unions improving upon the stock implementation by JSon.Net
/// The default formatting used by Json.Net to serialize F# discriminated unions
/// and Option types is too verbose. This module implements a more succinct serialization
/// for those data types.
type CompactUnionJsonConverter(?tupleAsHeterogeneousArray:bool, ?usePropertyFormatterForValues:bool) =
    inherit Newtonsoft.Json.JsonConverter()

    ///  By default tuples are serialized as heterogeneous arrays.
    let tupleAsHeterogeneousArray = defaultArg tupleAsHeterogeneousArray true   
    ///  By default formatting is used for values
    let usePropertyFormatterForValues = defaultArg usePropertyFormatterForValues true
    let canConvertMemorised =
        memorise
            (fun objectType ->
                (   // Include F# discriminated unions
                    FSharpType.IsUnion objectType
                    // and exclude the standard FSharp lists (which are implemented as discriminated unions)
                    && not (objectType.GetTypeInfo().IsGenericType && objectType.GetGenericTypeDefinition() = typedefof<_ list>)
                )
                // include tuples
                || tupleAsHeterogeneousArray && FSharpType.IsTuple objectType
            )

    override __.CanConvert(objectType:System.Type) = canConvertMemorised objectType

    override __.WriteJson(writer:JsonWriter, value:obj, serializer:JsonSerializer) =
        let t = value.GetType()

        let convertName =
            match serializer.ContractResolver with
            | :? DefaultContractResolver as resolver -> resolver.GetResolvedPropertyName
            | _ -> id

        // Option type?
        if isOptionType t then
            let cases = getUnionCasesMemorised t
            let none, some = cases.[0], cases.[1]

            let case, fields = getUnionFields value

            if case = none then
                () // None is serialized as just null

            // Some _
            else
                // Handle cases `Some None` and `Some null`
                let innerType = (getUnionCaseProperyInfoFields some).[0].PropertyType
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
        // Tuple
        else if tupleAsHeterogeneousArray && isTupleType t then
            let v = FSharpValue.GetTupleFields value
            serializer.Serialize(writer, v)
        // Discriminated union
        else
            let case, fields = getUnionFields value

            match fields with
            // Field-less union case
            | [||] when usePropertyFormatterForValues -> writer.WriteValue(convertName case.Name)
            | [||] when not usePropertyFormatterForValues -> writer.WriteValue(case.Name)
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
        let failreadwith s = raise (JsonReaderException s)
        let failreadwithf format = Printf.ksprintf failreadwith format
        // Option type?
        if isOptionType objectType then
            let cases = getUnionCasesMemorised objectType
            let caseNone, caseSome = cases.[0], cases.[1]
            let jToken = Linq.JToken.ReadFrom(reader)

            // Json Null maps to `None`
            if jToken.Type = Linq.JTokenType.Null then
                FSharpValue.MakeUnion(caseNone, [||])

            // Json that is not null must map to `Some _`
            else
                let nestedType = objectType.GetTypeInfo().GetGenericArguments().[0]

                // Try to retrieve the 'Some' attribute:
                // if the specified Json an object of the form `{ "Some" = token }`
                // then return `Some token`, otherwise returns `None`.
                let tryGetSomeAttributeValue (jToken:Linq.JToken) =
                    if jToken.Type = Linq.JTokenType.Object then
                        let jObject = jToken :?> Linq.JObject
                        match jObject.TryGetValue (SomeFieldIdentifier, System.StringComparison.OrdinalIgnoreCase) with
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
                        failreadwithf "Unexpected 'Some' Json attribute. Attribute value: %O" someAttributeValue

                    | None when hasFieldNamedSome nestedType ->
                        failreadwith "Types with a field named 'Some' and nested under an option type must be boxed under a 'Some' attribute when serialized to Json."

                    | None ->
                        // type is option<'a> where 'a is not an option type and not a
                        // type that would be serialized as a Json object.
                        // i.e. 'a is either a base Json type (e.g. integer or string) or
                        // a Json array.
                        // This means that the Json is not boxed under the `Some` attribute and we can therefore
                        // deserialize the object of type 'a directly without performing any unboxing.
                        jToken.ToObject(nestedType, serializer)

                constructUnionCase caseSome [| nestedValue |]

        // Tuple type?
        else if tupleAsHeterogeneousArray && isTupleType objectType then
            match reader.TokenType with
            // JSON is an object with one field per element of the tuple
            | JsonToken.StartObject ->
                // backward-compat with legacy tuple serialization:
                // if reader.TokenType is StartObject then we should expecte legacy JSON format for tuples
                let jToken = Linq.JObject.Load(reader)
                if isNull jToken then
                    failreadwithf "Expecting a legacy tuple, got null"
                else
                    let readProperty (prop: PropertyInfo) =
                        match jToken.TryGetValue(prop.Name) with
                        | false,_ ->
                            failreadwithf "Cannot parse legacy tuple value: %O. Missing property: %s" jToken prop.Name
                        | true, jsonProp ->
                            jsonProp.ToObject(prop.PropertyType, serializer)
                    let tupleValues =
                        objectType.GetTypeInfo().DeclaredProperties
                        |> Seq.filter isTupleItemProperty
                        |> Seq.map readProperty
                        |> Array.ofSeq
                    System.Activator.CreateInstance(objectType, tupleValues)

            // JSON is an heterogeneous array
            | JsonToken.StartArray ->
                let tupleType = objectType
                let elementTypes = FSharpType.GetTupleElements(tupleType)

                let readElement elementType =
                    let more = reader.Read()
                    if not more then
                        failreadwith "Missing array element in deserialized JSON"

                    let jToken = Linq.JToken.ReadFrom(reader)
                    jToken.ToObject(elementType, serializer)

                let deserializedAsUntypedArray =
                    elementTypes
                    |> Array.map readElement

                let more = reader.Read()
                if reader.TokenType <> JsonToken.EndArray then
                    failreadwith "Expecting end of array token in deserialized JSON"

                FSharpValue.MakeTuple(deserializedAsUntypedArray, tupleType)
            | _ ->
                failreadwithf "Expecting a JSON array or a JSON object, got something else: %A" reader.TokenType
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
                let matchingCase = findNoFieldsMatchingUnionCaseByNameAndType (objectType, caseName)
                match matchingCase with
                | Some case -> constructUnionCase case [||]
                | None ->
                    let cases = getUnionCasesMemorised objectType
                    failreadwithf "Cannot parse DU field-less value: %O. Expected names: %O" caseName (System.String.Join(", ", cases |> Seq.map(fun c->c.Name)))

            // Type 2 or 3: Case with fields
            elif jToken.Type = Linq.JTokenType.Object then
                let jObject = jToken :?> Linq.JObject
                let jObjectProperties = jObject.Properties()
                if Seq.length jObjectProperties <> 1 then
                    failreadwith "Incorrect Json format for discriminated union. A DU value with fields must be serialized to a Json object with a single Json attribute"

                let caseProperty = jObjectProperties |> Seq.head
                /// Lookup the DU case by name
                let matchingCase = findMatchingUnionCaseByNameAndType (objectType, caseProperty.Name)
                
                match matchingCase with
                | None ->
                    failreadwithf "Case with fields '%s' does not exist for discriminated union %s" caseProperty.Name objectType.Name
                | Some case  -> 
                    let propertyInfosForCase = getUnionCaseProperyInfoFields case
                    // Type 2: A union case with a single field: Case2 of 'a
                    if propertyInfosForCase.Length = 1 then
                        let fieldType = propertyInfosForCase.[0].PropertyType
                        let field = caseProperty.Value.ToObject(fieldType, serializer)
                        constructUnionCase case [|field|]
                    // Type 3: A union case with more than one field: Case3 of 'a1 * 'a2 ... * 'an
                    else                    
                        // Here there could be an ambiguity:
                        // the Json values are either the fields of the case
                        // or if the array is Use target type to resolve ambiguity
                        let fields =
                            propertyInfosForCase
                            |> Seq.zip caseProperty.Value
                            |> Seq.map (fun (v,t) -> v.ToObject(t.PropertyType, serializer))
                            |> Seq.toArray
                        constructUnionCase case fields
            else
                failreadwithf "Unexpected Json token type %O: %O" jToken.Type jToken

/// Compact serializer
module Compact =
    open System.Runtime.CompilerServices

    /// Compact serialization where tuples are serialized as heterogeneous arrays
    type TupleAsArraySettings =
        static member formatting = Formatting.Indented
        static member settings =
            JsonSerializerSettings(
                NullValueHandling = NullValueHandling.Ignore,

                // MissingMemberHandling is not technically needed for
                // compact serialization but it avoids certain ambiguities
                // that guarantee that deserialization coincides with the 
                // default Json.Net deserialization.
                // (where 'coincides' means 'if the deserialization succeeds they both return the same object')
                // This allows us to easily define the BackwardCompatible 
                // serializer (that handles both Compact and Default Json format) by reusing 
                // the Compact deserializer.
                MissingMemberHandling = MissingMemberHandling.Error,
                Converters = [| CompactUnionJsonConverter(true) |]
            )

    type private S = With<TupleAsArraySettings>

    /// Serialize an object to Json with the specified converter
    [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
    let inline serialize< ^T> x = S.serialize x
    /// Serialize an object to Json with the specified converter and save the result to a file
    [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
    let inline serializeToFile< ^T> file obj = S.serializeToFile file obj
    /// Serialize an object to Json with the specified converter and write the result to a stream
    [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
    let inline serializeToStream< ^T> stream obj = S.serializeToStream stream obj
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

    /// Legacy compact serialization where tuples are serialized as objects instead of arrays
    module Legacy =
        /// Compact serialization where tuples are serialized as JSON objects
        type TupleAsObjectSettings =
            static member formatting = Formatting.Indented
            static member settings =
                JsonSerializerSettings(
                    NullValueHandling = NullValueHandling.Ignore,
                    MissingMemberHandling = MissingMemberHandling.Error,
                    Converters = [| CompactUnionJsonConverter(false) |]
                )

        type private S = With<TupleAsObjectSettings>

        /// Serialize an object to Json with the specified converter
        [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
        let inline serialize< ^T> x = S.serialize x
        /// Serialize an object to Json with the specified converter and save the result to a file
        [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
        let inline serializeToFile< ^T> file obj = S.serializeToFile file obj
        /// Serialize an object to Json with the specified converter and write the result to a stream
        [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
        let inline serializeToStream< ^T> stream obj = S.serializeToStream stream obj
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

    /// Compact serializer where desearilization requires presence of all properties 
    /// expect optional ones (of type option<_>)
    module Strict =

        /// A contract resolver that requires presence of all properties
        /// that are not of type option<_>
        type RequireNonOptionalPropertiesContractResolver() =
            inherit Newtonsoft.Json.Serialization.CamelCasePropertyNamesContractResolver()
            override __.CreateProperty(_member, memberSerialization) =
                let property = base.CreateProperty(_member, memberSerialization)
                let isRequired = not (property.PropertyType.GetTypeInfo().IsGenericType 
                                   && property.PropertyType.GetGenericTypeDefinition() = typedefof<option<_>>)
                if isRequired then 
                    property.Required <- Required.Always
                    property.NullValueHandling <- System.Nullable NullValueHandling.Ignore
                property

        /// Compact serialization where tuples are serialized as JSON objects
        type CompactStrictSettings =
            static member formatting = Formatting.Indented
            static member settings =
                JsonSerializerSettings
                    (
                        ContractResolver = RequireNonOptionalPropertiesContractResolver(),
                        Converters = [|CompactUnionJsonConverter(true)|]
                    )

        type private S = With<CompactStrictSettings>

        /// Serialize an object to Json with the specified converter
        [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
        let inline serialize< ^T> x = S.serialize x
        /// Serialize an object to Json with the specified converter and save the result to a file
        [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
        let inline serializeToFile< ^T> file obj = S.serializeToFile file obj
        /// Serialize an object to Json with the specified converter and write the result to a stream
        [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
        let inline serializeToStream< ^T> stream obj = S.serializeToStream stream obj
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
