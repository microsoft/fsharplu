module Microsoft.FSharpLu.Json.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsCheck
open Microsoft.FSharpLu.Json
open Newtonsoft.Json

type WithFields = SomeField of int * int
type SimpleDu = Foo | FooBar | Bar
type ComplexDu = ComplexDu of WithFields | SimpleDU | AString of string
type 'a RecursiveList = RecListLeaf of 'a | RecListCons of 'a RecursiveList
type OptionOfBase = int option
type OptionOfDu = SimpleDu option
type Color = Red | Blue
type Shape = Circle of int * int | Rectangle
type 'a Tree = Leaf of 'a | Node of 'a Tree * 'a Tree
type 'a Test = Case1 | Case2 of int | Case3 of int * string * 'a
type MapType = Map<string,Color>
type 'a NestedOptions = 'a option option option option
type ConsecutiveUppercaseRecord = { BAR : int ; BAZNumber : int }
type ConsecutiveUppercaseDu = FOO | FOOWithRecord of ConsecutiveUppercaseRecord

type 'a Wrapper = { WrappedField : 'a }
type NestedStructure = { subField : int }
type NestedOptionStructure = { field : NestedStructure option }

type SomeTupleType = int * string list * int * int64 * OptionOfDu * Color * int Tree  

/// Test cases for possible ambiguity between option types and other DU or records with a 'Some' field.
module SomeAmbiguity =
    type 'a RecordWithFieldNamedSome = { Some : 'a }
    type DUWithFieldlessCaseNamedSome = Some of string | Bla
    type DUWithCaseWithFieldNamedSome = Some | Bla
    type 'a Ambiguous1 = 'a RecordWithFieldNamedSome option
    type Ambiguous2 = DUWithFieldlessCaseNamedSome option
    type Ambiguous3 = DUWithCaseWithFieldNamedSome option

let inline defaultSerialize< ^T> (x: ^T) = Compact.serialize< ^T> x

let inline reciprocal< ^T when ^T:equality> (serialize: ^T->string) (deserialize: string-> ^T) (x: ^T) =
    // theoretically one round trip is sufficient; we perform
    // two round trips here to test for possible side-effects
    x |> serialize |> deserialize |> serialize |> deserialize = x

let inline canBeSerialized< ^T when ^T:equality> (serialize: ^T->string) (deserialize: string-> ^T) (x: ^T) =
    serialize x |> printfn "%A"

let inline areReciprocal< ^T when ^T:equality> (serialize: ^T->string) (deserialize: string-> ^T) (x: ^T) =
    let s = x |> serialize
    let sds = s |> deserialize |> serialize
    Assert.AreEqual(s, sds, sprintf "Inconsistent serialization: 1st call: <%s> 2nd call <%s>. Type %A" s sds (typeof< ^T>))
    let sdsd = sds |> deserialize
    Assert.AreEqual(sdsd, x, sprintf "Did not get the same object back: <%A> gave back <%A> for type %A" x sdsd (typeof< ^T>))

/// Check that given object serializes to the specified Json string (using default serializer)
let inline serializedAs json o =
    let s = defaultSerialize o
    Assert.AreEqual(json, s, sprintf "Object was not serialized to the expected format")

/// Check that deserialization coincides with NewtonSoft's default serializer.
/// That is: when the Json is deserializable by both deserializers Union and Default
/// they produce the same output object.
let inline coincidesWithDefault< ^T when ^T:equality> (x: ^T) =
    let deserializationMustCoincide json =
        match Default.tryDeserialize< ^T> json, Compact.tryDeserialize< ^T> json with
        | Choice2Of2 error1, Choice2Of2 error2->
            Assert.IsTrue(true, "Json not parseable by either deserializer: no ambiguity")
        | Choice1Of2 _, Choice2Of2 error
        | Choice2Of2 error, Choice1Of2 _ ->
            Assert.IsTrue(true, "Json parseable by exactly one deserializer: no ambiguity")
        | Choice1Of2 v1, Choice1Of2 v2 when v1 <> v2 ->
            Assert.Fail(sprintf "Deserializers do not coincide: %A <> %A" v1 v2)
        | Choice1Of2 v1, Choice1Of2 v2 ->
            Assert.IsTrue(true)
    x |> Default.serialize |> deserializationMustCoincide
    x |> Compact.serialize |> deserializationMustCoincide

/// Check that output format of Default Json.Net serializer can be parsed by
/// the BackwardCompatible deserializer
let inline backwardCompatibleWithDefault< ^T when ^T:equality> (x: ^T) =
    let json = x |> Default.serialize
    let o1 = json |> Default.deserialize : ^T
    let o2 = json |> BackwardCompatible.deserialize : ^T
    Assert.AreEqual(o1, o2,
        sprintf "BackwardCompatible should coincide with Json.Net when deserializing default Json format. %A <> %A" o1 o2)

type CamelCaseSettings =
    static member settings =
        let s =
            JsonSerializerSettings(
                NullValueHandling = NullValueHandling.Ignore,
                MissingMemberHandling = MissingMemberHandling.Error,
                ContractResolver = Serialization.CamelCasePropertyNamesContractResolver())
        s.Converters.Add(CompactUnionJsonConverter(true))
        s
    static member formatting = Formatting.None

type CamelCaseSerializer = With<CamelCaseSettings>

type ReciprocalityCompact () =
    static member x1 = reciprocal<ComplexDu> Compact.serialize Compact.deserialize
    static member x2 = reciprocal<ComplexDu RecursiveList> Compact.serialize Compact.deserialize
    static member x3 = reciprocal<WithFields> Compact.serialize Compact.deserialize
    static member x4 = reciprocal<SimpleDu> Compact.serialize Compact.deserialize
    static member x5 = reciprocal<ComplexDu> Compact.serialize Compact.deserialize
    static member x6 = reciprocal<OptionOfBase> Compact.serialize Compact.deserialize
    static member x7 = reciprocal<OptionOfDu> Compact.serialize Compact.deserialize
    static member x8 = reciprocal<Color> Compact.serialize Compact.deserialize
    static member x9 = reciprocal<Shape> Compact.serialize Compact.deserialize
    static member x10 = reciprocal<int Tree> Compact.serialize Compact.deserialize
    static member x11 = reciprocal<int Tree Test> Compact.serialize Compact.deserialize
    static member x12 = reciprocal<int Test> Compact.serialize Compact.deserialize
    static member x13 = reciprocal<int list Tree> Compact.serialize Compact.deserialize
    static member x14 = reciprocal<string NestedOptions> Compact.serialize Compact.deserialize
    static member x15 = reciprocal<string> Compact.serialize Compact.deserialize
    static member x16 = reciprocal<string option> Compact.serialize Compact.deserialize
    static member x17 = reciprocal<string option option> Compact.serialize Compact.deserialize
    static member x18 = reciprocal<string option option option option> Compact.serialize Compact.deserialize
    static member x19 = reciprocal<int NestedOptions> Compact.serialize Compact.deserialize
    static member x20 = reciprocal<SomeAmbiguity.Ambiguous1<string>> Compact.serialize Compact.deserialize
    static member x21 = reciprocal<SomeAmbiguity.Ambiguous1<SimpleDu>> Compact.serialize Compact.deserialize
    static member x22 = reciprocal<NestedOptionStructure> Compact.serialize Compact.deserialize
    static member x23 = reciprocal<SomeAmbiguity.Ambiguous2> Compact.serialize Compact.deserialize
    static member x24 = reciprocal<SomeAmbiguity.Ambiguous3> Compact.serialize Compact.deserialize
    static member x25 = reciprocal<int list> Compact.serialize Compact.deserialize
    static member x26 = reciprocal<SomeTupleType> Compact.serialize Compact.deserialize

type ReciprocalityCamelCase () =
    static member x1 = reciprocal<ComplexDu> CamelCaseSerializer.serialize CamelCaseSerializer.deserialize
    static member x2 = reciprocal<ComplexDu RecursiveList> CamelCaseSerializer.serialize CamelCaseSerializer.deserialize
    static member x3 = reciprocal<WithFields> CamelCaseSerializer.serialize CamelCaseSerializer.deserialize
    static member x4 = reciprocal<SimpleDu> CamelCaseSerializer.serialize CamelCaseSerializer.deserialize
    static member x5 = reciprocal<ComplexDu> CamelCaseSerializer.serialize CamelCaseSerializer.deserialize
    static member x6 = reciprocal<OptionOfBase> CamelCaseSerializer.serialize CamelCaseSerializer.deserialize
    static member x7 = reciprocal<OptionOfDu> CamelCaseSerializer.serialize CamelCaseSerializer.deserialize
    static member x8 = reciprocal<Color> CamelCaseSerializer.serialize CamelCaseSerializer.deserialize
    static member x9 = reciprocal<Shape> CamelCaseSerializer.serialize CamelCaseSerializer.deserialize
    static member x10 = reciprocal<int Tree> CamelCaseSerializer.serialize CamelCaseSerializer.deserialize
    static member x11 = reciprocal<int Tree Test> CamelCaseSerializer.serialize CamelCaseSerializer.deserialize
    static member x12 = reciprocal<int Test> CamelCaseSerializer.serialize CamelCaseSerializer.deserialize
    static member x13 = reciprocal<int list Tree> CamelCaseSerializer.serialize CamelCaseSerializer.deserialize
    static member x14 = reciprocal<string NestedOptions> CamelCaseSerializer.serialize CamelCaseSerializer.deserialize
    static member x15 = reciprocal<string> CamelCaseSerializer.serialize CamelCaseSerializer.deserialize
    static member x16 = reciprocal<string option> CamelCaseSerializer.serialize CamelCaseSerializer.deserialize
    static member x17 = reciprocal<string option option> CamelCaseSerializer.serialize CamelCaseSerializer.deserialize
    static member x18 = reciprocal<string option option option option> CamelCaseSerializer.serialize CamelCaseSerializer.deserialize
    static member x19 = reciprocal<int NestedOptions> CamelCaseSerializer.serialize CamelCaseSerializer.deserialize
    static member x20 = reciprocal<SomeAmbiguity.Ambiguous1<string>> CamelCaseSerializer.serialize CamelCaseSerializer.deserialize
    static member x21 = reciprocal<SomeAmbiguity.Ambiguous1<SimpleDu>> CamelCaseSerializer.serialize CamelCaseSerializer.deserialize
    static member x22 = reciprocal<NestedOptionStructure> CamelCaseSerializer.serialize CamelCaseSerializer.deserialize
    static member x23 = reciprocal<SomeAmbiguity.Ambiguous2> CamelCaseSerializer.serialize CamelCaseSerializer.deserialize
    static member x24 = reciprocal<SomeAmbiguity.Ambiguous3> CamelCaseSerializer.serialize CamelCaseSerializer.deserialize
    static member x25 = reciprocal<int list> CamelCaseSerializer.serialize CamelCaseSerializer.deserialize
    static member x26 = reciprocal<SomeTupleType> CamelCaseSerializer.serialize CamelCaseSerializer.deserialize

type CoincidesWithJsonNetOnDeserialization () =
    static member x1 = coincidesWithDefault<ComplexDu>
    static member x2 = coincidesWithDefault<ComplexDu RecursiveList>
    static member x3 = coincidesWithDefault<WithFields>
    static member x4 = coincidesWithDefault<SimpleDu>
    static member x5 = coincidesWithDefault<ComplexDu>
    static member x6 = coincidesWithDefault<OptionOfBase>
    static member x7 = coincidesWithDefault<OptionOfDu>
    static member x8 = coincidesWithDefault<Color>
    static member x9 = coincidesWithDefault<Shape>
    static member x10 = coincidesWithDefault<int Tree>
    static member x11 = coincidesWithDefault<int Tree Test>
    static member x12 = coincidesWithDefault<int Test>
    static member x13 = coincidesWithDefault<int list Tree>
    static member x14 = coincidesWithDefault<string NestedOptions>
    static member x15 = coincidesWithDefault<string>
    static member x16 = coincidesWithDefault<string option>
    static member x17 = coincidesWithDefault<string option option>
    static member x18 = coincidesWithDefault<string option option option option>
    static member x19 = coincidesWithDefault<int NestedOptions>
    static member x20 = coincidesWithDefault<SomeAmbiguity.Ambiguous1<string>>
    static member x21 = coincidesWithDefault<SomeAmbiguity.Ambiguous1<SimpleDu>>
    static member x22 = coincidesWithDefault<NestedOptionStructure>
    static member x23 = coincidesWithDefault<SomeAmbiguity.Ambiguous2>
    static member x24 = coincidesWithDefault<SomeAmbiguity.Ambiguous3>
    static member x25 = coincidesWithDefault<int list>
    static member x26 = coincidesWithDefault<SomeTupleType>


type BackwardCompatibility () =
    static member x1 = backwardCompatibleWithDefault<ComplexDu>
    static member x2 = backwardCompatibleWithDefault<ComplexDu RecursiveList>
    static member x3 = backwardCompatibleWithDefault<WithFields>
    static member x4 = backwardCompatibleWithDefault<SimpleDu>
    static member x5 = backwardCompatibleWithDefault<ComplexDu>
    static member x6 = backwardCompatibleWithDefault<OptionOfBase>
    static member x7 = backwardCompatibleWithDefault<OptionOfDu>
    static member x8 = backwardCompatibleWithDefault<Color>
    static member x9 = backwardCompatibleWithDefault<Shape>
    static member x10 = backwardCompatibleWithDefault<int Tree>
    static member x11 = backwardCompatibleWithDefault<int Tree Test>
    static member x12 = backwardCompatibleWithDefault<int Test>
    static member x13 = backwardCompatibleWithDefault<int list Tree>
    static member x14 = backwardCompatibleWithDefault<string NestedOptions>
    static member x15 = backwardCompatibleWithDefault<string>
    static member x16 = backwardCompatibleWithDefault<string option>
    static member x17 = backwardCompatibleWithDefault<string option option>
    static member x18 = backwardCompatibleWithDefault<string option option option option>
    static member x19 = backwardCompatibleWithDefault<int NestedOptions>
    static member x20 = backwardCompatibleWithDefault<SomeAmbiguity.Ambiguous1<string>>
    static member x21 = backwardCompatibleWithDefault<SomeAmbiguity.Ambiguous1<SimpleDu>>
    static member x22 = backwardCompatibleWithDefault<NestedOptionStructure>
    static member x23 = backwardCompatibleWithDefault<SomeAmbiguity.Ambiguous2>
    static member x24 = backwardCompatibleWithDefault<SomeAmbiguity.Ambiguous3>
    static member x25 = backwardCompatibleWithDefault<int list>
    static member x26 = backwardCompatibleWithDefault<SomeTupleType>


let inline ``Run using all serializers``< ^T when ^T:equality> (test: (^T->string)->(string-> ^T)-> ^T->unit) (input: ^T) =
    [
        Compact.serialize, Compact.deserialize
        Default.serialize, Default.deserialize
        CamelCaseSerializer.serialize, CamelCaseSerializer.deserialize
        BackwardCompatible.serialize, BackwardCompatible.deserialize
    ] |> List.iter (fun (s, d) -> test s d input)

let inline testBackwardCompat< ^T when ^T:equality> (x: ^T) =
    let y =
        x
        |> Compact.Legacy.serialize< ^T>
        |> Compact.deserialize< ^T>

    Assert.AreEqual(x, y, "Tuple deserialization is not backward compatible!")

type ARecord = {
    id: System.Guid
    name: string
}

let inline assertStrictFailsToDeserialize< ^T> t = 
    let s = Compact.Strict.tryDeserialize t
    match s with 
    | Choice1Of2 _ -> Assert.IsTrue(true)
    | Choice2Of2 _ -> Assert.IsTrue(false)

/// Test for the compact serializer
[<TestClass>]
type JsonSerializerTests() =

    [<ClassInitialize>]
    static member Init(context : TestContext) = ()

    [<TestMethod>]
    [<TestCategory("FSharpLu.Json")>]
    member __.``Serialize field-less DU`` () =
        FooBar |> serializedAs "\"FooBar\""

    [<TestMethod>]
    [<TestCategory("FSharpLu.Json")>]
    member __.``Dont' touch Fsharp lists`` () =
        [1;2;3] |> serializedAs (Default.serialize [1;2;3])

    [<TestMethod>]
    [<TestCategory("FSharpLu.Json")>]
    member __.``Handles Some`` () =
        Some "test" |> serializedAs "\"test\""

    [<TestMethod>]
    [<TestCategory("FSharpLu..Json")>]
    member __.``Handles None`` () =
        None |> serializedAs "null"

    [<TestMethod>]
    [<TestCategory("FSharpLu.Json")>]
    member __.``Handles just the expected types``() =
        let conv = CompactUnionJsonConverter(true)
        Assert.IsTrue(conv.CanConvert(Color.Red.GetType()))
        Assert.IsTrue(conv.CanConvert(typeof<Color>))
        Assert.IsTrue(conv.CanConvert(typeof<_ option>))
        Assert.IsTrue(conv.CanConvert(typeof<_ Tree>))
        Assert.IsTrue(conv.CanConvert(typeof<Shape>))
        Assert.IsFalse(conv.CanConvert(typeof<_ list>))
        Assert.IsFalse(conv.CanConvert(typeof<Map<_,_>>))

    [<TestMethod>]
    [<TestCategory("FSharpLu.Json")>]
    member __.``Serialization does not raise exceptions for basic types``() =
        ``Run using all serializers`` canBeSerialized <| Red
        ``Run using all serializers`` canBeSerialized <| Blue
        ``Run using all serializers`` canBeSerialized <| Circle (8,99)
        ``Run using all serializers`` canBeSerialized <| Some 8
        ``Run using all serializers`` canBeSerialized <| Some (Circle(5,120))
        ``Run using all serializers`` canBeSerialized <| None
        ``Run using all serializers`` canBeSerialized <| Some (Node(Leaf 1,Leaf 9))
        ``Run using all serializers`` canBeSerialized <| Case1
        ``Run using all serializers`` canBeSerialized <| Case2 (3)
        ``Run using all serializers`` canBeSerialized <| Case3 (3,"s", "Foo")
        ``Run using all serializers`` canBeSerialized <| Circle (8,10)
        ``Run using all serializers`` canBeSerialized <| Leaf ["s";"s"]

    [<TestMethod>]
    [<TestCategory("FSharpLu.Json")>]
    member __.``Serialization and deserialization are reciprocal``() =
        ``Run using all serializers`` areReciprocal <| Some 8
        ``Run using all serializers`` areReciprocal <| Leaf "s"
        ``Run using all serializers`` areReciprocal  <| Leaf ["s";"s"]
        ``Run using all serializers`` areReciprocal  <| Leaf "s"
        ``Run using all serializers`` areReciprocal  <| Some 8
        ``Run using all serializers`` areReciprocal  <| Red
        ``Run using all serializers`` areReciprocal  <| Circle (8,10)
        ``Run using all serializers`` areReciprocal  <| Node((Leaf "s"),(Leaf "s"))
        ``Run using all serializers`` areReciprocal  <| Leaf ["s";"s"]
        ``Run using all serializers`` areReciprocal  <| Node((Leaf 1),(Leaf 9))
        ``Run using all serializers`` areReciprocal  <| Case1
        ``Run using all serializers`` areReciprocal  <| Case2 (3)
        ``Run using all serializers`` areReciprocal  <| Case3 (3,"s", "Foo")
        ``Run using all serializers`` areReciprocal  <| (["test", [3;3;4]] |> Map.ofSeq)
        ``Run using all serializers`` areReciprocal  <| ["test", [3;3;4]]
        ``Run using all serializers`` areReciprocal  <| Some (Some (Some None))
        ``Run using all serializers`` areReciprocal  <| Some (Some None)
        ``Run using all serializers`` areReciprocal  <| Some null
        ``Run using all serializers`` areReciprocal  <| Some None
        ``Run using all serializers`` areReciprocal  <| Some (Some (Some None))
        ``Run using all serializers`` areReciprocal  <| (1,2,3,4,5,6,7,8,9,10)

    [<TestMethod>]
    [<TestCategory("FSharpLu.Json")>]
    member __.``No ambiguity between records and Option type``() =
        ``Run using all serializers`` areReciprocal <| Some (Some (Some None))
        ``Run using all serializers`` areReciprocal <| { SomeAmbiguity.Some = null }
        ``Run using all serializers`` areReciprocal <| { SomeAmbiguity.Some = SimpleDu.Foo }
        ``Run using all serializers`` areReciprocal <| { SomeAmbiguity.Some = "test" }
        ``Run using all serializers`` areReciprocal <| { SomeAmbiguity.Some = 123 }
        ``Run using all serializers`` areReciprocal <| (Option.Some { SomeAmbiguity.Some = 345 })
        ``Run using all serializers`` areReciprocal <| (Option.Some <| SomeAmbiguity.DUWithFieldlessCaseNamedSome.Some "ambiguous")
        ``Run using all serializers`` areReciprocal <| (Option.Some { SomeAmbiguity.RecordWithFieldNamedSome.Some = 8 })
        ``Run using all serializers`` areReciprocal <| (Option.Some <| SomeAmbiguity.DUWithCaseWithFieldNamedSome.Some)

    [<TestMethod>]
    [<TestCategory("FSharpLu.Json.CamelCase")>]
    member __.``CamelCaseSerializer handles discriminated unions`` () =
        let du = ComplexDu <| SomeField (2, 3)
        let str = CamelCaseSerializer.serialize du
        Assert.AreEqual("""{"complexDu":{"someField":[2,3]}}""", str)

    [<TestMethod>]
    [<TestCategory("FSharpLu.Json.CamelCase")>]
    member __.``CamelCaseSerializer handles discriminated unions with consecutive uppercase characters`` () =
        let du = [ FOO ; FOOWithRecord { BAR=2; BAZNumber=3 } ]
        let str = CamelCaseSerializer.serialize du
        Assert.AreEqual("""["foo",{"fooWithRecord":{"bar":2,"bazNumber":3}}]""", str)

    [<TestMethod>]
    [<TestCategory("FSharpLu.Json.CamelCase")>]
    member __.``CamelCaseSerializer handles option type`` () =
        let du = { WrappedField = Some Red }
        let str = CamelCaseSerializer.serialize du
        Assert.AreEqual("""{"wrappedField":"red"}""", str)

    [<TestMethod>]
    [<TestCategory("FSharpLu.Json.Fuzzing")>]
    member __.``Fuzzing Reciprocal Compact`` () =
        Check.VerboseThrowOnFailureAll<ReciprocalityCompact>()

    [<TestMethod>]
    [<TestCategory("FSharpLu.Json.Fuzzing")>]
    member __.``Fuzzing Reciprocal CamelCase`` () =
        Check.VerboseThrowOnFailureAll<ReciprocalityCamelCase>()

    [<TestMethod>]
    [<TestCategory("FSharpLu.Json.Fuzzing")>]
    member __.``Deserialization coincides with JSon.Net (Fuzzing)`` () =
        Check.VerboseThrowOnFailureAll<CoincidesWithJsonNetOnDeserialization>()

    [<TestMethod>]
    [<TestCategory("FSharpLu.Json.Fuzzing")>]
    member __.``BackwardCompatible deserializes default Json.Net format and returns same object`` () =
        Check.VerboseThrowOnFailureAll<BackwardCompatibility>()

    [<TestMethod>]
    [<TestCategory("FSharpLu.Json.Tuples")>]
    member __.``Serialize tuples as list`` () =
        (1, 2) |> serializedAs (defaultSerialize [1; 2])
        (1, 2, 3) |> serializedAs (defaultSerialize [1; 2; 3])
        (1, 2, 3, 4, 5, 6, 7, 8, 9, 10) |> serializedAs (defaultSerialize [1; 2; 3; 4; 5; 6; 7; 8; 9; 10])

    [<TestMethod>]
    [<TestCategory("FSharpLu.Json.Tuples")>]
    member __.``Tuple serialization is backward compatible`` () =
        (1, 2) |> testBackwardCompat
        (1, "test", 5) |> testBackwardCompat
        (1, ["foo"; "bar"]) |> testBackwardCompat
        (1, ["foo"; "bar"], 4, "hello", ("bird", 3), 2, 3, 2, 4, 7) |> testBackwardCompat
        (1, 2, 3, 4, 5, 6, 7, 8, 9, 10) |> testBackwardCompat
        // Check for nested serialization: legacy JSON.net serialization breaks down tuples 
        // in buckets of 7 elements maximum. Each additional bucket gets nested under a 
        // "Rest" JSON field.
        (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17) |> testBackwardCompat

    [<TestMethod>]
    [<TestCategory("FSharpLu.Json.Tuples")>]
    member __.``Legacy tuple deserialization handles property reordering`` () =
        let r =
            """{ "Item3": 3, "Item2": 2, "Item1": 1 }"""
            |> Compact.deserialize<int*int*int>
        Assert.AreEqual(r, (1,2,3), "Tuple deserialization should handle JSON properties in any order")

    [<TestMethod>]
    [<TestCategory("FSharpLu.Json.Strictness")>]
    member __.``Reject missing fields`` () =
        assertStrictFailsToDeserialize<ARecord> """{ "name":"hola" }"""
        assertStrictFailsToDeserialize<ARecord> """{ "id":"f893e695-496d-4e30-8fc7-b2ff59725e6c" }"""
        Assert.ThrowsException<JsonSerializationException>(fun () -> Compact.Strict.deserialize<ARecord> """{ "name":"hola" }""" |> ignore) |> ignore
        ()