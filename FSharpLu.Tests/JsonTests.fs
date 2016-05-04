module Microsoft.FSharpLu.Json.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsCheck
open Microsoft.FSharpLu.Json

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

type 'a Ambiguous = { Some : 'a }

let inline serialize< ^T> x = Compact.serialize< ^T> x
let inline deserialize x = Compact.deserialize x

let inline reciprocal< ^T when ^T:equality> (x: ^T) =
    // theoretically one round trip is sufficient; we perform 
    // two round trips here to test for possible side-effects
    // in the serialization functions
    x |> serialize |> deserialize |> serialize |> deserialize = x

let inline areReciprocal (x:'T) = 
    Assert.IsTrue(reciprocal x)

/// Check that given object serializes to the specified Json string
let inline serializedAs json o = 
    Assert.AreEqual(json, serialize o)

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

type Reciprocality () =
    static member x1 = reciprocal<ComplexDu>
    static member x2 = reciprocal<ComplexDu RecursiveList>
    static member x3 = reciprocal<WithFields>
    static member x4 = reciprocal<SimpleDu>
    static member x5 = reciprocal<ComplexDu>
    static member x6 = reciprocal<OptionOfBase>
    static member x7 = reciprocal<OptionOfDu>
    static member x8 = reciprocal<Color>
    static member x9 = reciprocal<Shape>
    static member x10 = reciprocal<int Tree>
    static member x11 = reciprocal<int Tree Test>
    static member x12 = reciprocal<int Test>
    static member x13 = reciprocal<int list Tree>
    static member x14 = reciprocal<string NestedOptions>
    static member x15 = reciprocal<string>
    static member x16 = reciprocal<string option>
    static member x17 = reciprocal<string option option>
    static member x18 = reciprocal<string option option option option>
    static member x19 = reciprocal<int NestedOptions>
    static member x20 = reciprocal<Ambiguous<string>>
    static member x21 = reciprocal<Ambiguous<SimpleDu>>

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
    static member x20 = coincidesWithDefault<Ambiguous<string>>
    static member x21 = coincidesWithDefault<Ambiguous<SimpleDu>>

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
    static member x20 = backwardCompatibleWithDefault<Ambiguous<string>>
    static member x21 = backwardCompatibleWithDefault<Ambiguous<SimpleDu>>


[<TestClass>]
type JsonSerializerTests() =

    [<ClassInitialize>]
    static member init(context : TestContext) = ()

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
        let conv = CompactUnionJsonConverter()
        Assert.IsTrue(conv.CanConvert(Color.Red.GetType()))
        Assert.IsTrue(conv.CanConvert(typeof<Color>))
        Assert.IsTrue(conv.CanConvert(typeof<_ option>))
        Assert.IsTrue(conv.CanConvert(typeof<_ Tree>))
        Assert.IsTrue(conv.CanConvert(typeof<Shape>))
        Assert.IsFalse(conv.CanConvert(typeof<_ list>))
        Assert.IsFalse(conv.CanConvert(typeof<Map<_,_>>))

    [<TestMethod>]
    [<TestCategory("FSharpLu.Json")>]
    member this.``Serialization does not raise exceptions for basic types``() =
        [
            serialize <| Red
            serialize <| Blue
            serialize <| Circle (8,99)
            serialize <| Some 8
            serialize <| Some (Circle(5,120))
            serialize <| None
            serialize <| Some (Node(Leaf 1,Leaf 9))
            serialize <| Case1
            serialize <| Case2 (3)
            serialize <| Case3 (3,"s", "Foo")
            serialize <| Circle (8,10)
            serialize <| Leaf ["s";"s"]
        ] |> printfn "%A"

    [<TestMethod>]
    [<TestCategory("FSharpLu.Json")>]
    member this.``Serialization and deserialization are reciprocal``() =
        areReciprocal <| Some 8
        areReciprocal <| Red
        areReciprocal <| Circle (8,10)
        areReciprocal <| Leaf ["s";"s"]
        areReciprocal <| Leaf "s"
        areReciprocal <| Some 8
        areReciprocal <| Red
        areReciprocal <| Circle (8,10)
        areReciprocal <| Node((Leaf "s"),(Leaf "s")) 
        areReciprocal <| Leaf ["s";"s"]
        areReciprocal <| Node((Leaf 1),(Leaf 9))
        areReciprocal <| Case1
        areReciprocal <| Case2 (3)
        areReciprocal <| Case3 (3,"s", "Foo")
        areReciprocal <| (["test", [3;3;4]] |> Map.ofSeq)
        areReciprocal <| ["test", [3;3;4]]
        areReciprocal <| Some (Some (Some None))
        areReciprocal <| Some (Some None)
        areReciprocal <| Some null
        areReciprocal <| Some None
        areReciprocal <| Some (Some (Some None))

    [<TestMethod>]
    [<TestCategory("FSharpLu.Json")>]
    member this.``No ambiguity between records and Option type``() =
        areReciprocal <| Some (Some (Some None))
        areReciprocal <| { Some = null }
        areReciprocal <| { Some = SimpleDu.Foo }
        areReciprocal <| { Some = "test" }
        areReciprocal <| { Some = 123 }

    [<TestMethod>]
    [<TestCategory("FSharpLu.Json.Fuzzing")>]
    member __.``Fuzzing Reciprocal`` () =
        Check.VerboseThrowOnFailureAll<Reciprocality>()

    [<TestMethod>]
    [<TestCategory("FSharpLu.Json.Fuzzing")>]
    member __.``Deserialization coincides with JSon.Net (Fuzzing)`` () =
        Check.VerboseThrowOnFailureAll<CoincidesWithJsonNetOnDeserialization>()

    [<TestMethod>]
    [<TestCategory("FSharpLu.Json.Fuzzing")>]
    member __.``BackwardCompatible deserializes default Json.Net format and returns same object`` () =
        Check.VerboseThrowOnFailureAll<BackwardCompatibility>()