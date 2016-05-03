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


let serialize = Union.serialize
let deserialize = Union.deserialize

let reciprocal<'T when 'T:equality> (x:'T) =
    // theoretically one round trip is sufficient; we perform 
    // two round trips here to test for possible side-effects
    // in the serialization functions
    x |> serialize |> deserialize |> serialize |> deserialize = x

let areReciprocal (x:'T) = 
    Assert.IsTrue(reciprocal x)

let serializedAs json o = 
    Assert.AreEqual(json, serialize o)

let conv = DiscriminatedUnionJsonConverter()

type FuzzList () =
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
        Assert.IsTrue(conv.CanConvert(Color.Red.GetType()))
        Assert.IsTrue(conv.CanConvert(typeof<Color>))
        Assert.IsTrue(conv.CanConvert(typeof<_ option>))
        Assert.IsTrue(conv.CanConvert(typeof<_ Tree>))
        Assert.IsTrue(conv.CanConvert(typeof<Shape>))
        Assert.IsFalse(conv.CanConvert(typeof<_ list>))

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

    [<TestMethod>]
    [<TestCategory("FSharpLu.Json")>]
    member __.Fuzzing () =
        Check.VerboseThrowOnFailureAll<FuzzList>()
