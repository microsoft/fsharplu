#r @"System.dll"
#r @"System.Core.dll"
#r @"%USERPROFILE%\.nuget\packages\Newtonsoft.Json\10.0.3\lib\net45\Newtonsoft.Json.dll"

#load "Helpers.fs"
#load "WithFunctor.fs"
#load "Default.fs"
#load "Compact.fs"
#load "BackwardCompatible.fs"
open Microsoft.FSharpLu.Json

type WithFields = SomeField of int * int
type ComplexDu = ComplexDu of WithFields | SimpleDU | AString of string
let x= (ComplexDu (SomeField (4,9))) |> Default.serialize
let y = x |> Default.deserialize<ComplexDu>
let z = x |> Compact.deserialize<ComplexDu>

module T =
    type OptionOfBase = int option
    let x = Some 5
    // Some 5 |> Default.serialize |> Default.deserialize :> OptionOfBase
    // Some 5 |> Default.serialize |> BackwardCompatible.deserialize :> OptionOfBase
    Default.deserialize<OptionOfBase> "null"
    BackwardCompatible.deserialize<OptionOfBase> "null"

module T2  =
    type X = {Some :string}

    Option.Some { X.Some = "test"} |> Compact.serialize

    Option.Some { X.Some = null} |> Compact.serialize

    let z = Option.Some { X.Some = null} |> Compact.serialize |> Compact.deserialize<X option>

    Some (Some null) |> Compact.serialize<obj option option> |> Compact.deserialize<obj option option>

    Some null |> Compact.serialize<obj option> |> Compact.deserialize<obj option>

    null  |> Compact.serialize|> Compact.deserialize<obj option option>

/// Compact serialization of tuples: enhancement proposed in https://github.com/Microsoft/fsharplu/issues/31
module Tuple =
    open Microsoft.FSharp.Reflection

    let x = 1, 2
    let t = x.GetType()
    let e = FSharpType.GetTupleElements(t)
    FSharpType.IsTuple t
    FSharpValue.GetTupleFields(x)
    Compact.serialize (2,3)

    Compact.serialize (1,2,3) = Compact.serialize [1;2;3]

    Compact.serialize [1;2;3]
    |> Default.deserialize<int []>

    Compact.serialize<int list> [1;2]
    |> Compact.deserialize<FSharp.Collections.List<int>>

    Compact.serialize (1,2)
    |> Compact.deserialize<int * int>

    (ComplexDu (SomeField (4,9)))
    |> Compact.serialize

    FSharpType.IsTuple <| y.GetType()


    Compact.Legacy.serialize (1,2,3)

    let order = """
{
  "Item3": 3,
  "Item2": 2,
  "Item1": 1,
}"""
    Compact.deserialize<int*int*int> order

    Compact.serialize (1,"a string", ["a"; "list"])

    Compact.Legacy.serialize (1,"a string", ["a"; "list"])

    " [ 1, 'A string' ] " |> Compact.deserialize<int * string>


    Compact.serialize<int32*int32> (1u,2u)
    |> Compact.deserialize<int32 * int32>

    Compact.serialize<int32*int32> (1u,2u)
    |> Compact.deserialize<int32 List>


    FSharpValue.MakeTuple([|2;3|],typeof<int*int>)


    Compact.Legacy.serialize (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
    |> Compact.Legacy.deserialize<int*int*int*int*int*int*int*int*int*int*int*int*int*int*int*int*int> 

    Compact.Legacy.serialize (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
    |> Compact.deserialize<int*int*int*int*int*int*int*int*int*int*int*int*int*int*int*int*int> 

    ////
    module Deserialization =
        open Newtonsoft.Json
        open System.IO
        let x = (1u,2u)
        let json =  Compact.serialize<int32*int32> x

        let stringReader = new StringReader(json)
        let reader = new JsonTextReader(stringReader)

        let more = reader.Read()
        if not more || reader.TokenType <> JsonToken.StartArray then
            failwithf "Expecting a JSON array, got something else"

        let tupleType = x.GetType()
        let elementTypes = FSharpType.GetTupleElements(tupleType)

        let readElement elementType =
            let more = reader.Read()
            if not more then
                failwith "Missing array element in deserialized JSON"

            if isNull reader.Value then
                failwith "expeciting a JSON element, got a token instead"
            let jToken = Linq.JToken.ReadFrom(reader)
            jToken.ToObject(elementType)

        elementTypes
        |> Array.map readElement
