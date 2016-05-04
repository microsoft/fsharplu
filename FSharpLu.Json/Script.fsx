#r @"System.dll"
#r @"System.Core.dll"
#r @"..\packages\Newtonsoft.Json.8.0.3\lib\net45\Newtonsoft.Json.dll"

#load "Helpers.fs"
#load "WithFunctor.fs"
#load "NewtonSoft.fs"
#load "Compact.fs"
#load "BackwardCompatible.fs"
open Microsoft.FSharpLu.Json

type WithFields = SomeField of int * int
type ComplexDu = ComplexDu of WithFields | SimpleDU | AString of string
let x= (ComplexDu (SomeField (4,9))) |> Default.serialize
let y = x |> Default.deserialize :> ComplexDu 
let z = x |> Compact.deserialize :> ComplexDu

module T =
    type OptionOfBase = int option
    let x = Some 5
    Some 5 |> Default.serialize |> Default.deserialize :> OptionOfBase 
    Some 5 |> Default.serialize |> BackwardCompatible.deserialize :> OptionOfBase 
    Default.deserialize<OptionOfBase> "null"
    BackwardCompatible.deserialize<OptionOfBase> "null"
