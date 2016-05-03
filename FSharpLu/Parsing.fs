/// Parsing text to F# data types
module Microsoft.FSharpLu.Parsing

open Microsoft.FSharpLu.Option

/// Try to parse an int32
let public tryParseInt =
    System.Int32.TryParse >> ofPair

/// A parser for string parameter
let public tryParseString = Some

/// Try to parse a boolean
let public tryParseBoolean = function
    | "0" -> Some false
    | "1" -> Some true
    | b -> System.Boolean.TryParse b |> ofPair

/// Parse a boolean
let public parseBoolean =
    tryParseBoolean >> orDo (fun() -> invalidOp "Invalid boolean format")

/// Parse a C#-like enumeration (i.e. of the form type MyEnum = One = 1 | Two = 2)
let public tryParseEnum<'T when 'T : struct
                            and 'T : (new : unit -> 'T) 
                            and 'T :> System.ValueType> e = 
    System.Enum.TryParse<'T>(e, true) |> ofPair

/// Lookup value from a dictionary and try to parse it with the provided parser
let public tryParseDictValue dict key parser =
   Collections.Dictionary.tryGetValue dict key
   |> Option.bind parser

/// Try to parse a Guid
let public tryParseGuid value =
    System.Guid.TryParse value |> ofPair

module Union =
    open FSharp.Reflection

    /// Parse a field-less discriminated union of type 'T from string
    /// This only works with simple *field-less* discriminated union of 
    /// the form "type Bla = Foo | Bar"
    let tryParse<'T> (string:string) =
        let fields =
                typeof<'T>
                |> FSharpType.GetUnionCases 
                |> Array.filter (fun case -> case.Name.Equals(string, System.StringComparison.InvariantCultureIgnoreCase))
        match fields with
        | [| case |] -> Some(FSharpValue.MakeUnion(case,[||]) :?> 'T)
        | _ -> None
