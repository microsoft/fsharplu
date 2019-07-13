namespace Microsoft.FSharpLu.FSharpFromCSharp

open System
open System.Runtime.CompilerServices

/// Extension methods helper to access option types from C#
[<Extension>]
type OptionTypeHelpers =
    [<Extension>]
    static member IsSome x = Option.isSome x

    [<Extension>]
    static member OrDefault(x, v) =
        match x with
        | None -> v
        | Some x -> x

    [<Extension>]
    static member MapOrDefault<'T,'R>(x, f:Func<'T,'R>, v:'R) =
        match x with
        | None -> v
        | Some x -> f.Invoke(x)

    [<Extension>]
    static member Some a = Option.Some a

    [<Extension>]
    static member None a = Option.None
