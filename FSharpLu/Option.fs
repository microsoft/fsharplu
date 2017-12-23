(*

Copyright (c) Microsoft Corporation.

Description:

    Option type helpers

Author:

    William Blum (WiBlum) created 9/27/2012

Revision history:
    Repackaged into FSharpLu on 2/18/2015

*)

module Microsoft.FSharpLu.Option

/// Branching combinator for function returning value of type 'a option
let public (|-) f g =
    match f with
    | None -> g()
    | v -> v

/// Dual branching combinator for function returning value of type 'a option
let public (|+) f g =
    match f with
    | None -> None
    | Some v -> g v

/// Convert a result to option type according to a boolean success code
let public ofCurriedPair result success =
    if success then Some result else None

/// Convert a success*result pair to a Some type
let public ofPair (success, result) =
    ofCurriedPair result success

/// Inner-join an element with a function returning an option type.
/// Apply a function to an element and return a pair of the element and the function result
/// or None if the function returns nothing.
let public mapjoin e f =
    match f e with
    | None -> None
    | Some a -> Some(e, a)

/// Convert a nullable type to a Some type
let public ofNullable (a:System.Nullable<'T>) =
    if a.HasValue then
        Some(a.Value)
    else
        None

/// Pattern-matching on option (equivalent to F# match)
let _match none some =
    function
    | None -> none()
    | Some v -> some v

/// Pattern-matching on option (equivalent to F# match)
let mapOrDefault f defaultValue =
    function
    | None -> defaultValue
    | Some v -> f v

/// Provide default value for a None optional argument
let public orDefault defaultValue x =
    defaultArg x defaultValue

/// Return the value of a some type or raise an exception if it is None
let public orRaise anException =
    function
    | None -> raise anException
    | Some v -> v

/// Return the value of a some type or if it is None execute the given default operation
let public orDo (defaultOrFailure:unit -> 'T) =
    function
    | None -> defaultOrFailure()
    | Some v -> v

// Try f then g. (f >-> g) x evaluates to match f x with None -> g x | v -> v
let (>->) f g x =
    match f x with
    | None -> g x
    | v -> v

/// Convert string to option type where null and empty string become None.
let ofString s =
    if System.String.IsNullOrEmpty s then
        None
    else
        Some s

/// Convert .net object to option type
let fromObject x =
    if obj.ReferenceEquals(x, null) then None else Some x

/// Make sure an option value does not host a null by replacing Some null by None.
let fromNull x =
    Option.bind fromObject x

/// Convert a .net Nullable object to option type
let fromNullable (o:System.Nullable<'t>) =
    if o.HasValue then
        Some o.Value
    else
        None

/// The Maybe monad
type MaybeMonad() =
    member __.Return(v) = Some v
    member __.ReturnFrom(v) = v
    member __.Bind(p, f) = match p with None -> None | Some v -> f v
    member __.Delay(f) = f
    member __.Combine(first, fallback) = if Option.isSome first then first else fallback()
    member __.Run(f) = f()

let public maybe = MaybeMonad()

