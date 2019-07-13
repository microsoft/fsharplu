module Microsoft.FSharpLu.Disposable

open System

/// A disposable container for an object of type 'T
type IDisposable<'T> =
    abstract member Value:'T
    inherit IDisposable

/// Creates and instance of IDisposable<'T>
type IDisposable with
    static member create value onDispose =
        {
            new IDisposable<_> with
                member __.Value = value
                member __.Dispose() = onDispose value
        }


/// IDisposable-aware operations on sequences
module Seq =
    open System.Collections.Generic

    /// Returns all elements in a sequence verifying a given condition and dispose those that do not verify the condition.
    /// (Assumes that condition does not raise an exception. If it does then there may be a memory leak. If this is a problem
    /// then replace e.Dispose() by a "use x = e in ..." construct in the code below.)
    let filterDispose<'a when 'a :> System.IDisposable> condition  =
        Seq.choose (fun (e:'a) -> if condition e then
                                        Some e
                                    else
                                        e.Dispose(); None)

    /// Returns the first element in a sequence verifying a given condition and dispose all enumerated
    /// elements that do not verify the condition.
    let tryFindDispose<'a when 'a :> System.IDisposable> condition  =
        Seq.tryFind (fun (e:'a) -> if condition e then
                                        true
                                    else
                                        e.Dispose(); false)

    /// Same as Seq.find but along the way dispose enumerated element that do not meet the condition.
    let findDispose<'a when 'a :> System.IDisposable> condition  =
        Seq.find (fun (e:'a) -> if condition e then
                                    true
                                else
                                    e.Dispose(); false)

    /// Equivalent of Seq.map but dispose elements of the original sequence
    let mapDispose<'a, 'b when 'a :> System.IDisposable> (f:'a->'b) =
        Seq.map (fun (x :'a) -> let y = f x in x.Dispose(); y)

    open System.Collections

    let noReset() = raise (new System.NotSupportedException("Reset not supported on this enumerable"))

    let IEnumerator_cast (e : IEnumerator) : IEnumerator<'T> =
        {
            new IEnumerator<'T> with
                member x.Current = unbox e.Current
            interface IEnumerator with
                member x.Current = unbox e.Current
                member x.MoveNext() = e.MoveNext()
                member x.Reset() = noReset()
            interface System.IDisposable with
                member x.Dispose() =
                    match e with
                    | :? System.IDisposable as e -> e.Dispose()
                    | _ -> ()
        }

    /// Cast an untyped IEnumerable to a generic IEnumerable<'t>
    /// while preserving the IDisposability of the original source.
    type DisposableEnumerable<'t> (source:IEnumerable) =
        interface IDisposable with
            member x.Dispose () =
                match source with
                | :? IDisposable as e -> e.Dispose()
                | _ -> ()
        interface IEnumerable with
            member x.GetEnumerator() =
                IEnumerator_cast (source.GetEnumerator()) :> IEnumerator
        interface IEnumerable<'t> with
            member x.GetEnumerator () =
                IEnumerator_cast (source.GetEnumerator()) :> IEnumerator<'t>

    /// Same as Seq.cast must return an IDisposable sequence
    /// which preserves the disposability of the original source sequence
    let castAsDisposable<'a> source =
        new DisposableEnumerable<'a>(source)