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