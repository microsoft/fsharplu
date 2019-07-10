/// Copyright (c) Microsoft Corporation.
namespace Microsoft.FSharpLu


/// Extensions for the AsyncSeq module from FSharp.Control
module AsyncSeq =
    open FSharp.Control

    /// Create an AsyncSeq from a sequence of async computation
    let ofSeqAsync (s:seq<Async<'t>>) : AsyncSeq<'t> =
        asyncSeq {
            for asyncElement in s do
                let! v =  asyncElement
                yield v
        }

    /// Concatanate an asynchronous sequence of AsyncSeq
    let concat (s:AsyncSeq<AsyncSeq<'t>>) : AsyncSeq<'t> =
        asyncSeq {
            for innerSeq in s do
                for e in innerSeq do
                    yield e
        }
