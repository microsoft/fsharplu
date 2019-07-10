namespace Microsoft.FSharpLu.Collections

open System
open System.Collections.Generic
open Microsoft.FSharpLu.Logging

/// Supported cache map operations
type private MapMailboxMessage<'Key, 'Value when 'Key:comparison> =
    | Add of 'Key * 'Value
    | Clear
    | Count of int AsyncReplyChannel
    | ContainsKey of 'Key * bool AsyncReplyChannel
    | GetItem of 'Key * 'Value option AsyncReplyChannel
    | GetSnapshot of Map<'Key, 'Value> AsyncReplyChannel
    | Contains of 'Key * 'Value * bool AsyncReplyChannel
    | Remove of 'Key
    | Dispose

module private CacheMap =
    /// Mailbox processor implementing immutable cache
    /// Note: one thing to keep in mind that if a failure occurs in the agent (MailboxProcessor), it will create deadlocks:
    /// any call to a member expecting a result (like GetItem or Contains) will just hang forever and
    /// block the calling thread instead of propagating the failure to the caller.
    /// There are no any obvious possible exception in the current implementation
    /// (though ch.Reply could possibly fail but that's a corner case). Nevertheless this is something
    /// to keep in mind if we ever have to touch this code again. For instance one method you might
    /// want to add in the future is the conditional add (useful to implement concurrent conditional mutations to the dictionary).
    /// The conditional add would normally take a function as parameter to evaluate the condition.
    /// If this function throws an exception it takes down the entire agent and block all other consumers of the CacheMap.
    /// Possible solution:
    /// Convert all channel replies to Choice<'a, exn>
    /// add try/catch for everything that might throw, return Choice2Of2 exn on that channel. If channel.Reply fails then catch exception
    /// log it, and move on.
    /// It is also safe to reset the cache to empty in case of an error
    let mapMailbox<'Key, 'Value when 'Key: comparison and 'Value: equality>() =
        MailboxProcessor.Start(fun inbox ->
            let rec loop(map: Map<'Key, 'Value >) =
                async{
                    let! msg = inbox.Receive()
                    match msg with
                    | Add(k,v) ->
                        return! loop(map.Add(k, v))
    
                    | Clear ->
                        return! loop Map.empty
    
                    | Count ch ->
                        ch.Reply <| map.Count
                        return! loop map
    
                    | ContainsKey(k, ch) ->
                        ch.Reply <| map.ContainsKey k
                        return! loop map
    
                    | GetItem(k, ch) ->
                        ch.Reply <| map.TryFind k
                        return! loop(map)
    
                    | GetSnapshot ch ->
                        ch.Reply <| map
                        return! loop(map)
    
                    | Contains (k, v, ch) ->
                        ch.Reply <| (map |> Map.exists(fun key value -> key = k && value = v))
                        return! loop(map)
    
                    | Remove k ->
                        return! loop (map.Remove k)
    
                    | Dispose ->
                        return ()
                }
            loop Map.empty)

/// A thread-safe Map.
/// Implemented using F# Map and MailboxProcessor
type CacheMap<'Key, 'Value when 'Key:comparison and 'Value: equality>()=

    let mb =
        let mb = CacheMap.mapMailbox()
        mb.Error.Add(fun error -> Trace.error "%A" error)
        mb

    member __.Add (key: 'Key, value: 'Value) =
        mb.Post(Add(key, value))

    member __.Clear() =
        mb.Post Clear

    member __.Contains (key:'Key, value: 'Value) =
        mb.PostAndReply(fun ch -> Contains(key, value, ch))

    member __.ContainsKey(key: 'Key) =
        mb.PostAndReply (fun ch -> ContainsKey(key, ch))

    member __.Count =
        mb.PostAndReply Count

    member __.GetEnumerator() =
        (mb.PostAndReply(GetSnapshot) :> IDictionary<'Key, 'Value>).GetEnumerator()

    member __.Item
        with get (key: 'Key) =
            match mb.PostAndReply(fun ch -> GetItem(key, ch)) with
            | Some r -> r
            | None -> raise (KeyNotFoundException(sprintf "Item with key:%A is not in the CacheMap" key))

        and set (key: 'Key) (v: 'Value) =
            mb.Post(Add(key, v))

    member __.Remove(key: 'Key) =
        let r = mb.PostAndReply(fun ch -> ContainsKey(key, ch))
        if r then
            mb.Post(Remove key)
        r

    member __.Remove(key: 'Key, value: 'Value) =
        let r = mb.PostAndReply(fun ch -> Contains(key, value, ch))
        if r then
            mb.Post(Remove key)
        r

    member __.TryGetValue(key: 'Key) =
        mb.PostAndReply(fun ch -> GetItem(key, ch))

    member this.Snapshot =
        mb.PostAndReply(GetSnapshot)

    /// Wrap function f with a new function that tries to get matching value from cache first before calling f
    member this.MakeCacheFunction (f: 'Key -> 'Value) =
        fun (k: 'Key) ->
            match this.TryGetValue k with
            | Some v -> v
            | None ->
                let v = f k
                this.Add(k, v)
                v

    member this.MakeAsyncCacheFunction (f: 'Key -> 'Value Async) =
        fun (k: 'Key) ->
            async{
                match this.TryGetValue k with
                | Some v -> return v
                | None ->
                    let! v = f k
                    this.Add(k, v)
                    return v
            }

    interface IDisposable with
        member __.Dispose() =
            mb.Post Dispose

    interface IDictionary<'Key, 'Value> with
        member x.Add(key: 'Key, value: 'Value) = x.Add(key, value)
        member x.Add(item: KeyValuePair<'Key,'Value>) = x.Add(item.Key, item.Value)
        member x.Clear() = x.Clear()
        member x.Contains(item: KeyValuePair<'Key,'Value>) = x.Contains(item.Key, item.Value)
        member x.ContainsKey(key: 'Key) = x.ContainsKey key

        member __.CopyTo(array: KeyValuePair<'Key,'Value> [], arrayIndex: int) =
            (mb.PostAndReply(GetSnapshot) :> IDictionary<'Key, 'Value>).CopyTo(array, arrayIndex)

        member x.Count = x.Count
        member x.GetEnumerator(): IEnumerator<KeyValuePair<'Key,'Value>> = x.GetEnumerator()

        member __.GetEnumerator(): System.Collections.IEnumerator =
            failwith "Not implemented"

        member __.IsReadOnly = false

        member x.Item
            with get (key: 'Key) = x.[key]
            and set (key: 'Key) (v: 'Value) = x.[key] <- v

        member x.Remove(key: 'Key) = x.Remove(key)
        member x.Remove(item: KeyValuePair<'Key,'Value>) = x.Remove(item.Key, item.Value)
        member x.TryGetValue(key: 'Key, v: byref<'Value>) =
            match x.TryGetValue key with
            | Some r ->
                v <- r
                true
            | None ->
                false

        member __.Keys: ICollection<'Key> =
            (mb.PostAndReply(GetSnapshot) :> IDictionary<'Key, 'Value>).Keys

        member __.Values: ICollection<'Value> =
            (mb.PostAndReply(GetSnapshot) :> IDictionary<'Key, 'Value>).Values

