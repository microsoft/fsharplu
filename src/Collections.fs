(* 

Copyright (c) Microsoft Corporation.

Description:

    Helper functions to work with generic collections (Hashset,
    Dictionary, IEnumerable, ...), option types and triples

Author:

    William Blum (WiBlum) created 9/27/2012

Revision history:
    Repackaged into FSharpLu on 2/18/2015

*)

namespace Microsoft.FSharpLu

open System
open System.Runtime.InteropServices 
open System.Collections.Generic
open System.Collections.Concurrent
open System.Linq

////////////////
// Option type helpers
module Option =
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
    let public ofNullable (a:Nullable<'T>) = 
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

    /// The Maybe monad
    type MaybeMonad() = 
        member x.Return(v) = Some v
        member x.ReturnFrom(v) = v
        member x.Bind(p, f) = match p with None -> None | Some v -> f v
        member x.Delay(f) = f
        member x.Combine(first, fallback) = if Option.isSome first then first else fallback()
        member x.Run(f) = f()

    let public maybe = MaybeMonad()

////////////////
// Tuple manipulation
module Triple =
    /// Get the first element in a triple
    let first (x,_,_) = x

    /// Get the second element in a triple
    let second (_,x,_) = x

    /// Get the third element in a triple
    let third (_,_,x) = x

////////////////
// Dictionaries
module Collections =

    /// Get the key from a key-value pair
    let getKey (kvp:KeyValuePair<'K,'V>) = kvp.Key

    /// Get the value from a key-value pair
    let getValue (kvp:KeyValuePair<'K,'V>) = kvp.Value

    /// Convert a KeyValuePair structure into a pair
    let KeyValueToPair (kvp:KeyValuePair<'K,'V>) = kvp.Key,kvp.Value

    /// Iteration over dictionary key/value pairs
    let dictIter f = 
        Seq.iter (KeyValueToPair >> f)

    /// Convert a dictionary enumeration to a enumeration of pairs of type (key,value)
    let dictAsPairs<'K,'V> : (IDictionary<'K,'V> -> IEnumerable<'K*'V>)  =
        Seq.map KeyValueToPair

    /// Alternative to TryGetValue for IDictionary based on option type
    let tryGetValue<'K,'V> (h:IDictionary<'K,'V>) key =
        match h.TryGetValue(key) with
        | false, _ -> None
        | true, v -> Some(v)

    /// Calculate the union of two sets (source and target) and store the 
    /// result in the first one (target). 
    ///
    /// Same as HashSet.UnionWith except that it returns true if the target set
    /// is a proper subset of the source set before the union is calculated.
    let unionWith (target:HashSet<'t>) (source:HashSet<'t>) =
       let sizeBefore = target.Count
       target.UnionWith(source)
       target.Count > sizeBefore

    /// Calculate the union of two dictionaries (source and target) and store
    /// the result in the first one (target).
    ///
    /// If the same key exists in the source and the target dictionary then
    /// the two corresponding values are merged using the provided 'merge' function
    /// and the result is stored in the target dictionary.
    ///
    /// If a key from source does not exist in target then the provided 'tranform' 
    /// function is applied to the corresponding value and the result is added to 
    /// the target dictionary under the same key.
    let dictUnionWith (target:IDictionary<'t,'v>) (source:IDictionary<'t,'v>) merge transform =
        let grows = ref false
        for e in source do
            if target.ContainsKey(e.Key) then
                target.[e.Key] <- merge target.[e.Key] e.Value
            else
                target.Add(e.Key, transform e.Value)
                grows := true
        done
        !grows

    /// Functor used to create a HashMultiMap container type. HashMultimaps are like dictionaries
    /// except that each key can map to multiple elements.
    [<AbstractClass>]
    type public HashMultiMapFunctor<'K,'V when 'K: equality and 'V:comparison>
        (
            dict : IDictionary<'K,HashSet<'V>>
        ) =

        /// Add a value to the multimap under a specified key
        abstract member Add : 'K -> 'V -> unit
    
        /// Remove a single value under the specified key from the multimap
        abstract member RemoveValue : 'K -> 'V -> bool

        /// Non concurrent implementation of Add
        member x.AddNonConcurrent (key:'K) (value:'V) =
            match dict.TryGetValue key with
            | true, hashset ->
                hashset.Add(value) |> ignore
            | _ ->
                dict.Add(key, HashSet<'V>([value]))

        /// Remove all entries from the map
        member x.Clear() =
            dict.Clear()

        /// Remove all entries from the map and force clearing each value bag
        member x.DeepClear() =
            dict.Values
            |> Seq.iter (fun bag -> bag.Clear())
            x.Clear()

        /// Set the set of values associated with a given key
        member x.SetRange (key:'K) (values:HashSet<'V>) =
            dict.[key] <- values

        /// Remove an entire key entry
        member x.RemoveKey (key:'K) =
            dict.Remove key |> ignore

        /// Gets the sequence of hash keys
        member x.Keys = dict.Keys

        /// Remove a single value from the map (non-concurrent implementation)
        member x.RemoveValueNonConcurrent (key:'K) (value:'V) =
            match dict.TryGetValue key with
            | true, values -> 
                if values.Remove value then
                    if not (values.Any()) then
                        dict.Remove key |> ignore
                    true
                else
                    false
            | false, _ ->
                false

        /// Determine if a key exists
        member x.ContainsKey (key:'K) =
            dict.ContainsKey key

        /// Returns the values associated with a specified key as an enumerable
        member x.Item (key:'K) =
            match dict.TryGetValue key with
            | true, values -> values.AsEnumerable()
            | _ -> Seq.empty<'V>

        /// Same as Items but returns a collection instead
        member x.ItemAsCollection (key:'K) =
            match dict.TryGetValue key with
            | true, values -> values :> ICollection<'V>
            | _ -> Set.empty<'V> :> ICollection<'V>

        /// Lookup item
        member x.TryGetValue (key:'K, [<Out>]value:byref<HashSet<'V>>) =
            dict.TryGetValue(key,&value)

    /// A non-concurrent HashMultiMap container.
    type public HashMultiMap<'K,'V when 'K: equality and 'V:comparison>
        (
            hasheq: IEqualityComparer<'K>
        ) =
        inherit HashMultiMapFunctor<'K,'V>(Dictionary<'K,HashSet<'V>>(hasheq))
        override x.Add key value = x.AddNonConcurrent key value
        override x.RemoveValue key value = x.RemoveValueNonConcurrent key value

    /// A concurrent HashMultiMap container type created from an existing
    /// concurrent dictionary.
    //
    /// WARNING: this implementation only guarantees thread-safety when accessing the 
    /// keys. The values are still stored in a thread-*unsafe* HashSet.
    /// This is fine as long as no two threads access and modify the same key's values at the same time.
    /// (which is the case in the context of RetSet)
    type public ConcurrentHashMultiMap<'K,'V when 'K: equality and 'V:comparison>
        (
            concurrentDict : ConcurrentDictionary<'K,HashSet<'V>>
        ) =
        inherit HashMultiMapFunctor<'K,'V>(concurrentDict)
    
        /// Thread-safe implementation of Add.
        override x.Add key value =
            concurrentDict.AddOrUpdate(
                key,
                (fun _ -> new HashSet<'V>([value])),
                (fun _ (existingHashSet:HashSet<'V>) -> 
                        lock existingHashSet (fun () -> existingHashSet.Add value |> ignore; existingHashSet))
                )
            |> ignore
 
        /// Thread-safe implementation of RemoveValue.
        override x.RemoveValue (key:'K) (value:'V) =
            // TODO: implement thread-safe value removal: requires either 
            // - extending ConcurrentDictionary class with a new RemoveOrUpdate atomic construct
            // - or replacing HashSet values with thread-safe HashSets
            raise (NotImplementedException())

    /// A concurrent HashMultiMap container type based on
    /// a specified equality comparer.
    //
    /// WARNING: this implementation only guarantees thread-safety when accessing the 
    /// keys. The values are still stored in a thread-*unsafe* HashSet.
    /// This is fine as long as no two threads access and modify the same key's values at the same time.
    /// (which is the case in the context of RetSet)
    type public ConcurrentHashMultiMapFromHashComparer<'K,'V when 'K: equality and 'V:comparison>
        (
            hasheq: IEqualityComparer<'K>
        ) =
        inherit ConcurrentHashMultiMap<'K,'V>(new ConcurrentDictionary<'K,HashSet<'V>>(hasheq))

    /// Convert an enumerable to a HashMultiMap
    let toHashMultiMap<'K,'V,'T when 'V:comparison and 'K:equality> 
            (keySelector:'T -> 'K)
            (valueSelector:'T -> 'V)
            (enumerable:seq<'T>)
            (hasheq: IEqualityComparer<'K>) =

        let map = new HashMultiMap<'K,'V>(hasheq)
        Seq.iter (fun e -> map.Add (keySelector e) (valueSelector e)) enumerable
        map

/// Extension to Seq module
module Seq =
    /// Same as Seq.filter but provide the element index as a parameter to the
    /// filter function.
    let public filteri f =
        Seq.mapi (fun i e -> i, e)
        >> Seq.filter (fun (i, e) -> f i e)
        >> Seq.map snd

    /// Apply a function to each element of a sequence and return a sequence of 
    /// pair consisting of the elements from the original sequence paired with 
    /// the result of the function applied to them.
    let augment f =
        Seq.map (fun e -> e, f e)

    /// Partition a sequence based on the specified condition. (Counterpart of List.partition
    /// for IEnumerables)
    let partition f =
        Seq.map (fun e -> if f e then Some(e), None else None, Some(e))
        >> Seq.cache
        >> fun p -> Seq.choose fst p, Seq.choose snd p

    /// T piping operator: iterate a function on each element in the sequence and returns the unchanged sequence
    let tee f =
        Seq.map (fun element -> f element; element)

    /// Helper TEE piping operator used to measure how many items go through
    /// a piped sequence.
    /// Usage:
    ///     |?= (fun len -> printf "Throughput %d" len)
    let (|?=) (source:seq<'t>) (measurer:int -> unit) =
        let counter = ref 0
        seq 
            {
                for element in source do
                     incr counter
                     yield element
                measurer !counter
                counter := 0 
            }

    /// Same as |?= but takes effect only if program is built is VERBOSEDEBUG macro on
    let inline (|?=@) (s:seq<'t>) (measurer:int -> unit) =
    #if DIAGNOSTICING
        s |?= measurer
    #else
        s
    #endif
