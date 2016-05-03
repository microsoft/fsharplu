////
//// Helpers and utilities for Asynchronous workflows
////
module Microsoft.FSharpLu.Async

open System
open System.Threading
open System.Threading.Tasks
open Logging

/// Start multiple async-workflows concurrently
/// and return a new async that waits for the first async to return.
/// After one async has returned all the remaining ones are cancelled,
/// and the workflow may return before the cancellations of the remaining workflows complete.
let Compete workflows =
    async {
        use loosers = new System.Threading.CancellationTokenSource()

        // The call to loosers.Cancel in the finally block is not sufficient.
        // Doing it from OnCancel() guarantees that the children
        // workflows are properly terminated if the parent workflow is cancelled.
        use! c = Async.OnCancel(fun() -> loosers.Cancel())
        
        try
            let! winningTask =
                workflows
                |> Seq.map (fun w -> Async.StartAsTask(w, cancellationToken=loosers.Token))
                |> Task.WhenAny
                |> Async.AwaitTask
            return winningTask.Result
        finally
            // cancell all other tasks
            loosers.Cancel()
    }

/// Async.Compete between an Async<'A> and a Task<'B>.
///
/// Create an asynchronous workflow that concurrently runs an async workflow and 
/// a .Net Task and returns a discriminating union of three possible outcomes depending on
/// which computation finished first or if they finished simultaneously.
/// When one of the computation wins, the other 'loosing' computation 
/// is cancelled synchronoulsy.
let CompeteWithTask<'A, 'B> (workflow:Async<'A>) (taskBuilder:CancellationTokenSource -> Task<'B>) =
    async {
        use looser = new System.Threading.CancellationTokenSource()
        
        // The call to looser.Cancel in the finally block is not sufficient.
        // Doing it from OnCancel guarantees that the children
        // task/workflow are properly terminated if the parent workflow is cancelled.
        use! c = Async.OnCancel(fun() -> looser.Cancel())

        let t1 = Async.StartAsTask(workflow, cancellationToken=looser.Token) 
        let t2 = taskBuilder looser
        try
            let! competition = Tasks.Task.WhenAny [| t1:> Task; t2:> Task |] |> Async.AwaitTask
            ()
        finally
            looser.Cancel()
        
        // Wait for the looser task cancellation to complete (a TaskCanceledException exception will be triggered when this happens)
        do! async {
            try
                if not t1.IsCompleted then
                    let! _ = Async.AwaitTask t1 in ()
                elif not t2.IsCompleted then
                    let! _ = Async.AwaitTask t2 in ()
            with
            :? System.AggregateException as e ->
                if e.InnerExceptions |> Seq.exists (function e -> e :? TaskCanceledException) then
                    raise e
        }
        return
            match t1.IsCompleted && not t1.IsCanceled, t2.IsCompleted && not t2.IsCanceled with
            | true, false -> Choice1Of3 (t1.Result)
            | false, true -> Choice2Of3 t2.Result
            | true, true -> Choice3Of3 (t1.Result, t2.Result)
            | false, false -> invalidOp "Both competing tasks failed to complete."
    }

/// Return an object that when disposed automatically calls the object's Release() method
let inline private releaseOnDispose (eventObject: ^T) =
    { new System.IDisposable with
        member x.Dispose() =
            (^T : (member Release : unit -> ^R) (eventObject)) |> ignore
    }

/// Create an asynchronous workflow that concurrently runs an async workflow and 
/// tries to acquire a given .Net threading object (e.g. SlimSemaphore, ...).
/// It returns a discriminating union representing the task that finished first. The other one
/// (either the workflow or the threading object) is properly terminated and disposed.
let inline CompeteWithThreadingObject<'A, ^R, ^T when ^T: (member Release : unit -> ^R) 
                                                  and ^T: (member WaitAsync : int -> CancellationToken -> Task<bool>)>
            (workflow:Async<'A>) (threadingObject: ^T) =
    async {
        let! r = CompeteWithTask workflow (fun source -> (^T: (member WaitAsync : int -> CancellationToken -> Task<bool>)(threadingObject, -1, source.Token)))
        return match r with
               | Choice1Of3 x -> Choice1Of3 x
               | Choice2Of3 true -> Choice2Of3 (releaseOnDispose threadingObject)
               | Choice3Of3 (x,true) -> Choice3Of3 (x, releaseOnDispose threadingObject)
               | Choice2Of3 false
               | Choice3Of3 (_,false) -> assert false; invalidOp "Both competing tasks failed to complete."
    }


/// Execute an asynchronous computation until it succeeds or the specified timeout expires.
let retry (timeout:TimeSpan, retryDelay:TimeSpan, f:unit -> Async<'T>) =
    let beginPollTime = DateTime.UtcNow
    let endPollTime = beginPollTime + timeout

    let rec loop () =
        async {
            try
                return! f()
            with
            | e when DateTime.UtcNow <= endPollTime ->
                Trace.info "Exception in retry loop (will retry in %ds): %s" (int retryDelay.TotalSeconds) e.Message
                do! Async.Sleep(int retryDelay.TotalMilliseconds)
                return! loop()
        }
    loop ()

/// Execute an asynchronous computation until it succeedes, an unexpected exception occurs or the specified timeout expires.
/// The exception filter should return true for permissible exeption and false for unexpected exceptions.
let retryOnSpecificFailures (timeout:TimeSpan, retryDelay:TimeSpan, f:unit -> Async<'T>, exceptionFilter: Exception -> bool) =
    let beginPollTime = DateTime.UtcNow
    let endPollTime = beginPollTime + timeout
    let rec loop () =
        async {
            try
                return! f()
            with
            | e when exceptionFilter e && DateTime.UtcNow <= endPollTime ->
                do! Async.Sleep(int retryDelay.TotalMilliseconds)
                return! loop()
        }
    loop ()

/// Execute an asynchronous computation until it returns something, an unexpected exception occurs or the specified timeout expires.
let retryUntilSome (timeout:TimeSpan) (retryDelay:TimeSpan) (f:unit -> Async<'T option>) =
    let retryDelay = if retryDelay > timeout then timeout else retryDelay

    let beginPollTime = DateTime.UtcNow
    let endPollTime = beginPollTime + timeout
    let rec loop () =
        async {
            let! r = f()
            match r with
            | Some v ->
                return v
            | None when DateTime.UtcNow <= endPollTime ->
                do! Async.Sleep(int retryDelay.TotalMilliseconds)
                return! loop()
            | None ->
                return raise <| System.TimeoutException()
        }
    loop ()

/// Execute an asynchronous computation until it returns something. Returns None if an unexpected exception occurs or the specified timeout expires.
let retryUntilSomeOrTimeout (timeout:TimeSpan) (retryDelay:TimeSpan) (f:unit -> Async<'T option>) =
    let retryDelay = if retryDelay > timeout then timeout else retryDelay

    let beginPollTime = DateTime.UtcNow
    let endPollTime = beginPollTime + timeout
    let rec loop () =
        async {
            let! r = f()
            match r with
            | None when DateTime.UtcNow <= endPollTime ->
                do! Async.Sleep(int retryDelay.TotalMilliseconds)
                return! loop()
            | Some _ | None ->
                return r
        }
    loop ()


module Synchronization =

    /// Interface for a pool-based synchronization object
    type IPool =
        interface 
            abstract InternalSemaphore : SemaphoreSlim
            abstract AcquireAsync : int option -> Async<System.IDisposable>
            abstract TryAcquireAsync : int option -> Async<System.IDisposable option>
        end

    /// Synchronization object used to limit the total 
    /// number of requests that can be granted concurrently.
    /// Usage:
    ///   let pool = new Pool(5)
    ///   ...
    ///   async {
    ////    use! token = pool.AcquireAsync()
    ///        ...
    ///   }
    type Pool(size:int) =
        let semaphore = new SemaphoreSlim(initialCount=size, maxCount=size)
    
        interface IPool with
            /// Returns the internal semaphore object
            member x.InternalSemaphore with get() = semaphore

            /// Wait until a token from the pool becomes available and acquire it
            /// Return an object that automatically releases the token to the pool when disposed.
            member x.AcquireAsync(?timeout) =
                async {
                    let! token = Async.CancellationToken
                    let! ok = semaphore.WaitAsync(defaultArg timeout -1, token) |> Async.AwaitTask 
                    if ok then
                        return releaseOnDispose semaphore                        
                    else
                        return failwith "Could not acquire a token from the pool"
                }
    
            /// Try acquiring a token from the pool.
            /// On success returns an object that automatically releases the token
            /// once disposed. Returns None on failure to acquire the token.
            member x.TryAcquireAsync(?timeout) =
                async {
                    let! token = Async.CancellationToken
                    let! entered = semaphore.WaitAsync(defaultArg timeout 0, token) |> Async.AwaitTask 
                    if entered then
                        return releaseOnDispose semaphore |> Some
                    else
                        return None
                }

    /// Nested Async pool. Behaves like pool but acquires a resource from a parent pool
    /// before acquring the token from this pool.
    type NestedPool(size:int, parent:IPool) =
        let pool = new Pool(size=size) :> IPool
    
        interface IPool with
            /// Returns the internal semaphore object
            member x.InternalSemaphore with get() = pool.InternalSemaphore

            /// Wait until a token from the parent pool and this pool become available and acquire them.
            /// Return an object that automatically releases the tokens when disposed.
            member x.AcquireAsync(?timeout) =
                async {
                    let! parent = parent.AcquireAsync(timeout)
                    let! this = pool.AcquireAsync(timeout)
                    return
                        { new System.IDisposable with
                             member x.Dispose() =
                                this.Dispose()
                                parent.Dispose()
                         }
                }
    
            /// Try acquiring a token from the parent pool and this pool.
            /// On success returns an object that automatically releases the tokens
            /// once disposed. Returns None on failure to acquire a token from the parent
            /// or from this pool.
            member x.TryAcquireAsync(?timeout) =
                async {
                    let! parent = parent.TryAcquireAsync(timeout)
                    match parent with
                    | None -> return None
                    | Some parentToken ->
                        let! thisToken = pool.TryAcquireAsync(timeout)
                        match thisToken with
                        | None -> parentToken.Dispose()
                                  return None
                        | Some token ->
                            return Some
                                { new System.IDisposable with
                                     member x.Dispose() =
                                        token.Dispose()
                                        parentToken.Dispose()
                                 }
                }


    /// Single-use event object that can be waited on asynchronoulsy
    type SingleUseEvent() =
        let semaphore = new SemaphoreSlim(initialCount=0, maxCount=1)
    
        // Signal the event
        member x.Fire() =
            semaphore.Release() |> ignore
    
        // Wait for the event to occur
        member x.WaitAsync(?timeout) =
            async {
                let! token = Async.CancellationToken
                let! ok = semaphore.WaitAsync((defaultArg timeout -1), token) |> Async.AwaitTask
                if ok then
                    semaphore.Release() |> ignore
                    return ()
                else
                    return failwith "Wait on SingleUseEvent timed-out."
            }

     /// Asynchronous critical section
     type CriticalSection() =
        inherit Pool(1)
        member x.CriticalBlock(f:unit->'A, ?blockName) =
            let description = match blockName with None -> "" | Some name -> " (" + name + ")"
            async {
                //printfn "Entering critical section%s" description
                use! block = (x:> IPool).AcquireAsync None
                //printfn "Critical section entered%s" description
                let ret = f()
                //printfn "Leaving critical section%s" description
                return ret
            }

        member x.CriticalAsyncBlock(task:Async<'A>, ?blockName) =
            let description = match blockName with None -> "" | Some name -> " (" + name + ")"
            async {
                use! block = (x:> IPool).AcquireAsync None
                return! task
            }

/// Asynchronous file copy
let copyFile source target = 
    async {
        use sourceStream = System.IO.File.Open(source, System.IO.FileMode.Open, System.IO.FileAccess.Read, System.IO.FileShare.Read)
        use targetStream = System.IO.File.Open(target, System.IO.FileMode.Create, System.IO.FileAccess.Write)
        let task = sourceStream.CopyToAsync(targetStream)
        return! task |> Async.AwaitIAsyncResult
    }
