/// WMI wrapper utilities
/// Ported and adapated from
///   http://msdn.microsoft.com/en-us/library/hh850032(v=vs.85).aspx
module Microsoft.FSharpLu.Management.Wmi

open System.Management
open Microsoft.FSharpLu.Disposable

/// WMI job states
type JobState =
    | New = 2
    | Starting = 3
    | Running = 4
    | Suspended = 5
    | ShuttingDown = 6
    | Completed = 7
    | Terminated = 8
    | Killed = 9
    | Exception = 10
    | CompletedWithWarnings = 32768

/// InvokeMethod return codes
type ReturnCode =
    | Success = 0
    | Started = 4096 // Parameters Checked - Transition Started
    | AccessDenied = 32769
    | InvalidState = 32775

/// Escape special characters occurring in WIM queries
let escapePath (path:string) = path.Replace(@"\", @"\\")

/// Typed helper to read a property value
let inline getValue< ^T> (param:string) (o:ManagementBaseObject) =
    o.[param] :?> ^T

/// Convert WMI error code to enum
let returnValue (outParams:ManagementBaseObject) =
    getValue<uint32> "ReturnValue" outParams |> int32 |> enum<ReturnCode>

/// Returns the first ManagementObject in a collection
let first : ManagementObjectCollection -> ManagementObject =
    Seq.cast<ManagementObject> >> Seq.head

/// Returns the first ManagementObject or None if the collection is empty
let inline tryFirst (collection: ManagementObjectCollection) =
    if collection.Count = 0 then
        None
    else
        collection |> Seq.cast<ManagementObject> |> Seq.head |> Some

/// Typed helper for the GetRelated WMI method
let inline getRelated<'t> (relatedClass:string) (mo:ManagementObject) =
    mo.GetRelated relatedClass |> Seq.castAsDisposable<'t>

/// Typed helper for the GetInstances WMI method
let inline getInstances (mc:ManagementClass) =
    mc.GetInstances() |> Seq.castAsDisposable<ManagementObject>

/// Returns the first instance of a given WMI class
let getFirstInstance (scope:ManagementScope) (className:string) =
    use serviceClass = new ManagementClass(className, Scope = scope)
    use instances = getInstances serviceClass
    Seq.head instances

/// Typed helper for the GetRelationships WMI method
let getRelationships (relationshipClass:string) (mo:ManagementObject) =
    mo.GetRelationships(relationshipClass) |> Seq.castAsDisposable<ManagementObject>

/// GetRelationships WMI method composed with Seq.head
let findRelationship (relationshipClass:string) filter (vm:ManagementObject) =
    use r = vm.GetRelationships(relationshipClass) |> Seq.castAsDisposable<ManagementObject>
    r
    |> Seq.filterDispose filter
    |> Seq.head

let inline getRelatedRelationships (relatedClass:string) (relationshipClass:string) (relatedRole:string) (thisRole:string) (o:ManagementObject) =
    let related = o.GetRelated(relatedClass, relationshipClass, null, null, relatedRole, thisRole, false, null)
    related |> Seq.cast<ManagementObject>

/// GetRelated WMI method composed with Seq.head
let inline firstRelatedRelationshipRole (relatedClass:string) (relationshipClass:string) (relatedRole:string) (thisRole:string) (o:ManagementObject) =
    use related = o.GetRelated(relatedClass, relationshipClass, null, null, relatedRole, thisRole, false, null)
    related |> Seq.cast<ManagementObject> |> Seq.head

/// GetRelated WMI method composed with Seq.head
let inline firstRelatedRelationship (relatedClass:string) (relationshipClass:string) (o:ManagementObject) =
    firstRelatedRelationshipRole relatedClass relationshipClass null null o

/// GetRelated WMI method composed with Seq.head
let inline firstRelated (relatedClass:string) (o:ManagementObject) =
    firstRelatedRelationship relatedClass null o

/// GetRelated WMI method composed with tryFirst
let inline tryFirstRelated (relatedClass:string) (o:ManagementObject) =
    use related = o.GetRelated(relatedClass, null, null, null, null, null, false, null)
    related |> tryFirst

/// GetRelated WMI method composed with Seq.findDispose
let findRelated (relatedClass:string) condition (o:ManagementObject) =
    use related = o.GetRelated(relatedClass)
    related |> Seq.cast<ManagementObject> |> Seq.findDispose condition

/// GetRelated WMI method composed with Seq.tryFindDispose
let tryFindRelated (relatedClass:string) condition (o:ManagementObject) =
    use related = o.GetRelated(relatedClass)
    related |> Seq.cast<ManagementObject> |> Seq.tryFindDispose condition

/// Typed helper for the Get WMI method
let get (mos:ManagementObjectSearcher) =
    mos.Get() |> Seq.castAsDisposable<ManagementObject>

/// Internal helper used to invoke a WMI method
let inline invoke_internal (wmiObject:ManagementObject) methodName paramPairs =
    use inParams = wmiObject.GetMethodParameters(methodName)
    Seq.iter (fun (p,v) -> inParams.[p] <-  v) paramPairs
    wmiObject.InvokeMethod(methodName, inParams, null)

/// Returns true if the job state indicates that the job completed.
let isJobComplete (jobStateObj:ManagementObject) =
    let jobState = enum<JobState>(int32 (getValue<uint16> "JobState" jobStateObj))
    (jobState = JobState.Completed) ||
    (jobState = JobState.CompletedWithWarnings) ||
    (jobState = JobState.Terminated) ||
    (jobState = JobState.Exception) ||
    (jobState = JobState.Killed)

/// Returns true if the job state indicates that the job succeeded.
let isJobSuccessful (jobStateObj:ManagementObject) =
    let jobState = enum<JobState>(int32 (getValue<uint16> "JobState" jobStateObj))
    (jobState = JobState.Completed) || (jobState = JobState.CompletedWithWarnings)

/// Validates the output parameters of a WMI method call method call (outputParameters)
/// calling a user-defined functions in case of errors.
/// Returns true if job completed successfully;
/// Throws an exception if job failed immediately
/// If job gave an error then return the result of the custom job error processing function.
/// Adapted from the C# version at http://msdn.microsoft.com/en-us/library/hh850032(v=vs.85).aspx
let validateOutput (log:Microsoft.FSharpLu.Logger.Logger<_,_>) (scope:ManagementScope) customErrorProcessing (outputParameters:ManagementBaseObject) =
    async {
        match returnValue outputParameters with
        | ReturnCode.Success -> return true
        | ReturnCode.Started ->
            // The method invoked an asynchronous operation. Get the Job object
            // and wait for it to complete. Then we can check its result.
            use job = new ManagementObject(getValue<string> "Job" outputParameters)
            job.Scope <- scope

            while isJobComplete job |> not do
                do! Async.Sleep(1000)
                // ManagementObjects are offline objects. Call Get() on the object to have its
                // current property state.
                job.Get()
            done

            if isJobSuccessful job then
                return true
            else
                // In some cases the Job object can contain helpful information about
                // why the method call failed. If it did contain such information,
                // use it instead of a generic message.
                let errorDescription =
                    let err = getValue<string> "ErrorDescription" job
                    if err <> null then err else ""
                let errorCode = getValue<uint16> "ErrorCode" job
                return customErrorProcessing log job errorCode errorDescription

        | _  as error ->
            return raise (new ManagementException(sprintf "The WMI method call failed with code %O" error))
    }

/// Invoke a WMI method synchronously and validate the output
let private invokeSyncValidate log scope (wmiObject:ManagementObject) errorProcessing methodName paramPairs =
    let outParams = invoke_internal wmiObject methodName paramPairs
    let result = validateOutput log scope errorProcessing outParams |> Async.RunSynchronously
    result, outParams

/// Invoke a WMI method, validate the result and return it.
/// Throw an exception if the output cannot be validated.
let public invokeSync log scope (wmiObject:ManagementObject) errorProcessing methodName paramPairs =
    let success, out = invokeSyncValidate log scope wmiObject errorProcessing methodName paramPairs
    assert success
    out

/// Execute a WMI query. Note: each element from the
/// returned collection that is enumerated by the caller must be disposed.
/// Non-enumerated element do not need to be disposed.
let inline query (scope:ManagementScope) wqlQuery =
    let query = new SelectQuery(wqlQuery)
    use searcher = new ManagementObjectSearcher(scope, query)
    searcher.Get()

/// Execute a query and return the first match or None if there is no result.
let inline tryQuerySingle (scope:ManagementScope) wqlQuery =
    use collection = query scope wqlQuery
    if collection.Count = 0 then
        None
    else
        collection |> first |> Some

/// Execute a query and return the first match
let inline querySingle (scope:ManagementScope) wqlQuery =
    use collection = query scope wqlQuery
    if collection.Count = 0 then
        invalidOp (sprintf "WMI query returned not result: %s" wqlQuery)
    else
        collection |> first

/// Returns the list of all objects with the specified class name
let getAllClassObjectsAsList (scope:ManagementScope) className  =
    let queryCommand = sprintf "SELECT * FROM %s" className
    use collection = query scope queryCommand
    // When the function returns the collection object will be disposed
    // (but not the elements it contains).
    // Converting the enumeration to a list forces enumeration of the entire collection
    // and allows the caller to enumerate the elements without accessing the
    // original collection.
    collection |> Seq.cast<ManagementObject> |> Seq.toList |> Seq.ofList

/// Default connection options
let defaultConnectionOptions () =
    new ConnectionOptions(Authentication = AuthenticationLevel.Default, Impersonation = ImpersonationLevel.Impersonate)
