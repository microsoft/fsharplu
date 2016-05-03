/// Exception and error handling
module Microsoft.FSharpLu.ErrorHandling

/// Return true if the exception matches a given condition or is an aggregate of 
/// at least one exception matching the condition
let rec matchAggregatedException (exceptionMatch:System.Exception -> 'a option) (e:System.Exception) =
    match e with
    | :? System.AggregateException as e -> Seq.tryPick (matchAggregatedException exceptionMatch) e.InnerExceptions
    | e -> exceptionMatch e

/// Active pattern used to match exceptions that are part of a
/// larger aggregated exception
let (|IsAggregateOf|_|) = matchAggregatedException

/// Determine if an exception is of a given type
/// Together with the active pattern above this
/// allows one to write matching expression of the form
///     try ...
///     with IsAggregateOf someExceptionOfType<System.IndexOutOfRangeException> -> ...
let inline someExceptionOfType< ^t when ^t :> System.Exception> (e:System.Exception) =
    match e with
    | :? 't as ex -> Some ex
    | _ -> None

    