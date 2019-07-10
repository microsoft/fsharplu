namespace Microsoft.FSharpLu.Azure

type Region = string

type Subscription = string

/// Options for waiting functions
type TimeoutOption =
    | Return
    | ThrowOnTimeout