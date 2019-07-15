namespace Microsoft.FSharpLu.Platform

open System
open Microsoft.FSharpLu.Parsing

/// All known OS platforms
type Platform =
    | Windows
    | Linux

    /// Parse platform from a string
    static member Parse (s: String) : Platform =
        match Union.tryParse s with
        | Some p -> p
        | None -> failwithf "Unknown platform: %s" s

    /// Gets the current platform
    static member Current =
        match Environment.OSVersion.Platform with
        | PlatformID.Unix -> Platform.Linux
        | PlatformID.Win32NT -> Platform.Windows
        | _ -> failwithf "platform not supported %A" Environment.OSVersion.Platform

    /// Generic type used to define a platform specific value that only gets evaluated when fetched
    type PlatformSpecificDelayed<'a> =
        {
            WindowsEvaluation : unit -> 'a
            LinuxEvaluation : unit -> 'a
        }
        member inline x.Linux = x.LinuxEvaluation()
        member inline x.Windows = x.WindowsEvaluation()
        member inline x.ForPlatform platform =
            match platform with
            | Linux -> x.Linux
            | Windows -> x.Windows

        member inline x.ForCurrentPlatform =
            x.ForPlatform Platform.Current

    /// Generic type used to define a platform specific value
    type PlatformSpecific<'a> =
        {
            Windows : 'a
            Linux : 'a
        }
        member inline x.ForPlatform platform =
            match platform with
            | Linux -> x.Linux
            | Windows -> x.Windows

        member inline x.ForCurrentPlatform =
            x.ForPlatform Platform.Current

type Environment =
    /// Return the name assigned to the current machine
    /// Note: on Linux, this value is only initialized correctly after network
    /// initialization has completed. We therefore define this value
    /// as a static property instead of a constant.
    static member MachineName
        with get () =
            System.Environment.MachineName

    /// Path to the OS drive root: / on Linux, C:\ on Windows.
    static member OSRootPath
        with get () =
            {
                LinuxEvaluation = fun () -> "/"
                WindowsEvaluation = fun () -> System.IO.Path.GetPathRoot(System.Environment.SystemDirectory)
            }