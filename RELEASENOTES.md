- 0.11.4
  - Build with dotnet SDK 2.2.401 and F# 4.6
  - Minor changes in state machine OutcomeLogger<_,_>

- 0.11.3

  - New: Fork and Join (WhenAll, WhenAny) for agent state machines
  - Fix: Under stressed condition, the call to `Async.AwaitEvent` in `startProcessAsync` could hang even if the process does terminate.

- 0.11.2 Add WMI and Hyper providers + minor fixes

  - Add WMI helpers, Win32 management and HyperV modules
  - Sysprep.ps1 file was not copied to build output directory in projects referencing FSharpLu
  - Fix bug in Diagnostics.startProcess where captured stdout/stderr could miss bytes if the process ends too quickly


- 0.11.1 Promote changes from private utility project into FSharp.Lu

    - Add new Microsoft.FSharpLu.Azure module, upgraded to latest version of Azure nugets and migrate from WindowsAzure.Storage to Microsoft.Azure.Storage.
    Includes number of F# helpers to access Azure Compute, Storage, Network, ARM API
    - Add HttpCommunication module, a strongly-typed HTTP client thin library
    - Add support for environment variable and error output capture in `startProcess` API
    - Add Knuth-Morris-Pratt implementation for searching a string in a file stream
    - Add state machine and agent module used to implement long-running operations in services
    - Add Windows/Linux platform helpers
    - Adds the Combine functor for the TraceTags statically-typed global logger
    - Many other utility functions added: SMB, sysprep, parsing, shutdown, EXE type, IDisposable helpers, Compression, certificate, CacheMap colletion...
    - Bump up version to 0.11.0
    - Hardening of startProcess API to handle corner cases (e.g. process ends too quickly or too slowly)