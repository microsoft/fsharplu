# FSharpLu F# library

This library provides F# lightweight utilities for string manipulations, logging, collection data structures, file operations, text processing, security, async, parsing, diagnostics, configuration files and Json serialization.

This is by no means a full-fledged utility library for F#, but rather a small collection of utilities and other thin wrappers accumulated throughout the development of various internal projects at Microsoft and meant to facilitate development with the .Net framework using the F# programming language.

Some of the provided utilities are just thin `let`-bindings wrappers around existing .Net libraries (e.g. module `FSharpLu.Text` or `FSharpLu.Parsing`) whereas some provide additional features (e.g. Json serialization in module `FSharpLu.Json`).

## Build status

| Branch | Status |
|--------|--------|
| current status | [![Build status](https://ci.appveyor.com/api/projects/status/y2lrc49c0lxprg77?svg=true)](https://ci.appveyor.com/project/blumu/fsharplu) |
|master | [![Build status](https://ci.appveyor.com/api/projects/status/y2lrc49c0lxprg77/branch/master?svg=true)](https://ci.appveyor.com/project/blumu/fsharplu/branch/master) |
| released builds | [![Build Status](https://dev.azure.com/msrp/Springfield/_apis/build/status/GitHub-repositories/FSharpLu-GitHub-Yaml-BuildAndSign?branchName=master)](https://dev.azure.com/msrp/Springfield/_build/latest?definitionId=653&branchName=master)|

## Build requirements

- F# compiler. See https://fsharp.org/use/Windows and https://fsharp.org/use/linux/

- Install .NET Core SDK from https://dotnet.microsoft.com/download/visual-studio-sdks.

  - .NET Core 2.2 SDK
  - .NET Core 3.0 SDK

- Install .NET Framework Developer Packs from https://www.microsoft.com/net/download/visual-studio-sdks
for the following versions of .NET:

  - .NET Framework 4.5.2
  - .NET Framework 4.6.1
  - .NET Framework 4.6.2
  - .NET Framework 4.7.2

To build project run `dotnet build` under the top-level directory or run the script `scripts\build.ps1`.

## Documentation

For the documentation please visit the [Wiki](https://github.com/Microsoft/fsharplu/wiki)

## License

[MIT](LICENSE.MD)

## Packages

- `FSharpLu`: The core set of utilities
- `FSharpLu.Json`: Json serialization of F# data types implemented as JSon.Net converters and providing more succinct serialization for option types and discriminate unions.
- `FSharpLu.Windows`: Windows-specific utility functions
- `FSharpLu.Azure`: Azure Resource Manager helpers
- Test libraries `FSharpLu.*.Tests`: Unit tests for a specific module

## FSharpLu modules

Here is a list of helper modules provided by FSharpLu.

### Main module

- [FSharpLu.Async](FSharpLu/Async.fs)
- [FSharpLu.AsyncSeq](FSharpLu/AsyncSeq.fs)
- [FSharpLu.CacheMap](FSharpLu/CacheMap.fs)
- [FSharpLu.Configuration](FSharpLu/Configuration.fs)
- [FSharpLu.Collection](FSharpLu/Collections.fs)
- [FSharpLu.Diagnostics](FSharpLu/Diagnostics.fs)
- [FSharpLu.Disposable](FSharpLu/Disposable.fs)
- [FSharpLu.ErrorHandling](FSharpLu/ErrorHandling.fs)
- [FSharpLu.ExecutableInfo](FSharpLu/ExecutableInfo.fs)
- [FSharpLu.File](FSharpLu/File.fs)
- [FSharpLu.FSharpFromCSharp](FSharpLu/FSharpFromCSharp.fs)
- [FSharpLu.Ini](FSharpLu/Ini.fs)
- [FSharpLu.Logger](FSharpLu/Logger.fs)
- [FSharpLu.MachineOperations](FSharpLu/MachineOperations.fs)
- [FSharpLu.Option](FSharpLu/Option.fs)
- [FSharpLu.Parsing](FSharpLu/Parsing.fs)
- [FSharpLu.Platform](FSharpLu/Platform.fs)
- [FSharpLu.QueueScheduler](FSharpLu/QueueScheduler.fs)
- [FSharpLu.Security](FSharpLu/Security.fs)
- [FSharpLu.ServiceRequest](FSharpLu/ServiceRequest.fs)
- [FSharpLu.StateMachine](FSharpLu/StateMachine.fs)
- [FSharpLu.Sysprep](FSharpLu/Sysprep.fs)
- [FSharpLu.Text](FSharpLu/Text.fs)
- [FSharpLu.TraceLogging](FSharpLu/TraceLogging.fs)

### Json

- [FSharpLu.Json](FSharpLu.Json/)

### Azure

- [FSharpLu.Azure](FSharpLu.Azure/)

### Windows

- [FSharpLu.Windows.Hyperv](FSharpLu.Windows/Hyperv.fs)
- [FSHarpLu.Windows.Security](FSharpLu.Windows/Security.fs)
- [FSHarpLu.Windows.Certificate](FSharpLu.Windows/Certificate.fs)
- [FSHarpLu.Windows.Registry](FSharpLu.Windows/Registry.fs)
- [FSHarpLu.Windows.TraceLoggingConsole](FSharpLu.Windows/TraceLoggingConsole.fs)
- [FSHarpLu.Windows.Smb](FSharpLu.Windows/Smb.fs)
- [FSHarpLu.Windows.EtwListener](FSharpLu.Windows/EtwListener.fs)
- [FSHarpLu.Windows.DirectoryServices](FSharpLu.Windows/DirectoryServices.fs)
- [FSHarpLu.Windows.Wmi](FSharpLu.Windows/Wmi.fs)
- [FSHarpLu.Windows.ManagementWin32](FSharpLu.Windows/ManagementWin32.fs)
- [FSHarpLu.Windows.ManagementHypervisor](FSharpLu.Windows/ManagementHypervisor.fs)
- [FSHarpLu.Windows.HyperV](FSharpLu.Windows/HyperV.fs)


## Microsoft Open Source Code of Conduct

This project has adopted the [Microsoft Open Source Code of Conduct](https://opensource.microsoft.com/codeofconduct/). For more information see the [Code of Conduct FAQ](https://opensource.microsoft.com/codeofconduct/faq/) or contact [opencode@microsoft.com](mailto:opencode@microsoft.com) with any additional questions or comments.
