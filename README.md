# FSharpLu F# library

This library provides F# lightweight utilities for string manipulations, logging, collection data structures, file operations, text processing, security, async, parsing, diagnostics, configuration files and Json serialization.

This is by no means a full-fledged utility library for F#, but rather a small collection of utilities and other thin wrappers accumulated throughout the development of various internal projects at Microsoft and meant to facilitate development with the .Net framework using the F# programming language.

Some of the provided utilities are just thin `let`-bindings wrappers around existing .Net libraries (e.g. module `FSharpLu.Text` or `FSharpLu.Parsing`) whereas some provide additional features (e.g. Json serialization in module `FSharpLu.Json`).


## Documentation

For the documentation please visit the [Wiki](https://github.com/Microsoft/fsharplu/wiki)

## License

[MIT](LICENSE.MD)

## Packages

- `FSharpLu`: The core set of utilities
- `FSharpLu.Json`: Json serialization of F# data types implemented as JSon.Net converters and providing more succinct serialization for option types and discriminate unions.
- `FSharpLu.Tests`: Unit tests for the entire solution.

## FSharpLu modules

Here is a list of helper modules provided by FSharpLu. 
- [FSharp.Async](FSharpLu/Async.fs)
- [FSharp.Configuration](FSharpLu/Configuration.fs)
- [FSharp.Collection](FSharpLu/Collections.fs)
- [FSharp.Diagnostics](FSharpLu/Diagnostics.fs)
- [FSharp.ErrorHandling](FSharpLu/ErrorHandling.fs)
- [FSharp.File](FSharpLu/File.fs)
- [FSharp.Logger](FSharpLu/Logger.fs)
- [FSharp.Option](FSharpLu/Option.fs)
- [FSharp.Parsing](FSharpLu/Parsing.fs)
- [FSharp.Security](FSharpLu/Security.fs)
- [FSharp.Text](FSharpLu/Text.fs)
- [FSharp.TraceLogging](FSharpLu/TraceLogging.fs)



## Microsoft Open Source Code of Conduct

This project has adopted the [Microsoft Open Source Code of Conduct](https://opensource.microsoft.com/codeofconduct/). For more information see the [Code of Conduct FAQ](https://opensource.microsoft.com/codeofconduct/faq/) or contact [opencode@microsoft.com](mailto:opencode@microsoft.com) with any additional questions or comments.
