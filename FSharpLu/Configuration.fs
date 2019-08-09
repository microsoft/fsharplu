////
//// Accessing settings from .Net app.config configuration
////
module Microsoft.FSharpLu.Configuration

open System.Configuration
open System.Reflection
open System.Runtime.CompilerServices
open Microsoft.FSharpLu

/// Read a configuration value.
/// Default implementation: read from the application configuration manager
let mutable public tryGetConfigValue =
    fun (key:string) ->
        match System.Configuration.ConfigurationManager.AppSettings.Get(key) with
        | null -> None
        | v -> Some v

/// Read a configuration value
let public getConfigValue key =
    tryGetConfigValue key |> Option.orDo (fun () -> invalidOp (sprintf "Configuration key %s missing from config file" key))

/// Set a configuration value
let mutable public setConfigValue =
    fun key value ->
        System.Configuration.ConfigurationManager.AppSettings.Set(key, value)

/// Get an array type value from the configuration file
let public getConfigArray name =
    getConfigValue name
    |> Text.splitNoEmptyEntries [|';'; '\t'; '\n'; '\r'|]
    |> Array.map (Text.trim [|' '|])

/// Use a user-specified .config file
let public loadCustomConfig filePath =
    let configFileMap = ExeConfigurationFileMap(ExeConfigFilename = filePath)
    let config = ConfigurationManager.OpenMappedExeConfiguration(configFileMap, ConfigurationUserLevel.None)
    if isNull config.AppSettings || isNull config.AppSettings.Settings then
        invalidOp (sprintf "Settings missing from config file: %s" filePath)
    let settings = config.AppSettings.Settings
    tryGetConfigValue <- fun (key:string) ->
                            match settings.[key] with
                            | null -> None
                            | v -> Some v.Value

    setConfigValue <- fun (key:string) value ->
                                if not <| isNull settings.[key] then
                                    settings.Remove(key)
                                settings.Add(key, value)
    config

/// Try loading a custom config file
let public tryLoadConfigFile configFile =
    File.getExistingFile configFile
    |> Option.map loadCustomConfig

/// Load configuration from a custom app.config file
let public loadConfigFile configFile =
    tryLoadConfigFile configFile
    |> Option.orDo (fun () -> invalidOp (sprintf "Config file missing: %s " configFile))

/// Try loading the configuration file next to the calling assembly
[<MethodImpl(MethodImplOptions.NoInlining)>]
let tryLoadConfigFileNextToAssembly () =
    let callingAssembly = Assembly.GetCallingAssembly()
    let path = sprintf "%s.config" callingAssembly.Location
    path
    |> File.getExistingFile
    |> Option.map loadCustomConfig
    |> ignore
