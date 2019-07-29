/// Copyright (c) Microsoft Corporation.
module Microsoft.FSharpLu.Azure.SettingProvider

open Microsoft.FSharpLu.Configuration
open System.Configuration

type SettingProvider =
    {
        getConnectionString : string -> string
        getSetting : string -> string
        tryGetSetting : string -> Option<string>
    }

/// Retrieve connection string from config file (app.config or web.config)
let ConfigFileProvider =
    {
        getConnectionString = fun key ->
            match ConfigurationManager.ConnectionStrings.Item(key) with
            | v when v <> null && v.ConnectionString <> null ->
                v.ConnectionString
            | _ ->
                match getConfigValue key with
                | null -> invalidOp "Connection string missing from app.config/web.config configuration"
                | v -> v

        getSetting = fun settingKey ->
            match getConfigValue settingKey with
            | null -> invalidOp "Setting missing from app.config/web.config configuration"
            | v -> v

        tryGetSetting = tryGetConfigValue
    }
