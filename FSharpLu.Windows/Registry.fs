/// Copyright (c) Microsoft Corporation.
/// Utilities for setting the registry values

[<AutoOpen>]
module Microsoft.FSharpLu.Registry

open Microsoft.FSharpLu.Logging

/// Function to open registry base key and subkey for certain bitness types.
let private openKeyForBitness (hive : Microsoft.Win32.RegistryHive) (keyPath : string) (bitness : ExecutableInfo.ExeType) =
    match bitness with
    | ExecutableInfo.ExeType.x86 ->
        Microsoft.Win32.RegistryKey.OpenBaseKey(hive, Microsoft.Win32.RegistryView.Registry32).OpenSubKey(keyPath, true)
    | ExecutableInfo.ExeType.amd64 ->
        Microsoft.Win32.RegistryKey.OpenBaseKey(hive, Microsoft.Win32.RegistryView.Registry64).OpenSubKey(keyPath, true)
    | _ -> failwith "Unsupported bittness for registry operations!"

/// Set DWORD value under HKEY_CURRENT_USER path.
let setHkcuDwordValue (keyPath:string) (name:string) (value:int) =
    use key = Microsoft.Win32.Registry.CurrentUser.OpenSubKey(keyPath, true)
    if not (isNull key) then
        key.SetValue(name, value, Microsoft.Win32.RegistryValueKind.DWord)
    else
        Trace.info "Key %s missing, registry value not updated." keyPath

/// Set DWORD value under HKEY_LOCAL_MACHINE path.
let setHklmDwordValue (keyPath:string) (name:string) (value:int) =
    use key = Microsoft.Win32.Registry.LocalMachine.OpenSubKey(keyPath, true)
    if not (isNull key) then
        key.SetValue(name, value, Microsoft.Win32.RegistryValueKind.DWord)
    else
        Trace.info "Key %s missing, registry value not updated." keyPath

/// Set DWORD value in registry for specified hive and key according to bitness.
let setRegistryDwordValueEx (hive : Microsoft.Win32.RegistryHive) (keyPath : string) (name : string) (value : int) (bitness:ExecutableInfo.ExeType) =
    use key =
        openKeyForBitness hive keyPath bitness

    if not (isNull key) then
        key.SetValue(name, value, Microsoft.Win32.RegistryValueKind.DWord)
    else
        Trace.info "Key %s missing, registry value not updated." keyPath

/// Delete registry value from specified hive and key regardless the key type
let deleteRegistryValue (hive : Microsoft.Win32.RegistryHive) (keyPath : string) (name : string) (bitness : ExecutableInfo.ExeType) =
    use key =
        openKeyForBitness hive keyPath bitness

    if not (isNull key) then
        key.DeleteValue(name)

/// Function to obtain registry DWORD value from specified hive and key.
let tryGetRegistryDwordValue (hive : Microsoft.Win32.RegistryHive) (keyPath : string) (name : string) (bitness : ExecutableInfo.ExeType) =
    use key =
        openKeyForBitness hive keyPath bitness

    let returnValue =
        if isNull key then
            None
        else
            let value = key.GetValue(name)
            if isNull value then
                None
            else
                Some (value :?> int)

    returnValue
