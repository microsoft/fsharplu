/// Security-related functions
module Microsoft.FSharpLu.Security
#nowarn "9"

open System.Security
open System.Runtime.InteropServices

/// Decrypt a secure string
let convertToUnsecureString (secureString:SecureString) =
    let unmanagedString = Marshal.SecureStringToGlobalAllocUnicode(secureString)
    try 
        Marshal.PtrToStringUni(unmanagedString)
    finally
        if unmanagedString <> System.IntPtr.Zero then
            Marshal.ZeroFreeGlobalAllocUnicode(unmanagedString)

/// Create a secure string
/// Converted from http://blogs.msdn.com/b/fpintos/archive/2009/06/12/how-to-properly-convert-securestring-to-string.aspx,
/// courtesy of F# Discussion <fsharp@microsoft.com>.
let convertToSecureString (s: string) = 
    if s = null then
        raise <| new System.ArgumentNullException("s")
    let gcHandle = GCHandle.Alloc(s,  GCHandleType.Pinned)
    try
        let secureString = new SecureString(NativeInterop.NativePtr.ofNativeInt (gcHandle.AddrOfPinnedObject()), s.Length)
        secureString.MakeReadOnly()
        secureString
    finally 
        gcHandle.Free()