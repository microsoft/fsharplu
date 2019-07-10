
module Microsoft.FSharpLu.ExecutableInfo

open Microsoft.FSharpLu.Logging
open Microsoft.FSharpLu.Diagnostics

/// Supported executable types
type ExeType =
    | Unknown = -1
    | x86 = 0
    | amd64 = 1
    | x16 = 2
    | com = 3

/// Returns the type of a Linux executable file - e.g. 32 or 64-bit.
let getElfExecutableType (executablePath:string) =
    (*
        Sample command and output of detecting the bitness of a file:
        file -b /usr/lib/grub/i386-pc/efiemu32.o
        ELF 32-bit LSB relocatable, Intel 80386, version 1 (SYSV), not stripped

        file -b /usr/lib/grub/i386-pc/efiemu64.o
        ELF 64-bit LSB relocatable, x86-64, version 1 (SYSV), BuildID[sha1]=b6c9061b8de69870da3b3bf449728c33ad9c69d3, not stripped
    *)
    // NOTE: Non-top calls to Async.RunSynchronously like this one are wrong and can cause deadlocks.
    // This will need to either be ported to Async or use a synchronous implementation of startProcess.
    let fileInfo = Process.startProcessAsync
                            "file"
                            (sprintf "-b \"%s\"" executablePath)
                            (System.IO.Directory.GetCurrentDirectory())
                            Process.ProcessStartFlags.RedirectStandardOutput
                            Process.ProcessTimeoutAction.NoTimeout
                            None
                   |> Async.RunSynchronously

    if fileInfo.StandardOutput.StartsWith("ELF 32-bit") then
        Trace.info "32 bit executable: %s" executablePath
        ExeType.x86
    elif fileInfo.StandardOutput.StartsWith("ELF 64-bit") then
        Trace.info "64 bit executable: %s" executablePath
        ExeType.amd64
    else
        Trace.info "Failed to detect executable bitness: %s. File info output: %s" executablePath fileInfo.StandardOutput
        ExeType.Unknown

/// Returns the type of a Windows executable file - e.g. 32 or 64-bit.
let getPeExecutableType (executablePath:string) =
    try
        use stream = new System.IO.FileStream(executablePath,
                                                System.IO.FileMode.Open,
                                                System.IO.FileAccess.Read,
                                                System.IO.FileShare.Read)
        let endian = if System.BitConverter.IsLittleEndian then id else Array.rev
        let bytes = Array.zeroCreate<byte> 4
        if stream.Length >= 64L && stream.Read(bytes, 0, 2) = 2 && bytes.[0] = 0x4Duy && bytes.[1] = 0x5Auy then
            if stream.Seek(0x3CL, System.IO.SeekOrigin.Begin) = 0x3CL && stream.Read(bytes, 0, 4) = 4 then
                let bytes = endian bytes
                let peHeaderOffset = System.BitConverter.ToUInt32(bytes, 0)
                if stream.Length >= int64 (peHeaderOffset + 6u) &&
                    stream.Seek(int64 peHeaderOffset, System.IO.SeekOrigin.Begin) = int64 peHeaderOffset &&
                    stream.Read(bytes, 0, 4) = 4 &&
                    bytes.[0] = 0x50uy && bytes.[1] = 0x45uy && bytes.[2] = 0uy && bytes.[3] = 0uy then
                    if stream.Read(bytes, 0, 2) <> 2 then
                        ExeType.Unknown
                    else
                        let machineType = System.BitConverter.ToUInt16(endian bytes, 0)
                        match machineType with
                            | 0x014Cus -> ExeType.x86
                            | 0x0200us | 0x8664us -> ExeType.amd64
                            | _ -> ExeType.Unknown
                else
                    ExeType.x16
            else
                ExeType.x16
        else
            if System.IO.Path.GetExtension(executablePath).ToUpper() = ".COM" then
                // 16-bit .COM files may not have an MS-DOS header.  We'll assume that any .COM file with no header
                // is a 16-bit executable, even though it may technically be a non-executable file that has been
                // given a .COM extension for some reason.
                ExeType.com
            else
                ExeType.Unknown

    with
    /// TODO: Remove this blanket exception handler and properly handle any expected parsing error instead.
    | e ->
        Trace.error "Exception occurred while determining format of file %s %O" executablePath e
        ExeType.Unknown
