////
//// File and path management
////
module Microsoft.FSharpLu.File

open System.IO

type Logger<'A,'B> = Logging.Logger<'A,'B>
open Microsoft.FSharpLu.Option
open Microsoft.FSharpLu

/// Path combine infix operator
let (++) x y = System.IO.Path.Combine(x,y)

/// Ensure specified file exists
let public getExistingFile path =
    if System.IO.File.Exists path then
        Some path
    else
        None

/// Ensure specified directory exists
let public getExistingDir path =
    if System.IO.Directory.Exists path then
        Some path
    else
        None

/// Return the size in bytes of a file
let fileLength filePath =
    let f = new System.IO.FileInfo(filePath)
    f.Length

/// Append a line to a text file
let public appendLine filepath line =
    System.IO.File.AppendAllLines(filepath, [line])

/// Append lines to a text file
let public appendLines filepath (lines:seq<string>) =
    System.IO.File.AppendAllLines(filepath, lines)

/// Write lines to a text file
let public writeLines filepath (lines:seq<string>) =
    System.IO.File.WriteAllLines(filepath, lines)

/// Create an empty file
let public createEmptyFile filepath =
    (
        use file = System.IO.File.Create(filepath)
        ()
    )

/// Write to a text file atomically while allowing concurrent reads.
/// Atomicity is guaranteed only if file content is < 8kb
///
// NOTE: An implementation based on System.IO.File.Replace 
// would not guarantee atomicity for files on SMB shares!
// (http://msdn.microsoft.com/en-us/library/windows/desktop/aa365512(v=vs.85).aspx)
let public atomaticWriteAllLines filePath lines =        
    /// Replace content of an existing file atomically using the 
    /// whitespace padding hack.
    let replaceExistingContentWithPaddingHack () =
        // CAUTION: Atomicity breaks if bufferSize < |content to be written|
        let bufferSize = 8192

        // Should not use FileMode.Create otherwise the file will be empty until the next flush.
        use fs = new FileStream(filePath, FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.Read, bufferSize, FileOptions.WriteThrough)
        use streamWriter = new StreamWriter(fs)
        streamWriter.AutoFlush <- false
        let newContent = Text.join streamWriter.NewLine lines

        // If new content is smaller than previous content
        // then we pad the content with spaces to 
        // prevent concurrent readers to see inconsistent content
        // after we flushed and before the file is closed.
        let oldLength = fs.Length |> int64
        streamWriter.Write(newContent)
        let newLength = newContent.Length |> int64
        let diff = oldLength - newLength
        if diff > 0L then
            streamWriter.Write(streamWriter.NewLine)
            streamWriter.Write(Array.create(diff |> int) ' ')
        streamWriter.Flush()
        // Trim the extra padding
        fs.SetLength(newLength)

    // Write content to a temp file in the target directory
    let writeToTempFile () =
        let targetDir = System.IO.Path.GetDirectoryName(filePath)
        let tempFileName = System.Guid.NewGuid().ToString().Replace("-","")
        let tempFile = targetDir ++ tempFileName
        System.IO.File.WriteAllLines(tempFile, lines |> Seq.toArray)
        tempFile

    // Logic differs depending on whether the file exists.
    if System.IO.File.Exists filePath then
        replaceExistingContentWithPaddingHack ()
    else
        // If the file does not exists then the previous logic does not work:
        // creating the file stream will leave the file empty until the file is flushed!
        // Instead we write to a separate file and then atomically rename it.
        let tempFile = writeToTempFile()
        try
            System.IO.File.Move(tempFile, filePath)
        with
            :? System.IO.IOException -> 
                if System.IO.File.Exists filePath then
                    // the target file has just been created by 
                    // another process: let the other process win
                    System.IO.File.Delete tempFile
                else
                    reraise()

/// Read a text file atomically while allowing concurrent writes.
/// Atomicity is guaranteed only if file content is < 8kb
let public atomaticReadAllText filePath =
    let bufferSize = 8192 // CAUTION: if < content to be written then atomicity breaks
    use fs = new FileStream(filePath, FileMode.Open, FileAccess.Read, FileShare.ReadWrite, bufferSize, FileOptions.SequentialScan)
    use streamReader = new StreamReader(fs)
    streamReader.ReadToEnd()

/// Read a text file atomically while allowing concurrent writes.
/// Returns result as array of lines
let public atomaticReadLines filePath =
    atomaticReadAllText filePath |> Text.splitOnString [|"\r\n"|]

/// Parse any batch file containing variable definitions of the following format
///   rem Some comment
///   set VAR=somevalue
///   set VAR2=some other value
let public parseBatchSetFile file =
    file
    |> atomaticReadLines
    |> Seq.filter (Text.startsWith "::" >> not)
    |> Seq.filter (Text.startsWith "rem" >> not)
    |> Seq.filter (Text.startsWith "REM" >> not)
    |> Seq.filter (System.String.IsNullOrWhiteSpace >> not)
    |> Seq.map (Text.skipPrefixCaseInsensitive "set " >> Text.splitOnce '=')
    |> Seq.toList

/// Serialize a sequence of key/value pairs to batch file
let public serializeToBatch filepath keypairs =
    keypairs |> Seq.map (fun (k,v) -> sprintf "set %s=%O" k v)
    |> atomaticWriteAllLines filepath

/// Wait until the specified file exists on disk.
/// Note: the immediate parent directory must already exist
let waitUntilExists (log:Logger<_,_>) filepath =
    async {
        if System.IO.File.Exists filepath then
            log.write "File %s already exists" filepath
            return filepath
        else
            let parentDir = System.IO.Path.GetDirectoryName filepath
            let fileName = System.IO.Path.GetFileName filepath
            use w = new System.IO.FileSystemWatcher(parentDir, fileName, IncludeSubdirectories = false, EnableRaisingEvents = true)

            let waitForFileAsync = 
                Async.Compete 
                    [
                        async {
                            let! v = Async.AwaitEvent w.Created
                            return v.FullPath
                        }
                        async {
                            let! v = Async.AwaitEvent w.Renamed
                            return v.FullPath
                        }
                    ]   
            
            // (a) Potential race condition if the file is created here,
            // taken care of by (b).

            let! waitForFile = Async.StartChild(waitForFileAsync)
            
            /// (b) Check again to handle race condition (a)
            if System.IO.File.Exists filepath then
                return filepath
            else
                log.write "awaiting for %s" filepath
                return! waitForFile
    }
