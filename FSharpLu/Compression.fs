module Microsoft.FSharpLu.Compression

open System
open System.IO
open Microsoft.FSharpLu.File
open System.IO.Compression

// This is extension of existing ZipFileExtensions class that defines CreateEntryFromFile function
module ZipFileExtensions =

    /// Add contents of a directory to the zip archive preserving relative directory structure
    let createEntryFromDirectory destinationZipFilePath sourceDirectory compressionLevel =
        use zipArchive = System.IO.Compression.ZipFile.Open(destinationZipFilePath, ZipArchiveMode.Update)

        let rec addEntries currentRelativeDirectory =
            (sourceDirectory ++ currentRelativeDirectory)
            |> Directory.EnumerateFiles
            |> Seq.map Path.GetFileName
            |> Seq.iter(fun file ->
                ZipFileExtensions.CreateEntryFromFile (zipArchive, sourceDirectory ++ currentRelativeDirectory ++ file, currentRelativeDirectory ++ file, compressionLevel) |> ignore)

            (sourceDirectory ++ currentRelativeDirectory)
            |> Directory.EnumerateDirectories
            // Do not need the full directory path, only care for the directory name
            |> Seq.map Path.GetFileName
            |> Seq.iter(fun d ->
                addEntries (currentRelativeDirectory ++ d))

        addEntries String.Empty

