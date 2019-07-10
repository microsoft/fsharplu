namespace Microsoft.FSharpLu.Azure

open System.IO.Compression
open System
open FSharp.Control
open Microsoft.FSharpLu.Async
open Microsoft.FSharpLu.Logging
open Microsoft.FSharpLu
open Microsoft.Azure.Storage.File
open Microsoft.Azure.Storage.DataMovement
open Microsoft.Azure.Storage

/// Azure Files storage helpers
module Files =
    module Constants =
        /// Validity period of the SAS token created when moving files
        let CopyFileSasTokenDuration = TimeSpan.FromDays 1.0
        /// The number of attempts to start a file copy accros storage accounts
        let FileCopyMaximumAttemps = 3

    type ResourceId = string
    type SecretUrl = string

    /// This type is used to access an Azure File Share
    type AzureFileShareCredentials =
    | Password of string
    | KeyVaultReference of ResourceId * SecretUrl

    /// URL and credentials to an Azure File Share
    type AzureFileShare =
        {
            /// Url to access the File Share
            shareUrl : string
            /// Name of the Storage Account
            storageAccountName : string
            /// Credentials used to access the file share
            credentials : AzureFileShareCredentials
        }

    /// Create a share if it does not exist
    let createShare (storage:CloudStorageAccount) shareName =
        async {
            let client = storage.CreateCloudFileClient()

            let share = client.GetShareReference(shareName)

            let! r = share.CreateIfNotExistsAsync().AsAsync

            return r
        }

    /// Recursively deletes the directory in azure
    let rec deleteCloudDirectory tags (dir:CloudFileDirectory) =
        let rec deleteFileBySegments token =
            async {
                let! fileResultSegment = dir.ListFilesAndDirectoriesSegmentedAsync(token).AsAsync

                if fileResultSegment.Results |> Seq.isEmpty then
                    // deleting the root folder will cause an exception
                    if not <| obj.ReferenceEquals(dir.Parent, null) then
                        do! dir.DeleteIfExistsAsync().AsAsync |> Async.Ignore
                else
                    for f in fileResultSegment.Results do
                        match f with
                        | :? CloudFile as file ->
                            do! file.DeleteIfExistsAsync().AsAsync |> Async.Ignore
                        | :? CloudFileDirectory as d ->
                            // we don't expect a deep file structure so recursing here is fine
                            do! deleteCloudDirectory tags d
                        | _ -> ()
                    // delete the remaining segments
                    return! deleteFileBySegments fileResultSegment.ContinuationToken
            }

        async {
            let! exists = dir.ExistsAsync() |> Async.AwaitTask
            if exists then
                 // null is to get the first segment in ListFilesAndDirectoriesSegmentedAsync
                do! deleteFileBySegments null
            else
                TraceTags.warning "Call to remove cloud directory received before the directory was created." (tags @ ["directory", dir.Uri.AbsoluteUri])
        }

    /// Delete a directory and its subdirectories on Azure Files.
    /// The underlying API call to Azure is synchronous in the sense that the workflow returns
    /// only after the directory is deleted or if a failure occurs.
    let deleteDir tags (storage:CloudStorageAccount) shareName directory =
        async {
            let client = storage.CreateCloudFileClient()

            let share = client.GetShareReference(shareName)

            let! shareExists = share.ExistsAsync() |> Async.AwaitTask
            if shareExists then
                let rootDir = share.GetRootDirectoryReference()
                let dir =
                    if System.String.IsNullOrWhiteSpace(directory) then
                        rootDir
                    else
                        rootDir.GetDirectoryReference(directory)

                do! deleteCloudDirectory tags dir
            else
                TraceTags.warning "Call to remove cloud directory received before the share was created." (tags @ ["directory", directory; "shareName", shareName])
        }

    /// Return a reference to a directory under an Azure share
    /// The directory path may contain subdirectories. e.g. `dir\subdir1\subdir3`
    let private getDirectoryReference (share:CloudFileShare) directoryPath =
        let rootDir = share.GetRootDirectoryReference()
        if System.String.IsNullOrWhiteSpace(directoryPath) then
            rootDir
        else
            rootDir.GetDirectoryReference(directoryPath)

    /// Get the content of a file stored on an Azure Files share
    let public getFileContent (storage:CloudStorageAccount) fileShareName directory filename =
        async {
            let fileClient = storage.CreateCloudFileClient()
            let share = fileClient.GetShareReference(fileShareName)
            let! shareExists = share.ExistsAsync().AsAsync
            if shareExists then
                let dir = getDirectoryReference share directory
                let! dirExists = dir.ExistsAsync().AsAsync
                if dirExists then
                    let file = dir.GetFileReference(filename)
                    if isNull file then
                        return None
                    else
                        let! fileExists = file.ExistsAsync().AsAsync
                        if fileExists then
                            let! text = file.DownloadTextAsync().AsAsync
                            return Some text
                        else
                            return None
                else
                    return None
            else
                return None
        }

    /// Create all the sub directories in a given directory path under an Azure File share
    /// The share is created if it does not exist
    let public createSubdirectories (storage:CloudStorageAccount) fileShareName directoryPath =
        async {
            let fileClient = storage.CreateCloudFileClient()
            let share = fileClient.GetShareReference(fileShareName)
            let! _ = share.CreateIfNotExistsAsync().AsAsync

            let rec create (parent:CloudFileDirectory)=
                function
                | [] -> async.Return()
                | dir::subdirs ->
                     async {
                        let dirRef = parent.GetDirectoryReference(dir)
                        let! _ = dirRef.CreateIfNotExistsAsync().AsAsync
                        return! create dirRef subdirs
                    }

            let rootDir = share.GetRootDirectoryReference()
            let subDirectories = directoryPath |> Text.split [| System.IO.Path.DirectorySeparatorChar; System.IO.Path.AltDirectorySeparatorChar |] |> Array.toList
            return! create rootDir subDirectories
        }

    /// Create a file in Azure Files and sets its content using the specified text.
    /// Overwrite the file if it already exists
    let public setFileContent (storage:CloudStorageAccount) fileShareName directory filename content =
        async {
            let fileClient = storage.CreateCloudFileClient()
            let share = fileClient.GetShareReference(fileShareName)
            let! _ = share.CreateIfNotExistsAsync().AsAsync
            let dir = getDirectoryReference share directory
            let! _ = dir.CreateIfNotExistsAsync().AsAsync
            let file = dir.GetFileReference(filename)
            do! file.UploadTextAsync(content).AsAsync
        }

    /// Enumerate all files in the specified Azure Files share and directory
    let private enumerateAllFilesInternal (storage:CloudStorageAccount) fileShareName directory recursively =
        async {
            let fileClient = storage.CreateCloudFileClient()
            let share = fileClient.GetShareReference(fileShareName)
            let! shareExists = share.ExistsAsync().AsAsync
            if shareExists then
                let dir = getDirectoryReference share directory
                let! dirExists = dir.ExistsAsync().AsAsync
                if dirExists then
                    return Request.PagedResults
                                  .enumerateAllFiles<CloudFileDirectory, CloudFile, IListFileItem, FileContinuationToken, FileResultSegment>
                                        dir true (fun dir token -> dir.ListFilesAndDirectoriesSegmentedAsync(token))
                else
                    return AsyncSeq.empty
            else
                return AsyncSeq.empty
        }

    /// Enumerate all files in the specified Azure Files share and directory
    let public enumerateAllFiles storage fileShareName directory recursively =
        async {
            let! files = enumerateAllFilesInternal storage fileShareName directory recursively
            return files |> AsyncSeq.map (fun (f:CloudFile) -> f.Name)
        }

    /// Defines a relative file mapping from an Azure Files file to another.
    /// This is used to perform file rediection when enumerating files in Azure Files.
    type FileRedirect =
        /// Map to the file itself
        | Self
        /// Map to the file's parent directory
        | Parent of DirFileRedirect
    and DirFileRedirect =
        /// Map to a specified file under the directory
        | File of string
        /// Map to the parent directory
        | Parent of DirFileRedirect
        /// Map to the specified sub-directory
        | Subdir of string * DirFileRedirect

    /// Defines a process to retrieve files from Azure Files and define
    /// where to store each file under the zip archive
    type FileSelector =
        {
            /// List of directories containing files to be selected (files get recursively enumerated under those directories)
            directories : string seq
            /// Filter applied to each files. Return true to keep, false to exclude
            filter : string -> bool
            /// Redirection function called for each enumerated file on Azure Files.
            /// This can be used to fetch a different file from the one that is being enumerated.
            /// A diffrent FileMapper may be returned for each file.
            redirect : string -> FileRedirect
            /// Maps the enumerated file name from Azure Files to
            /// a corresponding target file name.
            rename : string -> string
        }

    exception CannotLocateFileException

    /// Apply a file redirector to a file from Azure Files
    let rec resolveFileRedirect map (cloudFile:CloudFile) =
        if obj.ReferenceEquals(cloudFile, null) then
            raise CannotLocateFileException

        match map with
        | FileRedirect.Self -> cloudFile
        | FileRedirect.Parent m -> resolveDirRedirect m <| cloudFile.Parent

    /// Apply a file redirector to a directory from Azure Files
    and resolveDirRedirect map (cloudDir:CloudFileDirectory) =
        if obj.ReferenceEquals(cloudDir, null) then
            raise CannotLocateFileException

        match map with
        | DirFileRedirect.File f -> cloudDir.GetFileReference(f)
        | DirFileRedirect.Parent m -> resolveDirRedirect m <| cloudDir.Parent
        | DirFileRedirect.Subdir (dir, m) -> resolveDirRedirect m <| cloudDir.GetDirectoryReference(dir)

    /// Retrieve a set of files from Azure Files and write the compressed content to a zip archive
    let addFilesToZipArchive tags (archive:ZipArchive) context fileShareName selector =
        async {
            let filesInAllDirectories =
                selector.directories
                |> Seq.map (fun directory -> enumerateAllFilesInternal context fileShareName directory true)
                |> AsyncSeq.ofSeqAsync
                |> AsyncSeq.concat
                |> AsyncSeq.filter (fun (file:CloudFile) -> selector.filter file.Name)

            for file in filesInAllDirectories do
                try
                    let filePath = file.Parent.Uri.LocalPath.Substring 1
                    let fileName = file.Name
                    let targetFile = resolveFileRedirect (selector.redirect file.Name) file
                    use! targetFileStream = targetFile.OpenReadAsync() |> Async.AwaitTask
                    if targetFile.Name.EndsWith(".zip") then
                        use innerArchive = new ZipArchive(targetFileStream, ZipArchiveMode.Read)
                        let targetSubdirectory = fileName.Replace(".zip", "")
                        for entry in innerArchive.Entries do
                            use sourceStream = entry.Open()
                            use destinationStream = archive.CreateEntry(selector.rename (sprintf "%s/%s" targetSubdirectory entry.Name)).Open()
                            do! sourceStream.CopyToAsync(destinationStream) |> Async.AwaitTask
                    else
                        use entryStream = archive.CreateEntry(selector.rename fileName).Open()
                        do! targetFileStream.CopyToAsync(entryStream) |> Async.AwaitTask
                with
                    CannotLocateFileException ->
                        TraceTags.error "Cannot locate requested file on Azure Files share." (tags@["fileShareName", fileShareName])
        }

    /// Enumerate all files under an Azure Files directory using the specified file selectors,
    /// and send the compressed content to the specified stream.
    let compressFilesToStream tags context fileShareName selectors stream =
        async {
            use archive = new ZipArchive(stream, ZipArchiveMode.Create, true)
            for selector in selectors do
                do! addFilesToZipArchive tags archive context fileShareName selector
                do! stream.FlushAsync() |> Async.AwaitTask
        }

    /// Retrieve all files under a single Azure Files directory, compress it and write the
    /// compressed content to the specified stream.
    let compressDirectoryToStream tags context fileShareName directory filter stream =
        compressFilesToStream
            tags
            context
            fileShareName
            [
                {
                    directories = [directory]
                    filter = filter
                    redirect = fun _ -> Self
                    rename = id
                }
            ]
            stream

    let getAzureFiles (storage:CloudStorageAccount) fileShareName directory recursively =
        async {
            let! files = enumerateAllFilesInternal storage fileShareName directory recursively
            return files |> AsyncSeq.toBlockingSeq
        }
    /// Create the nested directory strucure of the provided share path
    let createCloudShareDirectory (fileClient:CloudFileClient) sharePath =
        let rec createNestedDirectory (parent:CloudFileDirectory) (nestedDirectories) =
            async {
                match nestedDirectories with
                | [] -> return parent
                | topMost :: subDirs ->
                    let directoryRef = parent.GetDirectoryReference topMost
                    let! _ = directoryRef.CreateIfNotExistsAsync() |> Async.AwaitTask
                    return! createNestedDirectory directoryRef subDirs
            }
        async {
            match (sprintf "%s" sharePath).Split(System.IO.Path.AltDirectorySeparatorChar) |> Seq.toList with
            | [] ->
                return failwithf "Invalid share path: %s" sharePath
            | fileShareName :: subDirs ->
                let shareReference = fileClient.GetShareReference fileShareName
                let! _ = shareReference.CreateIfNotExistsAsync() |> Async.AwaitTask
                let root = shareReference.GetRootDirectoryReference()
                let! _ = root.CreateIfNotExistsAsync() |> Async.AwaitTask
                return! createNestedDirectory root subDirs
        }

    /// Upload the provided content to a cloud file
    let uploadFile (storage:CloudStorageAccount) shareFolderPath fileName (contentStream:HttpCommunication.Utils.ReadOnlyClientStream) =
        async  {
            let fileClient = storage.CreateCloudFileClient()
            let! submissionDir = createCloudShareDirectory fileClient shareFolderPath
            let cloudFile = submissionDir.GetFileReference fileName
            do! cloudFile.UploadFromStreamAsync contentStream |> Async.AwaitTask
        }

    let getSasFileToken (cloudFileShare:CloudFileShare) permissions (duration:TimeSpan) =
        let sasConstraints = SharedAccessFilePolicy(
                                Permissions = permissions,
                                SharedAccessExpiryTime = System.Nullable<_>(System.DateTimeOffset.UtcNow.Add(duration))
                            )
        cloudFileShare.GetSharedAccessSignature sasConstraints

    /// Get the CloudFile object reference from a uri
    let getCloudFileFromPath tags (fileClient:CloudFileClient) (sharePath:string) =

        let rec getCloudFile (cloudDirectory:CloudFileDirectory) path =
            match path with
            | []        -> TraceTags.failwith "Invalid sharepath: %O" (tags@["sharePath", sharePath])
            | [ a ]     -> cloudDirectory.GetFileReference a
            | h :: t    -> getCloudFile (cloudDirectory.GetDirectoryReference h) t

        match sharePath.Split([|"/"|], StringSplitOptions.RemoveEmptyEntries) |> Seq.toList with
        | fileShareName :: path ->
            let shareReference = fileClient.GetShareReference fileShareName
            let root = shareReference.GetRootDirectoryReference()
            getCloudFile root path

        | [] -> TraceTags.failwith "Invalid sharepath: %O" (tags@["sharePath", sharePath])

    /// Start the process of copying a file to a specified cloud directory
    let startFileCopy tags (destination:CloudFileDirectory) (sourceFile:Blob.CloudBlob) =
        let fileReference = destination.GetFileReference(sourceFile.Name.Split('/') |> Seq.last)
        let sasFileToken = sourceFile.Container.GetSharedAccessSignature(
                                Blob.SharedAccessBlobPolicy(
                                    Permissions = Blob.SharedAccessBlobPermissions.Read,
                                    SharedAccessExpiryTime = (System.DateTimeOffset.UtcNow.Add(Constants.CopyFileSasTokenDuration) |> System.Nullable<_>))
                                )
        let fileSasUri = Uri(sprintf "%O" sourceFile.Uri + sasFileToken)
        let tags = tags@ ["fileSasUri", fileSasUri.AbsoluteUri; "targetUri", destination.Uri.AbsoluteUri]

        TraceTags.info "Copying file from SAS URL to Azure File destination" tags

        let rec getFileReference remainingAttempts =
            async{
                if remainingAttempts > 0 then
                    // copy across storage account requires the use of SAS token
                    let! x = fileReference.StartCopyAsync(fileSasUri) |> Async.AwaitTask
                    do! fileReference.FetchAttributesAsync() |> Async.AwaitTask
                    if isNull fileReference.CopyState then
                        TraceTags.warning "Failed to retrieve the CopyState after initiating the file copy" tags
                        return! getFileReference (remainingAttempts - 1)
                    else
                        return fileReference
                else
                    return TraceTags.failwith "Unable to copy file from SAS URL to Azure File destination" tags
            }

        getFileReference Constants.FileCopyMaximumAttemps

    /// Copy a cloud file to a specified cloud directory
    let copyFile tags (destination:CloudFileDirectory) (sourceFile:Blob.CloudBlob) =
        let rec waitForCopy (fileReference:CloudFile) =
            async{
                do! fileReference.FetchAttributesAsync() |> Async.AwaitTask
                match fileReference.CopyState.Status with
                | CopyStatus.Success ->
                    return fileReference
                | CopyStatus.Pending ->
                    do! Async.Sleep (int (TimeSpan.FromSeconds 5.0).TotalMilliseconds)
                    return! waitForCopy fileReference
                | copyStatus ->
                    return TraceTags.failwith "Azure file copy failed"
                            (tags@ ["name", fileReference.Name
                                    "copyStatus", sprintf "%A" copyStatus ])
            }

        async{
            let! fileReference = startFileCopy tags destination sourceFile
            return! waitForCopy fileReference
        }

// Excluding from F# Interactive evaluation: The following line in the code below
//  do! TransferManager.UploadDirectoryAsync(sourceDirPath, destinationDir, options, context).AsAsync
// causes an error when evaluating under F# interactive:
//    error FS0192: internal error: convMethodRef: could not bind to method
#if !INTERACTIVE

    /// Copy a local directory to an existing Azure file share.
    let copyDirectoryToShare tags (storage:CloudStorageAccount) sourceDirPath shareName targetDirectory createTargetDirectoryIfNotExists =
        let fileFailedCallback (e:TransferEventArgs) =
            TraceTags.error "File copy transfer failed"
                (tags@[
                        "destination", targetDirectory
                        "source", sourceDirPath
                        "error", e.Exception.Message
                        ])

        async {
            let client = storage.CreateCloudFileClient()

            let share = client.GetShareReference(shareName)
            let! shareExists = share.ExistsAsync().AsAsync
            if not shareExists then
                TraceTags.failwith "Azure Files share does not exist" (tags@["shareName", shareName])

            let rootDir = share.GetRootDirectoryReference()

            let targetIsRootDir = System.String.IsNullOrWhiteSpace(targetDirectory)
            
            let destinationDir : File.CloudFileDirectory =
                if not targetIsRootDir then
                    rootDir.GetDirectoryReference(targetDirectory)
                else
                    rootDir

            if createTargetDirectoryIfNotExists && not targetIsRootDir then
                let! created = destinationDir.CreateIfNotExistsAsync().AsAsync
                if created then
                    TraceTags.info "Created new directory"
                            (tags@["targetUri", destinationDir.Uri.AbsoluteUri])

            let context = DirectoryTransferContext()
            context.FileFailed.Add(fileFailedCallback)

            let options = UploadDirectoryOptions(Recursive = true)

            /// the following line causes an error when evaluating under F# interactive:
            ///  error FS0192: internal error: convMethodRef: could not bind to method
            let! transferStatus = TransferManager.UploadDirectoryAsync(sourceDirPath, destinationDir, options, context).AsAsync

            TraceTags.info "Directory files were uploaded successfully"
                        (tags@[ "transferStatus", sprintf "%A" transferStatus
                                "sourceDirPath", sourceDirPath
                                "targetUri", destinationDir.Uri.AbsoluteUri ])
        }
#endif
