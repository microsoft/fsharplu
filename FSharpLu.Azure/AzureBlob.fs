namespace Microsoft.FSharpLu.Azure

/// Blob related helpers
module Blob =
    open Microsoft.Azure.Storage.Blob
    open System
    open Microsoft.FSharpLu.Async
    open FSharp.Control

    module Constants =
        /// Validity period for the SAS token of the temporary location of the job package files
        let TemporaryLocationTokenDuration = TimeSpan.FromDays 1.0

        /// The name of the folder where files are temporary uploaded
        let TemporaryFolderName = "tempPackageUploadStorage".ToLower()

    /// Get reference info to the management service queue
    let getReference (storage:Microsoft.Azure.Storage.CloudStorageAccount) container blobRelativePath =
        let client = storage.CreateCloudBlobClient()
        let container = client.GetContainerReference(container)
        let blob = container.GetBlobReference(blobRelativePath)
        blob

    /// Return blob content specified by its SAS as a stream
    let downloadBlobSasToStream (sasBlobUri:Uri) stream =
        async {
            let blob = new Microsoft.Azure.Storage.Blob.CloudBlob(sasBlobUri)
            do! blob.DownloadToStreamAsync(stream) |> Async.AwaitTask
        }

    /// Return blob content as a stream
    let tryDownloadBlobToStream (storage:Microsoft.Azure.Storage.CloudStorageAccount) container blobRelativePath stream =
        async {
            let client = storage.CreateCloudBlobClient()
            let container = client.GetContainerReference(container)
            let! r = container.CreateIfNotExistsAsync() |> Async.AwaitTask
            let blob = container.GetBlockBlobReference(blobRelativePath)
            let! blobExists = blob.ExistsAsync() |> Async.AwaitTask
            if blobExists then
                do! blob.DownloadToStreamAsync (stream) |> Async.AwaitTask
                return true
            else
                return false
        }

    /// Create a SharedAccessBlobPolicy object that matches the permissions and expiration delay provided
    let createSharedAccessBlobPolicy permissions expirationDelay =
        let now = DateTimeOffset.Now
        new SharedAccessBlobPolicy(
            Permissions = permissions,
            SharedAccessStartTime = Nullable(now.AddMinutes(-5.0)),
            SharedAccessExpiryTime = Nullable(now.Add(expirationDelay)))

    /// Create a shared access URI with the specified permissions
    let generateBlobAccessUri (storage:Microsoft.Azure.Storage.CloudStorageAccount) permissions container blobRelativePath expirationDelay =
        let blob = getReference storage container blobRelativePath

        // Create a temporary URI with push access rights only.
        let policy = createSharedAccessBlobPolicy permissions expirationDelay
        let sas = blob.GetSharedAccessSignature(policy)
        Uri(blob.Uri.AbsoluteUri + sas)

    /// Create a shared access URI with permission to read from a blob
    let generateBlobReadAccessUri (storage:Microsoft.Azure.Storage.CloudStorageAccount) container blobRelativePath expirationDelay =
        generateBlobAccessUri storage SharedAccessBlobPermissions.Read container blobRelativePath expirationDelay

    /// Upload stream to the specified blob
    let getBlobReference (storage:Microsoft.Azure.Storage.CloudStorageAccount) container blobRelativePath=
        async {
            let client = storage.CreateCloudBlobClient()
            let container = client.GetContainerReference(container)
            let! r = container.CreateIfNotExistsAsync() |> Async.AwaitTask
            return container.GetBlockBlobReference(blobRelativePath)
        }

    /// Delete a blob if it exists
    let deleteBlobIfExists (storage:Microsoft.Azure.Storage.CloudStorageAccount) container blobRelativePath =
        async {
            let blob = getReference storage container blobRelativePath
            let! r = blob.DeleteIfExistsAsync() |> Async.AwaitTask
            return r
        }

    /// List all blobs under the specified container with the specified prefix
    let listBlobs (client:CloudBlobClient) container prefix =
        async {
            let container = client.GetContainerReference(container)
            let! exists = container.ExistsAsync().AsAsync
            let! cancellationToken = Async.CancellationToken
            if exists then
                return!
                    Request.PagedResults.
                        enumerateAllImmediateFiles<CloudBlobContainer, ICloudBlob, IListBlobItem, BlobContinuationToken, BlobResultSegment>
                                container
                                (fun container token ->
                                    container.ListBlobsSegmentedAsync(
                                                  prefix = prefix,
                                                  useFlatBlobListing = true, // gets all the blobs in all rcursive directories
                                                  blobListingDetails = BlobListingDetails.Metadata,
                                                  maxResults = System.Nullable(),
                                                  currentToken = token,
                                                  options = BlobRequestOptions(),
                                                  operationContext = null,
                                                  cancellationToken = cancellationToken))
                    |> AsyncSeq.toListAsync
            else
                return List.empty
        }

    /// List all blobs under the specified container with the specified prefix and extension
    let listBlobsWithExtension (client:CloudBlobClient) container prefix extension =
        async {
            let! blobs = listBlobs client container prefix
            return
                blobs
                |> Seq.filter (fun i -> System.String.Compare(System.IO.Path.GetExtension(i.Uri.AbsoluteUri), extension, true) = 0)
        }

    /// Get the sas url of the container storing the temporary files
    let getTemporaryLocationSasUrl (storage:Microsoft.Azure.Storage.CloudStorageAccount) =
        async  {
            let bloblClient = storage.CreateCloudBlobClient()
            let tempFileContainer = bloblClient.GetContainerReference(Constants.TemporaryFolderName)
            let! _ = tempFileContainer.CreateIfNotExistsAsync() |> Async.AwaitTask
            let policy = createSharedAccessBlobPolicy (SharedAccessBlobPermissions.Read ||| SharedAccessBlobPermissions.Write) Constants.TemporaryLocationTokenDuration
            let sas = tempFileContainer.GetSharedAccessSignature(policy)
            return Uri(sprintf "%O" tempFileContainer.Uri + sas)
        }

    /// Upload the provided content to a cloud file
    let uploadFileStream containerSasUrl fileName (contentStream:System.IO.Stream) =
        async  {

            let blobContainer = CloudBlobContainer containerSasUrl
            let cloudBlob = blobContainer.GetBlockBlobReference(fileName)
            do! cloudBlob.UploadFromStreamAsync contentStream |> Async.AwaitTask
            return cloudBlob.Uri
        }