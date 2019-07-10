/// Helpers for certificate management and verification
module Microsoft.FSharpLu.Certificate

open System
open System.Security.Cryptography.X509Certificates

/// Result of a validation check
type ValidationResult =
| Success
| Error of string

/// Try to retrieve a certificate with the provided thumbprint from the current user store location
let tryGetCertificate thumbprint =
    let certStore = new X509Store(StoreName.My, StoreLocation.CurrentUser)
    certStore.Open(OpenFlags.ReadOnly)
    let certCollection = certStore.Certificates.Find(X509FindType.FindByThumbprint, thumbprint, false)
    // Get the first cert with the thumbprint
    certCollection
    |> Seq.cast<X509Certificate2>
    |> Seq.tryHead

/// Retrieve the certificate mathcing the thumbprint from the current user store location
let getCertificate thumbprint =
    match tryGetCertificate thumbprint with
    | None -> raise <| new Exception(sprintf "Unable to locate the certificate with thumbprint '%s'" thumbprint)
    | Some certificate -> certificate

/// build a certificate chain to validate a certificate against a known authority
let buildCertificateChain authority =
    let chain = new X509Chain(
                    ChainPolicy = new X509ChainPolicy(
                                        RevocationMode = X509RevocationMode.NoCheck,
                                        RevocationFlag = X509RevocationFlag.ExcludeRoot,
                                        VerificationFlags = X509VerificationFlags.AllowUnknownCertificateAuthority,
                                        VerificationTime = DateTime.Now,
                                        UrlRetrievalTimeout = new TimeSpan(0, 0, 0)
                                    )
                    )
    // This part is very important. You're adding your known root here.
    // It doesn't have to be in the computer store at all. Neither certificates do.
    chain.ChainPolicy.ExtraStore.Add(authority) |> ignore
    chain

/// Validates that the certificate is part of a trusted chain containing the authority
let validateCertificate (authority:X509Certificate2) =
    let chain = buildCertificateChain authority
    fun (certificateToValidate:X509Certificate2) ->
        try
            match chain.Build(certificateToValidate) with
            | false ->
                chain.ChainStatus
                |> Seq.map(fun x -> sprintf "%s %A" (x.StatusInformation.Trim()) x.Status)
                |> String.concat "\n"
                |> sprintf "Trust chain did not complete to the known authority anchor. Errors:\n %s"
                |> ValidationResult.Error
            | true ->
                let isTheAuthorityThumbprintPresentInTheChain =
                    chain.ChainElements
                    |> Seq.cast<X509ChainElement>
                    |> Seq.exists(fun x -> x.Certificate.Thumbprint = authority.Thumbprint)

                match isTheAuthorityThumbprintPresentInTheChain with
                | false ->
                    sprintf "The Root certificate: '%A' is not present in the validation chain of certificate '%A' " authority.Thumbprint certificateToValidate.Thumbprint
                    |> ValidationResult.Error
                | true ->
                    ValidationResult.Success
        finally
            chain.Reset()

#if _COM_REFERENCE_SUPPORTED
open CERTENROLLLib
            
/// Create a new self-signed certificate
/// http://blogs.technet.com/b/vishalagarwal/archive/2009/08/22/generating-a-certificate-self-signed-using-powershell-and-certenroll-interfaces.aspx
/// http://blogs.msdn.com/b/alejacma/archive/2008/09/05/how-to-create-a-certificate-request-with-certenroll-and-net-c.aspx?PageIndex=6
let createSelfSignedCertificate (subjectName:string) (password:string) : X509Certificate2 =
            
    // Create the DN
    let distinguishedName = CX500DistinguishedNameClass()
    distinguishedName.Encode("CN=" + subjectName, X500NameFlags.XCN_CERT_NAME_STR_NONE)
            
    // Create a new private key
    let privateKey =
        CX509PrivateKeyClass(
            ProviderName = "Microsoft Base Cryptographic Provider v1.0",
            MachineContext = false,
            Length = 2048,
            KeySpec = X509KeySpec.XCN_AT_SIGNATURE, // use is not limited
            ExportPolicy = X509PrivateKeyExportFlags.XCN_NCRYPT_ALLOW_PLAINTEXT_EXPORT_FLAG
        )
    privateKey.Create()
            
    // Set hashing algorithm
    let hashobj = CObjectIdClass()
    hashobj.InitializeFromAlgorithmName(ObjectIdGroupId.XCN_CRYPT_HASH_ALG_OID_GROUP_ID,
        ObjectIdPublicKeyFlags.XCN_CRYPT_OID_INFO_PUBKEY_ANY,
        AlgorithmFlags.AlgorithmFlagsNone, "SHA512")
            
    let sslServerObjectId = CObjectIdClass()
    sslServerObjectId.InitializeFromValue("1.3.6.1.5.5.7.3.1") // OID for SSL server
    let objectIdList = new CObjectIdsClass()
    objectIdList.Add(sslServerObjectId)
    let enhancedKeys = new CX509ExtensionEnhancedKeyUsageClass()
    enhancedKeys.InitializeEncode(objectIdList)
            
    // Create the self signing request
    let cert = CX509CertificateRequestCertificateClass()
    cert.InitializeFromPrivateKey(X509CertificateEnrollmentContext.ContextUser, privateKey, System.String.Empty)
    cert.Subject <- distinguishedName
    cert.Issuer <- distinguishedName
    cert.NotBefore <- System.DateTime.Now
    cert.NotAfter <- System.DateTime.Now.AddHours(24.0)
    cert.X509Extensions.Add(enhancedKeys :> IX509Extension :?> CX509Extension)
    cert.HashAlgorithm <- hashobj
    cert.Encode()
            
    // Load the certificate as Base64
    let enroll = CX509EnrollmentClass()
    enroll.InitializeFromRequest(cert)
    enroll.CertificateFriendlyName <- subjectName
    let requestInBase64 = enroll.CreateRequest()
    enroll.InstallResponse(InstallResponseRestrictionFlags.AllowUntrustedCertificate, requestInBase64, EncodingType.XCN_CRYPT_STRING_BASE64, password)
            
    // Create the base64 encoded PKCS#12 private key
    let base64encoded = enroll.CreatePFX(password, PFXExportOptions.PFXExportChainWithRoot)
            
    // Create the certificate object
    X509Certificate2(System.Convert.FromBase64String(base64encoded), password, X509KeyStorageFlags.Exportable)
#else
let createSelfSignedCertificate (subjectName:string) (password:string) : X509Certificate2 =
    failwith "createSelfSignedCertificate no implemented for the target platform."
#endif