namespace Microsoft.FSharpLu.Azure.Test

module Generators =
    open System
    open System.Diagnostics
    open System.Reflection
    open Microsoft.FSharpLu.Logging
    open FsCheck
    open Microsoft.FSharpLu.Azure
    open System.Collections.Generic
    open Microsoft.Azure.Management.Storage

    type Ip =
        | IpV4 of string

    let genIpV4Address =
        gen {
            let! _1, _2, _3, _4 = Gen.four(Gen.choose(0, 255))
            return IpV4(sprintf "%d.%d.%d.%d" _1 _2 _3 _4)
        }

    let genUri =
        gen {
            let! t = Gen.oneof [ gen { return "http://" }
                                 gen { return "https://"} ]
            let! host = Gen.oneof [ gen { let! (IpV4 ip) = genIpV4Address in return ip}
                                    gen { return "azure.com"}
                                  ]
            let! port = Gen.oneof [ gen { return None }
                                    gen { let! p = Gen.choose(0, 65535) in return Some p }
                                  ]

            let s =
                match port with
                | None -> sprintf "%s%s/" t host
                | Some p -> sprintf "%s%s:%d/" t host p

            let! segments = Gen.arrayOf (Arb.Default.NonEmptyString().Generator)
            let xs =
                segments
                |> Array.map(fun x -> x.Get)
                |> Array.map(fun s -> Uri.EscapeUriString s)

            let url =
                if Array.isEmpty xs then
                    s
                else
                    let joined = xs |> Array.reduce(fun s1 s2 -> sprintf "%s/%s" s1 s2)
                    sprintf "%s%s" s joined

            let uri = Uri(url)

            return uri
        }

    let genEndpoints =
        gen {
            let! blob, file, queue, table = Gen.four genUri
            let endpoints = Models.Endpoints(blob = blob.AbsolutePath, file = file.AbsolutePath, queue = queue.AbsolutePath, table = table.AbsolutePath)
            return endpoints
        }

    let genCustomDomain =
        gen {
            let! name = Arb.Default.NonEmptyString().Generator
            let! useSubDomain = Arb.generate<Nullable<bool>>
            let customDomain = Models.CustomDomain(name.Get, useSubDomain)
            return customDomain
        }

    let genSku =
        gen {
            let! name = Gen.elements 
                                  [ Models.SkuName.PremiumLRS
                                    Models.SkuName.PremiumZRS
                                    Models.SkuName.StandardGRS
                                    Models.SkuName.StandardGZRS
                                    Models.SkuName.StandardLRS
                                    Models.SkuName.StandardRAGRS
                                    Models.SkuName.StandardRAGZRS
                                    Models.SkuName.StandardZRS ]
            let! tier = Arb.generate<Nullable<Models.SkuTier>>
            return Models.Sku(name = name, tier = tier)
        }

    let genBlob =
        gen {
            let! enabled = Arb.generate<Nullable<bool>>
            let! lastEnabledTime = Arb.generate<Nullable<DateTime>>

            return Models.EncryptionService(enabled=enabled, lastEnabledTime = lastEnabledTime)
        }

    let genEncryptionServices =
        gen {
            let! blob = genBlob
            return Models.EncryptionServices(Blob = blob)
        }

    let genEncryption =
        gen {
            let! services = genEncryptionServices
            return Models.Encryption(Services = services)
        }

    let genStorageAccount =
        gen {
            let! encryption = genEncryption
            let! sku = genSku
            let! accessTier = Arb.generate<Nullable<Models.AccessTier>>
            let! accessKind = Gen.elements [Models.Kind.BlobStorage
                                            Models.Kind.BlockBlobStorage
                                            Models.Kind.FileStorage
                                            Models.Kind.Storage
                                            Models.Kind.StorageV2]

            let! accessCreationTime = Arb.generate<Nullable<DateTime>>
            let! accessCustomDomain = genCustomDomain
            let! id = Arb.generate<string>
            let! lastGeoFailoverTime = Arb.generate<Nullable<DateTime>>
            let! name = Arb.Default.NonEmptyString().Generator
            let! primaryEndpoints, secondaryEndpoints = Gen.two genEndpoints
            let! location, primaryLocation, secondaryLocation = Gen.three Arb.generate<string>
            let! provisioningState = Arb.generate<Nullable<Models.ProvisioningState>>
            let! statusOfPrimary, statusOfSecondary = Gen.two Arb.generate<Nullable<Models.AccountStatus>>
            let! tp = Arb.generate<string>
            let! tags = Arb.generate<IDictionary<string, string>>

            let acc = Models.StorageAccount( kind = accessKind
                                           , creationTime = accessCreationTime
                                           , customDomain = accessCustomDomain
                                           , tags = tags
                                           , id = id
                                           , lastGeoFailoverTime = lastGeoFailoverTime
                                           , location = location
                                           , name = name.Get
                                           , primaryEndpoints = primaryEndpoints
                                           , primaryLocation = primaryLocation
                                           , provisioningState = provisioningState
                                           , secondaryEndpoints = secondaryEndpoints
                                           , secondaryLocation = secondaryLocation
                                           , statusOfPrimary = statusOfPrimary
                                           , statusOfSecondary = statusOfSecondary
                                           , ``type`` = tp
                                           , sku = sku
                                           , accessTier = accessTier
                                           , encryption = encryption)
            return acc
        }

    let genStorageAccountKey =
        gen {
            let! keyName = Arb.generate<string>
            let! value = Arb.generate<string>
            let! perm = Arb.generate<Nullable<Models.KeyPermission>>
            return Models.StorageAccountKey(keyName = keyName, value = value, permissions = perm)
        }

    let genStorageAccountListKeys =
        gen {
            let! keys = Gen.nonEmptyListOf genStorageAccountKey
            return Models.StorageAccountListKeysResult(keys = Collections.Generic.List(keys))
        }

    let genListResponseAndKeys =
        gen {
            let! reqId = Arb.Default.NonEmptyString().Generator
            let! statusCode = Arb.generate<Net.HttpStatusCode>

            let! accounts = Gen.arrayOf genStorageAccount

            let keys = Dictionary<string, Models.StorageAccountListKeysResult>()
            for r in accounts do
                let! key1, key2 = Gen.two(Arb.Default.NonEmptyString().Generator)
                let! keyList = Gen.nonEmptyListOf genStorageAccountKey
                keys.[r.Name] <- Models.StorageAccountListKeysResult(keys = Collections.Generic.List(keyList))

            return accounts, keys
        }

    type 'a Resources =
        | Resources of 'a

    let genListResourcesAndNames =
        gen {
            let! accounts, keys = genListResponseAndKeys
            let n = accounts |> Seq.length
            let acct = Gen.sample n n (Gen.oneof( [ for a in accounts -> gen {return a.Name} ]))

            let f (_:Auth.Subscription) (_:string) =
                async {
                    let listKeys (_:string, name :string) = keys.[name]
                    return accounts, listKeys
                }
            return Resources(f, accounts, keys, Set.ofList acct)
        }

    let genListResources =
        gen {
            let! listRsp, keys = genListResponseAndKeys
            let f = (fun (_:Auth.Subscription) (_:string) ->
                    async {
                        let listKeys = fun (_:string, name :string)-> keys.[name]
                        return listRsp, listKeys
                    })
            return Resources(f, listRsp, keys)
        }

    type 'a ListResourcesWithTag =
        | ListResourcesWithTag of 'a * (string * string) * Models.StorageAccount[]

    let genListResourcesWithTag =
        gen {
            let! (Resources (f, accounts, keys)) = genListResources
            let! tagKey = Arb.Default.NonEmptyString().Generator
            let! tagValue = Arb.generate<string>

            let accountsWithTag = ResizeArray()

            for x in accounts do
                let! yes = Arb.generate<bool>
                if yes then
                    x.Tags.[tagKey.Get] <- tagValue
                    accountsWithTag.Add(x)
                else
                    x.Tags.Remove(tagKey.Get) |> ignore

            return ListResourcesWithTag(f, (tagKey.Get, tagValue), accountsWithTag.ToArray())
        }

    type Generators =
        static member IpV4() = Arb.fromGen genIpV4Address
        static member Uri() = Arb.fromGen genUri
        static member CustomDomain() = Arb.fromGen genCustomDomain
        static member Endpoints() = Arb.fromGen genEndpoints
        static member StorageAccount() = Arb.fromGen genStorageAccount
        static member ListResources() = Arb.fromGen genListResources
        static member ListResourcesWithTag() = Arb.fromGen genListResourcesWithTag
        static member ResourcesAndNames() = Arb.fromGen genListResourcesAndNames

        static member Skus() = Arb.fromGen genSku
        static member Blobs() = Arb.fromGen genBlob
        static member Encryption() = Arb.fromGen genEncryption
        static member EncryptionServices() = Arb.fromGen genEncryptionServices
        static member StorageAccountKey() = Arb.fromGen genStorageAccountKey
        static member StorageAccountListKeys() = Arb.fromGen genStorageAccountListKeys

    Arb.register<Generators>() |> ignore
