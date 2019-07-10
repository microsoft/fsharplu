[<Xunit.Trait("TestCategory", "Keyvault")>]
module Microsoft.FSharpLu.Azure.Test.Keyvault

open System
open Microsoft.FSharpLu.Azure.Vault
open Xunit

[<Fact>]
let ``Parse keyvault URL`` () =
    let x, y, z = parseKeyvaultSecretUrl "https://foo.vault.azure.net:443/secrets/bla/13564s6df"
    Assert.Equal(x, "https://foo.vault.azure.net:443/")
    Assert.Equal(y, "bla")
    Assert.Equal(z, "13564s6df")

[<Fact>]
let ``Parse keyvault URL 2`` () =
    let x, y, z = parseKeyvaultSecretUrl "https://foo.vault.azure.net:443/secrets/bla/13564s6df/"
    Assert.Equal(x, "https://foo.vault.azure.net:443/")
    Assert.Equal(y, "bla")
    Assert.Equal(z, "13564s6df")

[<Fact>]
let ``Parse keyvault URL without version`` () =
    let x, y, z = parseKeyvaultSecretUrl "https://bar.vault.azure.net:443/secrets/foo"
    Assert.Equal(x, "https://bar.vault.azure.net:443/")
    Assert.Equal(y, "foo")
    Assert.Equal(z, "")

[<Fact>]
let ``Parse keyvault URL without version 2`` () =
    let x, y, z = parseKeyvaultSecretUrl "https://bar.vault.azure.net:443/secrets/foo/"
    Assert.Equal(x, "https://bar.vault.azure.net:443/")
    Assert.Equal(y, "foo")
    Assert.Equal(z, "")

[<Fact>]
let ``Throws when parsing incorrect keyvault URLs`` () =
    let _ = Assert.Throws(Action(fun () -> parseKeyvaultSecretUrl "https://bar.vault.azure.net:443/secretsasd/foo/" |> ignore))
    let _ = Assert.Throws(Action(fun () -> parseKeyvaultSecretUrl "https://bar.vault.azure.net:443/secretsasdfoosdf" |> ignore))
    let _ = Assert.Throws(Action(fun () -> parseKeyvaultSecretUrl "https://bar.vault.azure.net:443/secrets/1/2/345" |> ignore))
    ()