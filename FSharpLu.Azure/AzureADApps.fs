module Microsoft.FSharpLu.Azure.AzureADApps

open Microsoft.FSharpLu.Azure.Auth

/// Client ID used to impersonate the Powershell client
/// See http://stackoverflow.com/questions/30096576/using-adal-for-accessing-the-azure-keyvault-on-behalf-of-a-user
/// http://stackoverflow.com/questions/35572929/acquire-azure-key-vault-secret-using-integrated-security?rq=1
let PowershellApp =
    {
        AzureADApplication.tenantId = "72f988bf-86f1-41af-91ab-2d7cd011db47"
        AzureADApplication.clientId = "1950a258-227b-4e31-a9cf-717495945fc2"
        AzureADApplication.redirectUri = "urn:ietf:wg:oauth:2.0:oob"
    }

let DefaultClientApp = PowershellApp