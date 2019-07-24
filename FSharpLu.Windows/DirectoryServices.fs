/// Active Directory helpers
module Microsoft.FSharpLu.Management.DirectoryServices

open System.DirectoryServices

/// Delete a node in AD.
/// It would be awesome if the .Net DirectoryServices API was async...
let deleteADNode ldapAddress =
    use ad = new DirectoryEntry(ldapAddress)
    // Workaround atttempt for the exception occurring when removing a machine from a domain different from the current user:
    // "DirectoryServicesCOMException (0x8007202B): A referral was returned from the server."
    ad.Options.Referral <- ReferralChasingOption.All
    ad.DeleteTree()

/// Get the LDAP address for the given machine name and domain
let machineLdapAddress machineName domain =
    sprintf "LDAP://CN=%s,OU=ComputersOU,DC=%s,DC=corp,DC=microsoft,DC=com" machineName domain

/// Delete a machine from AD
let deleteMachine machineName domain =
    machineLdapAddress machineName domain |> deleteADNode