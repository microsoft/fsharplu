/// Security and Windows authentication helpers
module Microsoft.FSharpLu.Security

/// Split login of the form "DOMAIN\alias" into domain and alias parts
let private splitLogin (login:string) =
    let aliasPos = login.LastIndexOf('\\')
    if aliasPos > 0 then
        let domain = login.Substring(0, aliasPos)
        let user = login.Substring(aliasPos + 1)
        domain, user
    else
        "", login

/// Equality test for AD identities
let private sameIdentity (d1,a1) (d2, a2) =
    let c x y = System.String.Compare(x, y, System.StringComparison.OrdinalIgnoreCase) = 0
    c a1 a2 && c d1 d2

////
//// Identity impersonation.
//// Ported from https://msdn.microsoft.com/en-us/library/w070t6ka(v=vs.110).aspx
////
open System
open System.Security.Principal
open Microsoft.Win32.SafeHandles
open System.Runtime.ConstrainedExecution
open System.Security.Permissions
open System.Runtime.InteropServices
open System.Security

type SafeTokenHandle private () =
    inherit SafeHandleZeroOrMinusOneIsInvalid(true)

    [<DllImport("kernel32.dll")>]
    [<ReliabilityContract(Consistency.WillNotCorruptState, Cer.Success)>]
    [<SuppressUnmanagedCodeSecurity>]
    static extern [<MarshalAs(UnmanagedType.Bool)>] bool CloseHandle(IntPtr handle)

    override x.ReleaseHandle() =
        CloseHandle(x.handle)

[<DllImport("advapi32.dll", SetLastError = true, CharSet = CharSet.Unicode)>]
extern bool LogonUser(string lpszUsername, string lpszDomain, string lpszPassword, int dwLogonType, int dwLogonProvider, [<Out>] SafeTokenHandle& phToken)

type PROVIDER =
    | DEFAULT = 0
    
type LOGON32 =
    | LOGON_INTERACTIVE       = 2
    | LOGON_NETWORK           = 3
    | LOGON_BATCH             = 4
    | LOGON_SERVICE           = 5
    | LOGON_UNLOCK            = 7
    | LOGON_NETWORK_CLEARTEXT = 8
    | LOGON_NEW_CREDENTIALS   = 9

[<PermissionSetAttribute(SecurityAction.Demand, Name = "FullTrust")>]
let public impersonate (log:Logger.Logger<_,_>) logonType alias domain getPwd f =
    let mutable safeTokenHandle = Unchecked.defaultof<_>
        
    let logonType = defaultArg logonType LOGON32.LOGON_INTERACTIVE
    let returnValue = LogonUser(alias, domain, getPwd(), (int)logonType, (int)PROVIDER.DEFAULT, &safeTokenHandle)
    
    if not returnValue then
        let ret = Marshal.GetLastWin32Error()
        log.write "[WARNING] LogonUser failed with error code : 0x%X" ret
        raise <| new ComponentModel.Win32Exception(ret)
    
    use x = safeTokenHandle
    log.write "Impersonating %s\%s from %s" domain alias (WindowsIdentity.GetCurrent().Name)
    use newId = new WindowsIdentity(safeTokenHandle.DangerousGetHandle())
    let result =
        (use impersonatedUser = newId.Impersonate()
        f())
    log.write "Reimpersonated as %s" (WindowsIdentity.GetCurrent().Name)
    result

[<PermissionSetAttribute(SecurityAction.Demand, Name = "FullTrust")>]
let public impersonateIfNecessary log logonType alias domain getPwd f =
    let currentLogin = WindowsIdentity.GetCurrent().Name |> splitLogin
    if not <| sameIdentity currentLogin (domain, alias) then
        impersonate log logonType  alias domain getPwd f
    else
        f()


