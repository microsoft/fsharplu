namespace FSharpLu.Windows.AssemblyInfo

open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

// General Information about an assembly is controlled through the following
// set of attributes. Change these attribute values to modify the information
// associated with an assembly.
[<assembly: AssemblyTitle("FSharpLu.Windows")>]
[<assembly: AssemblyDescription("")>]
[<assembly: AssemblyConfiguration("")>]
[<assembly: AssemblyCompany("")>]
[<assembly: AssemblyProduct("FSharpLu.Windows")>]
[<assembly: AssemblyCopyright("Copyright © Microsoft")>]
[<assembly: AssemblyTrademark("")>]
[<assembly: AssemblyCulture("")>]

// Setting ComVisible to false makes the types in this assembly not visible
// to COM components.  If you need to access a type in this assembly from
// COM, set the ComVisible attribute to true on that type.
[<assembly: ComVisible(false)>]

// The following GUID is for the ID of the typelib if this project is exposed to COM
[<assembly: Guid("2e2cdf04-f40a-4a61-a15d-18c2451ccf6e")>]

// Version information for an assembly consists of the following four values:
//
//       Major Version
//       Minor Version
//       Build Number
//       Revision
//
// You can specify all the values or you can default the Build and Revision Numbers
// by using the '*' as shown below:
// [<assembly: AssemblyVersion("0.10.*")>]
[<assembly: AssemblyVersion("0.10.*")>]
[<assembly: AssemblyFileVersion("0.10.*")>]

#if DELAYSIGNING
[<assembly:AssemblyDelaySignAttribute(true)>]
[<assembly:AssemblyKeyFileAttribute(@"..\msft-StrongName.pub")>]
#endif

do
    ()