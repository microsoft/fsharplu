namespace FSharpLu.Windows.AssemblyInfo

open System.Runtime.InteropServices
open System.Reflection

// Setting ComVisible to false makes the types in this assembly not visible
// to COM components.  If you need to access a type in this assembly from
// COM, set the ComVisible attribute to true on that type.
[<assembly: ComVisible(false)>]

#if DELAYSIGNING
[<assembly:AssemblyDelaySignAttribute(true)>]
[<assembly:AssemblyKeyFileAttribute(@"..\msft-StrongName.pub")>]
#endif
()