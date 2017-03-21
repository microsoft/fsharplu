#.SYNOPSIS
#   Build the solution
#.PARAMETER rebuild
#   Rebuild the solution instead of just building
#.PARAMETER delaysign
#   Delay-sign the assemblies after running the build
#.PARAMETER embedJsonNet
#   Embed NewtonSoft JSon.Net DLL in the target assembly. This option can be used to avoid diamond dependency hell issues when using NewtonSoft JSon.Net in a larger project.
param(
    [switch]$delaysign,
    [switch]$rebuild,
    [switch]$embedJsonNet
)

$ErrorActionPreference = 'stop'
$root = Split-Path -parent $psScriptRoot
$msbuild = "${env:ProgramFiles(x86)}\MSBuild\14.0\Bin\amd64\msbuild.exe"
$options = @()

if($embedJsonNet) {
    $options += '/p:EmbedJsonNet=true'
}

if($delaysign) {
    $options += '/p:CompilationSymbols="DELAYSIGNING" '
    if(-not $rebuild) {
        $rebuild = $true
        Write-Warning "Delaying signing requested: forcing rebuild"
    }
}

if($rebuild) {
    $target = 'Rebuild'
} else {
    $target = 'Build'
}

write-host  $msbuild $root\FSharpLu.sln /t:$target /p:Configuration=Release /p:Platform="Any CPU" $options /p:EmbedJsonNet=true
& $msbuild $root\FSharpLu.sln /t:$target /p:Configuration=Release /p:Platform="Any CPU" $options

if($delaysign) {
    Write-Host "Assemblies have been built and delay-signed. You can now submit them for strong name signing." -ForegroundColor Green
    exit 0
}
