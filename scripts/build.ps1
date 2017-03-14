#.SYNOPSIS
#   Build the solution
#.PARAMETER rebuild
#   Rebuild the solution instead of just building
#.PARAMETER delaysign
#   Delay-sign the assemblies after running the build
param(
    [switch]$delaysign,
    [switch]$rebuild
)

$ErrorActionPreference = 'stop'
$root = Split-Path -parent $psScriptRoot
$msbuild = "${env:ProgramFiles(x86)}\MSBuild\14.0\Bin\amd64\msbuild.exe"

if($delaysign) {
    $options = "/p:CompilationSymbols=DELAYSIGNING"
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

& $msbuild $root\FSharpLu.sln /t:$target /p:Configuration=Release /p:Platform="Any CPU" $options

if($delaysign) {
    Write-Host "Assemblies have been built and delay-signed. You can now submit them for strong name signing." -ForegroundColor Green
    exit 0
}
