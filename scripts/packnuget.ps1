#.SYNOPSIS
#   Pack projects as Nuget packages
#.PARAMETER pack
#   Pack the built files in a nuget package
#.PARAMETER push
#   Push the latest build nuget package to the specified feed
#.PARAMETER feed
#   URL to the Nuget feed
#.PARAMETER key
#   Name of the key used to connect to the Nuget feed
#.PARAMETER signed
#   Indicates that the assemblies have been signed and therefore should not be rebuilt prior to packaging
param(
    [switch]$skipBuild,
    [Parameter(ParameterSetName = "pack")][switch]$pack,
    [Parameter(ParameterSetName = "push")][switch]$push,
    [Parameter(ParameterSetName = "push", Mandatory=$true)]$feed,
    [Parameter(ParameterSetName = "push")]$key = 'Springfield',
    [switch]$signed
)

$ErrorActionPreference = 'stop'
$root = Split-Path -parent $PsScriptRoot
$nuget = "$root\.nuget\nuget.exe"

pushd $root

try {
    if($signed) {
        if(-not $skipBuild) {
            Write-Warning "Packing signed assemblies: skipping build..."
        }

        $output1 = Join-Path -Resolve $PSScriptRoot "..\FSharpLu\bin\release\"
        $output2 = Join-Path -Resolve $PSScriptRoot "..\FSharpLu.Json\bin\release\"
        Write-Host @"
Before proceeding please make sure to manually place the signed assemblies under output directories:
    $output1
    $output2
"@
        pause
    } elseif(-not $skipBuild) {
        & $PSScriptRoot/build.ps1
    }

    function checkSigned($assembly) {
        $sn = Get-Command "sn.exe"
        if(-not $sn) {
            throw "Could not located signing tool sn.exe in the system path."
        } else {
            & "$sn" -vf $assembly
            if($LASTEXITCODE -gt 0) {
                throw "Assembly not fully signed: $assembly! Make sure to manually overwrite the assemblies under the buikd output directory with the signed assemblies before running this script."
            }
        }

    }

    function pushLatest($name) {
        $file = [string](gci "$name.[0-9]*.[0-9].[0-9]*.[0-9]*.nupkg" | sort -Descending -Property LastAccessTime | select -First 1)
        Write-Host "Pushing $file" -ForegroundColor Cyan
        & $nuget push -source $feed $file -ApiKey $key
    }

    if($signed) {
        checkSigned $PSScriptRoot\..\FSharpLu\bin\release\Microsoft.FSharpLu.dll
        checkSigned $PSScriptRoot\..\FSharpLu.Json\bin\release\Microsoft.FSharpLu.Json.dll
    }

    & $nuget pack $root\FSharpLu\FSharpLu.fsproj -Prop Configuration=Release -Prop VisualStudioVersion=14.0
    & $nuget pack $root\FSharpLu.Json\FSharpLu.Json.fsproj -Prop Configuration=Release -Prop VisualStudioVersion=14.0

    if ($push) {
        pushLatest 'Microsoft.FSharpLu'
        pushLatest 'Microsoft.FSharpLu.Json'
    }
} finally {
    popd
}