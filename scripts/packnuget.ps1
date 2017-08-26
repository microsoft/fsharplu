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
#.PARAMETER version
#   Package version number (following semantic versioning http://semver.org/spec/v2.0.0.html)
#.PARAMETER configuration
#   Build configuration
param(
    [switch]$build,
    [Parameter(ParameterSetName = "pack")][switch]$pack,
    [Parameter(ParameterSetName = "push")][switch]$push,
    [Parameter(ParameterSetName = "push", Mandatory=$true)]$feed,
    [Parameter(ParameterSetName = "push")]$key = 'Springfield',
    [switch]$signed,
    [Parameter(Mandatory=$true)][string]$version,
    [ValidateSet('release', 'debug', IgnoreCase = $true)]$configuration='Release'
)

$ErrorActionPreference = 'stop'
$root = Split-Path -parent $PsScriptRoot
$outputDir = "$root\nugetouput"

Push-Location $root

try {
    if($signed) {
        if($build) {
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
    } elseif($build) {
        & $PSScriptRoot/Update-AssemblyVersion.ps1 -version $version
        & $PSScriptRoot/build.ps1 -configuration $configuration
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
        $file = [string](Get-ChildItem "$outputDir\$name.[0-9]*.[0-9]*.[0-9]*.nupkg" | Sort-Object -Descending -Property LastAccessTime | Select-Object -First 1)
        if(-not $file) {
            throw "Could not locate the nuget package to push under $outputDir"
        }
        Write-Host "Pushing $file" -ForegroundColor Cyan
        if($withdotnet) {
            # Note: `dotnet nuget` does not yet support CredentialProviders!
            dotnet nuget push --source $feed $file --api-key $key
        } else {
            & "$PSScriptRoot\..\.nuget\nuget.exe" push -source $feed -apikey $key $file
        }
    }

    if($signed) {
        checkSigned $PSScriptRoot\..\FSharpLu\bin\release\Microsoft.FSharpLu.dll
        checkSigned $PSScriptRoot\..\FSharpLu.Json\bin\release\Microsoft.FSharpLu.Json.dll
    }

    Write-Host "Packing nuget packages"
    # NOTE: Using --include-symbols to include symbols in Nuget package does not work due to a bug with
    # `dotnet build` where the symbol files gets automatically deleted when the build completes. 
    # There is no known workaround for now.
    dotnet pack -o $outputDir /p:PackageVersion="$version" /p:Configuration=$configuration --no-build $root\FSharpLu\FSharpLu.fsproj 
    dotnet pack -o $outputDir /p:PackageVersion="$version" /p:Configuration=$configuration --no-build $root\FSharpLu.Json\FSharpLu.Json.fsproj 

    if ($push) {
        pushLatest 'Microsoft.FSharpLu'
        pushLatest 'Microsoft.FSharpLu.Json'
    }
} finally {
    Pop-Location
}