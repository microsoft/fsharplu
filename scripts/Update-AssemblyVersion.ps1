#.SYNOPOSIS
#   Update the version number in the AssemblyInfo files
#.PARAMETER version
#   Nuget package version number of the form Major.Minor.Patch
param(
    [Parameter(Mandatory=$true)][string]$version
)

$assemblyInfoFiles = Get-ChildItem -Recurse -Filter AssemblyInfo.fs -File $PSScriptRoot\..\

$components = $version -split '\.'
if($components.Length -lt 2 -or ($components.Length -gt 3)) {
    throw "Incorrect version format '$version'. Expecting 'Major.Minor' or 'Major.Minor.Patch'"
}

Write-Host "Nuget package version: $version"

$major = $components[0]
$minor = $components[1]

# We only use the Major and Minor version and let msbuild assign the build and revision numbers
$assemblyVersion = "$major.$minor.*"

Write-Host "Assembly version format: $assemblyVersion"

$assemblyInfoFiles | `
 ForEach-Object { $file = $_.FullName
    $x = Get-Content $file -Raw -Encoding UTF8
    $r = $x -replace '(Assembly.*Version)\(".*"\)', "`$1(`"$assemblyVersion`")"
    $r | Set-Content -Path $file -Encoding UTF8 -NoNewline
}