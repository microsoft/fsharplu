#.SYNOPOSIS
#   Update the version number in the fsproj files (AssemblyInfo/nuget) and YAML build file
#.PARAMETER version
#   Nuget package version number of the form Major.Minor.Patch
param(
    [Parameter(Mandatory=$true)][string]$version
)

$components = $version -split '\.'
if($components.Length -lt 2 -or ($components.Length -gt 3)) {
    throw "Incorrect version format '$version'. Expecting 'Major.Minor' or 'Major.Minor.Patch'"
}

Write-Host "Nuget package version: $version"

$major = $components[0]
$minor = $components[1]

Write-Host "Assembly version format: $assemblyVersion"
# We only use the Major and Minor version and let msbuild assign the build and revision numbers
$assemblyVersion = "$major.$minor.*"

$assemblyInfoFiles = Get-ChildItem -Recurse -Filter *.fsproj -File $PSScriptRoot\..\
$assemblyInfoFiles | `
  ForEach-Object { $file = $_.FullName
    $x = Get-Content $file -Raw -Encoding UTF8
    $r = $x -replace '<AssemblyVersion>[ \d.`*]*([-<]?)', "<AssemblyVersion>$assemblyVersion`$1" `
            -replace '<Version>[ \d.`*]*([-<]?)', "<Version>$version`$1" `
            -replace '<PackageVersion>[ \d.`*]*([-<]?)', "<PackageVersion>$version`$1"
    $r | Set-Content -Path $file -Encoding UTF8 -NoNewline
  }

Write-Host "Update version in YAML: $version"
Get-ChildItem -Filter *.yml -File $PSScriptRoot\.. | `
ForEach-Object { $file = $_.FullName
    $x = Get-Content $file -Raw -Encoding UTF8
    $r = $x -replace 'version: [ \d.`*]*([-<]?).{build}', "version: $major.$minor.{build}" `
            -replace 'NUGET_PACKAGE_VERSION: [ \d.`*]*([-<]?)', "NUGET_PACKAGE_VERSION: $version`$1"
    $r | Set-Content -Path $file -Encoding UTF8 -NoNewline
}