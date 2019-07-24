#.SYNOPSIS
#   Return the version tag to be assigne to the assemblies and nuget package

# Read version tag from REALESENOTES.md file
Write-Host "Get nuget version tag from release notes"
if($Env:NUGET_PACKAGE_VERSION) {
    $version = $Env:NUGET_PACKAGE_VERSION
    Write-Host "Using version tag from environment variable NUGET_PACKAGE_VERSION: $version"
} else {
    $c = Get-Content "$PSScriptRoot\..\RELEASENOTES.md" -Encoding UTF8
    $firstLine = $c[0]
    $version = $firstLine -replace '- *([^ ]*).*', '$1'
    if(-not $version) {
        throw "Could not extract version tag from RELEASENOTES.md"
    }
    Write-Host "Using version tag extracted from release notes: $version"
}

return $version
