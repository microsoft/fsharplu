#.SYNOPSIS
#   Build the solution
#.PARAMETER rebuild
#   Rebuild the solution instead of just building
#.PARAMETER delaysign
#   Delay-sign the assemblies after running the build
#.PARAMETER configuration
#   Build configuration
param(
    [switch]$delaysign,
    [switch]$rebuild,
    [ValidateSet('release', 'debug', IgnoreCase = $true)]
    $configuration='Release'
)

$ErrorActionPreference = 'stop'

if($delaysign) {
    $options = "/p:DELAYSIGNING=True"
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

dotnet build --no-incremental /t:$target /p:Configuration="$configuration" $options #/v:detailed

if($delaysign) {
    Write-Host "Assemblies have been built and delay-signed. You can now submit them for strong name signing." -ForegroundColor Green
    exit 0
}
