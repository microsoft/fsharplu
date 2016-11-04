# Pack projects as Nuget packages
param(
    [switch]$skipBuild,
    [Parameter(ParameterSetName = "pack")][switch]$pack,
    [Parameter(ParameterSetName = "push")][switch]$push,
    [Parameter(ParameterSetName = "push", Mandatory=$true)]$feed,
    [Parameter(ParameterSetName = "push")]$key = 'Springfield'
)

$ErrorActionPreference = 'stop'
$root = Split-Path -parent $psScriptRoot
$nuget = "$root\.nuget\nuget.exe"
$msbuild = "${env:ProgramFiles(x86)}\MSBuild\14.0\Bin\amd64\msbuild.exe"

pushd $root

try {
    if(-not $skipBuild) {
        & $msbuild $root\FSharpLu.sln /p:Configuration=Release /p:Platform="Any CPU"
    }

    & $nuget pack $root\FSharpLu\FSharpLu.fsproj -Prop Configuration=Release -Prop VisualStudioVersion=14.0
    & $nuget pack $root\FSharpLu.Json\FSharpLu.Json.fsproj -Prop Configuration=Release -Prop VisualStudioVersion=14.0

    function pushLatest($name) {
        $file = [string](gci "$name.[0-9]*.[0-9].[0-9]*.[0-9]*.nupkg" | sort -Descending -Property LastAccessTime | select -First 1)
        Write-Host "Pushing $file" -ForegroundColor Cyan
        & $nuget push -source $feed $file -ApiKey $key
    }

    if ($push) {
        pushLatest 'Microsoft.FSharpLu'
        pushLatest 'Microsoft.FSharpLu.Json'
    }
} finally {
    popd
}