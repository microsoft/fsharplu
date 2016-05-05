# Pack projects as Nuget packages
param(
    $feed,
    $key = 'Springfield',
    [switch]$skipBuild,
    [switch]$push
)

$root = Split-Path -parent $psScriptRoot
$nuget = "$root\.nuget\nuget.exe"
$msbuild = "${env:ProgramFiles(x86)}\MSBuild\14.0\Bin\amd64\msbuild.exe"

pushd $root

if(-not $skipBuild) {
	& $msbuild $root\FSharpLu.sln /p:Configuration=Release /p:Platform="Any CPU"
}

& $nuget pack $root\FSharpLu\FSharpLu.fsproj -Prop Configuration=Release -Prop VisualStudioVersion=14.0
& $nuget pack $root\FSharpLu.Json\FSharpLu.Json.fsproj -Prop Configuration=Release -Prop VisualStudioVersion=14.0

function pushLatest($name) {
    $file = [string](gci "$name.*.*.*.*.nupkg" | sort -Descending -Property LastAccessTime | select -First 1)
    Write-Host "Pushing $file"
    & $nuget push -source $feed $file -ApiKey $key
}

if ($push) {
    pushLatest 'Microsoft.FSharpLu'
    pushLatest 'Microsoft.FSharpLu.Json'
}

popd