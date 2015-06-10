# pack with:
param($nugetFeed)

$root = Split-Path -parent $psScriptRoot
pushd $root
$nuget = "$root\.nuget\nuget.exe"
$msbuild = "${env:ProgramFiles(x86)}\\MSBuild\12.0\Bin\amd64\msbuild.exe"

& $msbuild $root\FSharpLu.sln /p:Configuration=Release /p:Platform="Any CPU"
& $nuget pack $root\src\FSharpLu.fsproj -Prop Configuration=Release

if ($nugetFeed) {
    & $nuget push -Source $nugetFeed *.nupkg
}

popd