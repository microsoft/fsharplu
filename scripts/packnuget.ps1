# Pack projects as Nuget packages
param(
    $feed,
	$key,
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

if ($push) {
    & $nuget push -source $feed *.nupkg -ApiKey $key
}

popd