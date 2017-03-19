// include Fake lib
#r @".\packages\FAKE.4.56.0\tools\FakeLib.dll"
open Fake

// Properties
let buildDir = "./build/"
let testDir  = "./FSharpLu.Tests/"
let packagingRoot = "./nuget/"

// Targets
Target "Clean" (fun _ ->
    CleanDir buildDir
)

Target "Default" (fun _ ->
    trace "Hello World from FAKE"
)

Target "Build" (fun _ ->
    !! "**/*.fsproj"
        |> MSBuildDebug "" "Build"
        |> Log "AppBuild-Output: "
)


Target "BuildDelaySign" (fun _ ->
    !! "**/*.fsproj"
        |> MSBuild "" "Build" ["CompilationSymbols","DELAYSIGNING"]
        |> Log "AppBuild-Output: "
)


Target "Test" (fun _ ->
    !! (testDir + "**/*.Tests.dll")
        |> MSTest.MSTest (fun p -> p)
)


Target "CreatePackage" (fun _ ->
    // Copy all the package files into a package folder
    //CopyFiles packagingDir allPackageFiles

    NuGet (fun p ->
        {p with
            Authors = ["$author$"]
            Project = "FSharpLu"
            //Description = projectDescription
            OutputPath = packagingRoot
            Properties = ["Configuration","Release"; "VisualStudioVersion","14.0"]
            //Summary = projectSummary
            //WorkingDir = packagingDir
            Version = "$version$"
            //AccessKey = myAccesskey
            //Publish = true
             })
            @"FSharpLu\FSharpLu.fsproj"

    NuGet (fun p ->
        {p with
            OutputPath = packagingRoot
            Properties = ["Configuration","Release"; "VisualStudioVersion","14.0"]
             })
            @"FSharpLu.Json\FSharpLu.Json.fsproj"
)

// Dependencies
"Clean"
    ==> "Default"

"Build"
    ==> "Test"

// start build
RunTargetOrDefault "Default"