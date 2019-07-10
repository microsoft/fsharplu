namespace Microsoft.FSharpLu.Ini

open Xunit

[<Trait("TestCategory", "Utilities.Ini")>]
module IniTests =

    let readWriteIsIdentity fileName = 
        // We don't use ReadAllText as it can affect EOL when comparing the strings
        let iniSource = System.IO.File.ReadAllLines(fileName) |> String.concat System.Environment.NewLine
        let config = readConfigurationFile fileName
        let generated = configurationToString config
        Assert.Equal(iniSource, generated)

    [<Fact>]
    let readIniConfigurationFileTest1() =
        let test1 = readConfigurationFile "test1.ini"
        Assert.False(test1 |> hasSection UnnamedSectionName, "did not expect section Unnamed")
        Assert.True(test1 |> hasSection "1Foo", "expected section Foo")
        Assert.True(test1 |> hasSection "2Bar", "expected section Bar")
        let barSection = test1 |> getSection "2Bar"
        Assert.True(test1 |> hasParameterInSection "2Bar" "TraceApplication", "expected name/value for TraceApplication")
        Assert.True(barSection |> hasParameter "AppVerifierArguments", "expected name/value for AppVerifierArguments")
        Assert.True(barSection |> getValueOrDefault "TraceApplication" "" = "C:\APP\CalendarReader.exe", "expected name/value TraceApplication=C:\APP\CalendarReader.exe")
        match test1 |> tryGetValueFromSection "2Bar" "AppVerifierArguments" with
        | Some value when value = "-enable Exceptions Handles Heaps Locks Memory Threadpool TLS" ->
            ()
        | Some _ ->
            Assert.True(false, "Invalid value found for parameter AppVerifierArguments in section Foo")
        | None ->
            Assert.True(false, "expected name/value AppVerifierArguments=-enable Exceptions Handles Heaps Locks Memory Threadpool TLS")

    [<Fact>]
    let readIniConfigurationFileTest2() =
        let test2 = readConfigurationFile "test2.ini"
        Assert.True( test2 |> hasSection UnnamedSectionName, "expected section Unnamed")
        let unnamedSection = test2 |> getSection UnnamedSectionName
        Assert.True(unnamedSection |> hasParameter "TestItGood1", "expected name/value for TestItGood1")
        Assert.True(unnamedSection |> hasParameter "TestItGood2", "expected name/value for TestItGood2")
        Assert.True(unnamedSection |> hasParameter "TestItGood3", "expected name/value for TestItGood3")
        Assert.True(unnamedSection |> hasParameter "TestItGood4", "expected name/value for TestItGood4")
        Assert.True(unnamedSection |> getValueOrDefault "TestItGood1" "" = "No spaces", "expected name/value TestItGood1=No spaces")
        Assert.True(unnamedSection |> getValueOrDefault "TestItGood2" "" = "Leading spaces", "expected name/value TestItGood2=Leading spaces")
        Assert.True(unnamedSection |> getValueOrDefault "TestItGood3" "" = "Trailing spaces", "expected name/value TestItGood3=Trailing spaces")
        Assert.True(test2 |> getValueFromSectionOrDefault UnnamedSectionName "TestItGood4" "" = "Leading and trailing spaces", "expected name/value TestItGood4=Leading and trailing spaces")

    [<Fact>]
    let readIniConfigurationFileTest3() =
        readWriteIsIdentity "test3.ini"

    [<Fact>]
    let readIniConfigurationFileTest4() =
        readWriteIsIdentity "test4.ini"

    [<Fact>]
    let readIniConfigurationFileTest5() =
        readWriteIsIdentity "test5.ini"        
