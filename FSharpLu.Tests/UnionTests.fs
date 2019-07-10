module Microsoft.FSharpLu.Union.Tests

open Microsoft.FSharpLu.Parsing.Union

open Microsoft.VisualStudio.TestTools.UnitTesting

type TestInnerCase =
| InnerCase1
| InnerCase2

type TestUnion =
| NoField
| OneField of TestInnerCase
| InvalidTwoField of TestInnerCase*TestInnerCase
| InvalidNotUnionField of string

[<TestClass>]
type UnionParsingTests() =

    [<TestMethod>]
    [<TestCategory("Utilities")>]
    member this.CanSerializeUnionCaseWithNoFields() =
        let testCase = NoField
        let result = WithCases.fieldAsString testCase
        Assert.AreEqual("NoField", result, "Invalid serialized string");

    [<TestMethod>]
    [<TestCategory("Utilities")>]
    member this.CanSerializeUnionCaseWithOneField() =
        let testCase = OneField InnerCase1
        let result = WithCases.fieldAsString testCase
        Assert.AreEqual("OneField/InnerCase1", result, "Invalid serialized string");

    [<TestMethod>]
    [<TestCategory("Utilities")>]
    member this.CannotSerializeUnionCaseWithOneField() =
        let testCase = InvalidTwoField (InnerCase1,InnerCase1)
        try
            let result = WithCases.fieldAsString testCase
            Assert.Fail("Expected an invalidArg exception at this point ")
        with
        | :? System.ArgumentException -> ()
        | _ -> Assert.Fail("Expected an invalidArg exception at this point ")

    [<TestMethod>]
    [<TestCategory("Utilities")>]
    member this.CannotSerializeUnionCaseWithInvalidField() =
        let testCase = InvalidNotUnionField "test"
        try
            let result = WithCases.fieldAsString testCase
            Assert.Fail("Expected an invalidArg exception at this point ")
        with
        | :? System.ArgumentException -> ()
        | _ -> Assert.Fail("Expected an invalidArg exception at this point ")


    [<TestMethod>]
    [<TestCategory("Utilities")>]
    member this.CanDeSerializeUnionCaseWithNoFields() =
        let testCase = "NoField"
        let result = WithCases.tryParseUnion<TestUnion> testCase
        match result  with
        | None -> Assert.Fail(sprintf "Could not deserialize string %s into %s" testCase typeof<TestUnion>.Name)
        | Some du ->
            Assert.AreEqual(NoField, du, "Invalid deserilized DiscriminatedUnion");

    [<TestMethod>]
    [<TestCategory("Utilities")>]
    member this.CanDeSerializeUnionCaseWithOneField() =
        let testCase = "OneField/InnerCase1"
        let result = WithCases.tryParseUnion<TestUnion> testCase
        match result  with
        | None -> Assert.Fail(sprintf "Could not deserialize string %s into %s" testCase typeof<TestUnion>.Name)
        | Some du ->
            Assert.AreEqual(OneField InnerCase1, du, "Invalid deserilized DiscriminatedUnion");




