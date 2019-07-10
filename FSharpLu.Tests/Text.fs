module Microsoft.FSharpLu.Text.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open System.IO
open Microsoft.FSharpLu.Text

type TestInnerCase =
| InnerCase1
| InnerCase2

type TestUnion =
| NoField
| OneField of TestInnerCase
| InvalidTwoField of TestInnerCase*TestInnerCase
| InvalidNotUnionField of string

[<TestClass>]
type pringfieldUtilitiesFileTests() =

    let createRandomFileWithThumbprint (size:int) (thumbprint:byte[]) =
        let tempFilePath = System.IO.Path.GetTempFileName()
        use stream = new FileStream(tempFilePath, FileMode.Create)
        let random = new System.Random()
        let thumbprintLocation = size/2-thumbprint.Length
        for i = 0 to thumbprintLocation do
            stream.WriteByte (random.Next(255) |> byte) |> ignore
        stream.Write (thumbprint, 0, thumbprint.Length) |> ignore
        for i = thumbprintLocation + 1 to size - 1 do
            stream.WriteByte (random.Next(255) |> byte) |> ignore
        stream.Flush()
        stream.Close()
        tempFilePath

    [<TestMethod>]
    [<TestCategory("Utilities")>]
    member this.KmpBacktrack1() =
        let result = computeKmpBacktrack "aaaaa"B
        Assert.IsTrue([|-1; -1; -1; -1; -1; 4|] = result, "backtrack not computed correctly")

    [<TestMethod>]
    [<TestCategory("Utilities")>]
    member this.KmpBacktrack2() =
        let result = computeKmpBacktrack "ababab"B
        Assert.IsTrue([|-1; 0; -1; 0; -1; 0; 4|] = result, "backtrack not computed correctly")

    [<TestMethod>]
    [<TestCategory("Utilities")>]
    member this.KmpBacktrack3() =
        let result = computeKmpBacktrack "abacabab"B
        Assert.IsTrue([|-1; 0; -1; 1; -1; 0; -1; 3; 2|] = result, "backtrack not computed correctly")

    [<TestMethod>]
    [<TestCategory("Utilities")>]
    member this.KmpBacktrack4() =
        let result = computeKmpBacktrack "aaabaaaaab"B
        Assert.IsTrue([|-1; -1; -1; 2; -1; -1; -1; 3; 3; 2; 4|] = result, "backtrack not computed correctly")

    [<TestMethod>]
    [<TestCategory("Utilities")>]
    member this.KmpBacktrack5() =
        let result = computeKmpBacktrack "ABCDABD"B
        Assert.IsTrue([|-1; 0; 0; 0; -1; 0; 2; 0|] = result, "backtrack not computed correctly")

    [<TestMethod>]
    [<TestCategory("Utilities")>]
    member this.KmpBacktrack6() =
        let result = computeKmpBacktrack "ABACABABC"B
        Assert.IsTrue([|-1; 0; -1; 1; -1; 0; -1; 3; 2; 0|] = result, "backtrack not computed correctly")

    [<TestMethod>]
    [<TestCategory("Utilities")>]
    member this.KmpBacktrack7() =
        let result = computeKmpBacktrack "ABACABABA"B
        Assert.IsTrue([|-1; 0; -1; 1; -1; 0; -1; 3; -1; 3|] = result, "backtrack not computed correctly")

    [<TestMethod>]
    [<TestCategory("Utilities")>]
    member this.KmpBacktrack8() =
        let result = computeKmpBacktrack "PARTICIPATE IN PARACHUTE "B
        Assert.IsTrue([|-1; 0; 0; 0; 0; 0; 0; -1; 0; 2; 0; 0; 0; 0; 0; -1; 0; 0; 3; 0; 0; 0; 0; 0; 0; 0;|] = result, "backtrack not computed correctly")

    [<TestMethod>]
    [<TestCategory("Utilities")>]
    member this.KmpFindBytesInStream1() =
        use stream = new MemoryStream("ABC ABCDAB ABCDABCDABDE"B)
        let result = kmpTryFindFirstBytesInStream stream "ABCDABD"B
        Assert.AreNotEqual(None, result, "byte array not found in stream")
        Assert.AreEqual(Some 15L, result, "byte array not found at correct position in stream")

    [<TestMethod>]
    [<TestCategory("Utilities")>]
    member this.KmpFindBytesInStream2() =
        use stream = new MemoryStream("aaaaaaaaab"B)
        let result = kmpTryFindFirstBytesInStream stream "aaab"B
        Assert.AreNotEqual(None, result, "byte array not found in stream")
        Assert.AreEqual(Some 6L, result, "byte array not found at correct position in stream")

    [<TestMethod>]
    [<TestCategory("Utilities")>]
    member this.KmpFindBytesInStream3() =
        use stream = new MemoryStream("ABC ABCDAB ABCDABCDABCDE"B)
        let result = kmpTryFindFirstBytesInStream stream "ABCDABD"B
        Assert.AreEqual(None, result, "byte array should not found in stream")

    [<TestMethod>]
    [<TestCategory("Utilities")>]
    member this.KmpFindBytesInStream4() =
        use stream = new MemoryStream("aaaaaaaacb"B)
        let result = kmpTryFindFirstBytesInStream stream "aaab"B
        Assert.AreEqual(None, result, "byte array should not found in stream")

    [<TestMethod>]
    [<TestCategory("Utilities")>]
    member this.KmpFindBytesInStream5() =
        use stream = new MemoryStream("abcabcacbcabcaabbccabca"B)
        let result = kmpFindBytesInStream FindOptions.FindAll stream "bca"B
        match result with
        | [1L;4L;8L;11L;20L] ->
            Assert.IsTrue(true)
        | [] ->
            Assert.IsTrue(false,"byte array not found in stream")
        | _ ->
            Assert.IsTrue(false, "byte array not found at correct positions in stream")

    [<TestMethod>]
    [<TestCategory("Utilities")>]
    [<Ignore>]
    member this.KmpFindBytesInLargeStream_200Meg() =
        let filePath = createRandomFileWithThumbprint 200000000 "ABCDABD"B
        let result = fileContainsBytes filePath "ABCDABD"B
        File.Delete(filePath)
        Assert.IsTrue(result, "thumbprint not found in file")

