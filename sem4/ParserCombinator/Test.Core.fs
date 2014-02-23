module ParserCombinators.Tests.Core
open NUnit.Framework
open ParserCombinators.Core

[<TestFixture>]
type ``Integer parsing``() =
    [<Test>]
    member x.``Empty string should fail``() =
        Assert.AreEqual((run (empty |>> int) ""), (run pint ""))
        
    [<Test>]
    member x.``One digit integers``() =
        Assert.AreEqual(Success(0, []), (run pint "0"))
        Assert.AreEqual(Success(1, []), (run pint "1"))
        
    [<Test>]
    member x.``Multiple digit integers``() =
        Assert.AreEqual(Success(123, []), (run pint "0123"))
        Assert.AreEqual(Success(123, []), (run pint "123"))
        
    [<Test>]
    member x.``Integer with signs``() =
        Assert.AreEqual(Success(123, []), (run pint "+0123"))
        Assert.AreEqual(Success(123, []), (run pint "+123"))
        Assert.AreEqual(Success(-123, []), (run pint "-0123"))
        Assert.AreEqual(Success(-123, []), (run pint "-123"))
    
    
[<TestFixture>]
type ``Float parsing``() =
        
    [<Test>]
    member x.``Parsing zero``() =
        Assert.AreEqual(Success(0.0, []), (run pfloat "0"))
        Assert.AreEqual(Success(0.0, []), (run pfloat "-0"))
        Assert.AreEqual(Success(0.0, []), (run pfloat "+0"))
        Assert.AreEqual(Success(0.0, []), (run pfloat "-0.0"))
        
    [<Test>]
    member x.``Parsing integers``() =
        Assert.AreEqual(Success(123.0, []), (run pfloat "123"))
        Assert.AreEqual(Success(123.0, []), (run pfloat "+123"))
        Assert.AreEqual(Success(-123.0, []), (run pfloat "-123"))



[<TestFixture>]
type ``Operator tests``() =
    [<Test>]
    member x.``Test of (>>%) operator``() =
        let p = (sym 'a') >>% 7
        Assert.True((run p "a") = Success(7, []))
        Assert.True((run pint "523") = Success(523, []))

    [<Test>]
    member x.``Test of (>>.) operator``() =        
        Assert.True(Success('b', []) = (run ((sym 'a') >>. (sym 'b')) "ab"))
        Assert.True(Success(958, []) = (run ((sym 'a') >>. pint) "a958"))
        Assert.True(Success('x', []) = (run ((many <| pstr "abc") >>. (sym 'x')) "abcabcx"))
       // Assert.True((Failure) = (run ((sym 'a') >>. (sym 'b')) "ac"))
       
    [<Test>]
    member x.``Test of (.>>) operator``() =        
        Assert.True(Success('a', []) = (run ((sym 'a') .>> (sym 'b')) "ab"))
        Assert.True(Success('a', []) = (run ((sym 'a') .>> pint) "a958"))
        Assert.True(Success(5, []) = (run (((many <| pstr "abc") >>% 5) .>> (sym 'x')) "abcabcx"))
        //Assert.True((Failure) = (run ((sym 'a') .>> (sym 'b')) "ac"))

    [<Test>]
    member x.``Test of (>|>>) operator``() =
        let p = (sym 'a') >|>> (sym 'b' >>% ['x'])
        Assert.True((run p "ab") = Success(['a';'x'], []))
        //Assert.True((run p "b") = Failure)

[<TestFixture>]
type ``Primitive parser tests``() =
    [<Test>]
    member x.``Test of (pstr) operator``() =
        let p = (pstr "abc") >>% 7
        Assert.True((run p "abc") = Success(7, []))
        //Assert.True((run p "abb") = Failure)