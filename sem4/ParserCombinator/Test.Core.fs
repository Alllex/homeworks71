module ParserCombinators.Tests.Core
open NUnit.Framework
open ParserCombinators.Core

let str2cs (s : string) = List.ofArray <| s.ToCharArray()
let mkres value rest = [(value, str2cs rest)]
let fails =
    function
    | Failure(_) -> true
    | _ -> false


type ``Basic parser operators``() =
    [<Test>]
    member x.``Value tests``() =
        Assert.True(run (value 5) "" = Success(5))
        Assert.True(run (value "abc") "" = Success("abc"))
        Assert.True(run (value 9) "a" |> fails)

    [<Test>]
    member x.``Empty tests``() =
        Assert.True(run empty "" |> fails)
        Assert.True(run empty "abcd" |> fails)

    [<Test>]
    member x.``Symf tests``() =
        Assert.True(run (symf ((=) 'a')) "a" = Success('a'))
        Assert.True(run (symf ((<>) 'a')) "b" = Success('b'))
        Assert.True(run (symf ((=) 'a')) "b" |> fails)
        Assert.True(run (symf (fun c -> List.exists ((=) c) ['a'..'e'])) "d" = Success('d'))

    [<Test>]
    member x.``Sym tests``() =
        Assert.True(run (sym 'a') "a" = Success('a'))
        Assert.True(run (sym 'a') "b" |> fails)
        Assert.True(run (sym 'a') "ax" |> fails)

    [<Test>]
    member x.``Operator (<|>) tests``() =
        Assert.True(run (sym 'b' <|> sym 'a') "a" = Success('a'))
        Assert.True(run (sym 'a' <|> sym 'b' <|> sym 'c') "a" = Success('a'))
        Assert.True(run (sym 'a' <|> sym 'b' <|> sym 'c') "b" = Success('b'))
        Assert.True(run (sym 'a' <|> sym 'b' <|> sym 'c') "c" = Success('c'))
        Assert.True(run (sym 'b' <|> sym 'a') "cx" |> fails)

    [<Test>]
    member x.``Operator (>>=) tests``() =
        Assert.True(run (sym 'b' >>= (fun _ -> value 4)) "b" = Success(4))
        Assert.True(run (sym 'b' >>= (fun _ -> value 4)) "a" |> fails)
        Assert.True(run (sym 'b' >>= (fun x -> sym x)) "bb" = Success('b'))
        Assert.True(run (sym 'b' >>= (fun _ -> sym 'a')) "bx" |> fails)

[<TestFixture>]
type ``All parsers``() =
    [<Test>]
    member x.``Parser (between) tests``() =
        Assert.True(run (between (sym '[') (many <| sym '+') (sym ']')) "[+]" = Success(['+']))

    [<Test>]
    member x.``Parser (opt) tests``() =
        Assert.True(run (opt (sym 'a')) "a" = Success(Some 'a'))
        Assert.True(run (opt (sym 'a')) "" = Success(None))
        Assert.True(run (sym 'a' .>> opt (sym 'b')) "ab" = Success('a'))
        Assert.True(run (sym 'a' .>> opt (sym 'b')) "a" = Success('a'))
        Assert.True(run (sym 'a' .>> opt (sym 'b')) "ba" |> fails)

    [<Test>]
    member x.``Parser (syms) tests``() =
        Assert.True(run (syms ['a';'b';'c']) "a" = Success('a'))
        Assert.True(run (syms ['a';'b';'c']) "b" = Success('b'))
        Assert.True(run (syms ['a';'b';'c']) "c" = Success('c'))
        Assert.True(run (syms ['a';'b';'c']) "d" |> fails)
    
[<TestFixture>]
type ``Integer parsing``() =
        
    [<Test>]
    member x.``One digit integers``() =
        Assert.True(run pdigit "7" = Success('7'))
        Assert.True(run pdigit "0" = Success('0'))
        Assert.True(run pdigit "9" = Success('9'))
        Assert.True(run pdigit "a" |> fails)

    [<Test>]
    member x.``Int parsing``() =
        Assert.True(run pint "42" = Success(42))
        Assert.True(run pint "+42" = Success(42))
        Assert.True(run pint "-7" = Success(-7))
        Assert.True(run pint "0" = Success(0))
        Assert.True(run pint "00" = Success(0))
        Assert.True(run pint "0123" = Success(123))
        Assert.True(run pint "-0" = Success(0))
        Assert.True(run pint "3.14" |> fails)
