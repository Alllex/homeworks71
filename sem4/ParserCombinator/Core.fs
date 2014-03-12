
module ParserCombinators.Core

open System

let chars2str cs = string (new System.String (cs |> List.toArray))
let chars2int s = int (new System.String(s |> List.toArray))
let str2chars (s : string) = List.ofArray <| s.ToCharArray()     

type ParserInfo(str : char list, line : int, col : int, expected : string) =
    class
        (*
        new(str : char list, line : int, col : int, expected : string) = new ParserInfo(str, line, col, expected, "")
        *)
        new(pi : ParserInfo, expected : string) = new ParserInfo(pi.Rest, pi.Line, pi.Column, expected)
        member x.Rest = str
        member x.Line = line
        member x.Column = col
        member x.ExpectedValue = expected
        member x.IsSucceeded() = str.Length = 0 
        (*
        member x.HasError() = Option.isSome err
        member x.IsStopped() = str.Length > 0 && not (x.HasError())
        member x.ErrorMessage = if x.HasError() then err.Value.ErrorMessage else ""
        *)
        interface IComparable with
            member x.CompareTo(o : obj) =
                match o with
                | :? ParserInfo as y -> str.Length.CompareTo(y.Rest.Length)
                | _ -> -1
        override x.Equals(o : obj) = 
            match o with
            | :? ParserInfo as y -> line = y.Line && col = y.Column && str = y.Rest
            | _ -> false
        override x.GetHashCode() = str.GetHashCode()
        override x.ToString() =
            let cut (len : int) (s : string) =
                if s.Length > len then s.Substring(0, len)
                else s
            let s = sprintf "Parser stopped at line %d, column %d, where \"%s...\"" line col (cut 10 <| chars2str str)
                    //+ if not (x.HasError()) then "" else "With error:\n" + x.ErrorMessage
            s

        static member StartInfo(s : string) = new ParserInfo(str2chars s, 0, 0, "")
        //static member UnknownErrorInfo = new ParserInfo([], 0, 0, Some <| ParserError.UnknownParserError)
    end

type ParserError(line : int, col : int, expected : string, errmsg : string) =
    class
        let exp = sprintf "Error: At line %d, column %d expected \"%s\"" line col expected
        let error = if errmsg.Length = 0 then exp else errmsg
        new(line : int, col : int, expected : string) = new ParserError(line, col, expected, "")
        new(pi : ParserInfo, expected : string, errmsg : string) = new ParserError(pi.Line, pi.Column, expected, errmsg)
        new(pi : ParserInfo, errmsg : string) = new ParserError(pi.Line, pi.Column, pi.ExpectedValue, errmsg)
        member x.Line = line
        member x.Column = col
        member x.ExpectedValue = expected
        member x.ErrorMessage = error
        static member SymfParserErrorMsg = "Character doesn't match the function"
        static member UnknownParserError = new ParserError(0, 0, "", "Unknown parser error")

        interface IComparable with
            member x.CompareTo(o : obj) =
                match o with
                | :? ParserError as y -> if line > y.Line then -1 else col.CompareTo(y.Column) 
                | _ -> -1
        override x.Equals(o : obj) = 
            match o with
            | :? ParserError as y -> line = y.Line && col = y.Column
            | _ -> false
        override x.GetHashCode() = exp.GetHashCode()
        
        override x.ToString() = x.ErrorMessage
    end
    
type ParserResult<'a> = Success of 'a
                      | Failure of ParserError   
                      
type ParserTempResult<'a> = 
        | S of 'a * ParserInfo
        | F of ParserError  

type internal Parser<'a> = ParserInfo -> (ParserTempResult<'a>) seq
  
let internal yield' x = seq { yield x }

/// Always returns a Success with x as result
let value x : Parser<'a> =
    fun pi -> yield' <| S(x, pi)

/// Always fails.
let empty : Parser<'a> =
    fun _ -> Seq.empty

/// Bind operator. Applies f to the result of parser p.
let (>>=) (p : Parser<'a>) (f : 'a -> Parser<'b>) : Parser<'b> =
    let apply (f : 'a -> Parser<'b>) = 
        function
        | S(a, pi) -> (f a) pi
        | F(e) -> yield' <| F(e)
    fun pi -> Seq.concat (seq { for x in p pi do yield apply f x })

/// Applies the first parser and if it fails, applies the second one.
let (<|>) (p1 : Parser<'a>) (p2 : Parser<'a>) : Parser<'a> =
    fun pi -> Seq.append (p1 pi) (p2 pi)

/// Parses characters which satisfy the given function.
let symf f : Parser<char> =
    fun pi -> 
        match pi.Rest with
        | c::nrest when f c -> 
            let nline = pi.Line + (if c = '\n' then 1 else 0)
            let ncol = pi.Column + 1
            yield' <| S(c, new ParserInfo(nrest, nline, ncol, pi.ExpectedValue))
        | _ -> 
            let exp = pi.ExpectedValue
            let err = if exp.Length <> 0 then new ParserError(pi, exp, "") else new ParserError(pi, ParserError.SymfParserErrorMsg)
            yield' <| F(err)

/// Runs the given parser against the given string.
let run (p : Parser<'a>) (s : string) = 
    let results = p <| ParserInfo.StartInfo(s)
    let succ = results
               |> Seq.filter (fun x -> match x with S(_, pi) -> pi.IsSucceeded() | _ -> false)
               |> Seq.map (fun (S(a, _)) -> a)
    if Seq.isEmpty succ 
    then 
         Failure(ParserError.UnknownParserError)
    else Success(Seq.head succ) 

type ParserBuilder() =
    member x.Bind(p, f) = p >>= f
    member x.Return(y) = value y

let parse = new ParserBuilder()

/// Runs p as many times as posible, returning a list with the results.
let rec many p : Parser<list<'a>> =
    parse {
        let! x = p
        let! xs = many p
        return x :: xs
    } <|> value []

/// If p succeeds returns the result x.
let (>>%) p x : Parser<'b> =
    p >>= (fun _ -> value x)

/// Applies p1 and p2 returning the result of p2.
let (>>.) p1 p2 : Parser<'b> =
    p1 >>= (fun _ -> p2)

/// Applies p1 and p2 returning the result of p1.
let (.>>) p1 p2 : Parser<'a> =
    p1 >>= (fun x -> p2 >>% x)
    
/// Applies p1 and p2 returning as a result a tuple with both results.
let (.>>.) p1 p2 : Parser<'a * 'b> =
    p1 >>= (fun x -> p2 >>= (fun y -> value (x, y)))
    
/// If p is successful applies f to the parsed element.
let (|>>) p f : Parser<'b> =
    p >>= (fun x -> value (f x))

/// Runs phead and ptail, then create a list
let (>|>>) phead ptail : Parser<'a list> =
    phead .>>. ptail >>= (fun (h, t) -> value (h :: t))

/// Runs p as many times as possible with at least one Succcess.
let many1 p : Parser<list<'a>> =
    p >|>> many p
    
/// Runs p n times.
let rec times n p : Parser<list<'a>> =
    parse {
        if n <= 0 then return []
        else
            let! x = p
            let! xs = times (n - 1) p
            return x :: xs
    }

/// Returns the first successful result of the given parser sequence.
let any ps : Parser<'a> =
    Seq.reduce (fun p1 p2 -> p1 <|> p2) ps

/// Tries to run p
let opt (p : Parser<'a>) : Parser<'a option> = 
    p |>> Some <|> value None

/// Tries to run p. If it success then apply f to result else return default value
let optf f default' (p : Parser<'a>) = 
    opt p |>> (fun x -> match x with Some y -> f y | None -> default')

/// Applies the parsers popen, p and pclose in sequence. It returns the result of p.
let between popen p pclose =
    popen >>. p .>> pclose

/// Parses the given character.
let sym c : Parser<char> = 
    fun pi -> symf ((=) c) (new ParserInfo(pi, "symbol \'" + chars2str [c] + "\'"))

/// Parses any of given character
let syms cs : Parser<char> =
    any (List.map sym cs)
    
/// Parses the given string.
let pstr (s : string) : Parser<string> =
    let chars2str = (fun cs -> new System.String(cs |> List.toArray))
    let rec chars (cs : list<char>) =
        match cs with
        | [] -> value []
        | c::cs' -> sym c >|>> chars cs'
    chars (Seq.toList s) |>> chars2str

/// Parses letter of English alphabet
let palpha = 
    syms <| ['a'..'z'] @ ['A'..'Z']

/// Parses any char sequence
let pword = palpha
            |> many1 |>> chars2str

/// Parses white space characters. Useful for skipping them.
let pwhitesp = many <| syms [' '; '\t'; '\n'; '\r']

/// Makes p skip white space before its parsing
let skipws p = pwhitesp >>. p

/// Parses the given char and skips white space characters.
let symw c : Parser<char> = skipws (sym c)

/// Parses the given string and skips white space characters before string.
let pstrw s : Parser<string> = skipws (pstr s)

/// Parses any char sequence and skips white space characters before.
let pwordw : Parser<string> = skipws pword

/// Parses any digit
let pdigit = symf (fun c -> c >= '0' && c <= '9')

/// Parses mathematical sign /+|-/
let psign = sym '+' <|> sym '-' <|> value '+'

/// Parses integer number which match /[+-]?\d+/
let pint : Parser<int> =
    psign >|>> many1 pdigit |>> chars2int

/// Parses float numbers which match /[+-]?(\d+(\.\d+)?)/
let pfloat : Parser<float> =
    parse {
        let! sign = psign
        let! i = many1 pdigit
        let! d = sym '.' >|>> many1 pdigit |> optf id []
        return float (chars2str <| sign::(i @ d))
    }

/// Parses p splitted by c as many times as possible
let splitter p spl =
    p >|>> many (spl >>. p) 

/// Parses p splitted by c as many times as possible, ignoring white space
let splitterws p spl = 
    skipws p >|>> many (skipws spl >>. skipws p)