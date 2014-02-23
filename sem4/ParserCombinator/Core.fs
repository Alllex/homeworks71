
module ParserCombinators.Core

let chars2str cs = string (new System.String (cs |> List.toArray))
let chars2int s = int (new System.String(s |> List.toArray))

type ParserResult<'a> =
    | Success of 'a * list<char> 
    | Failure of char list

type Parser<'a> = list<char> -> ParserResult<'a>

/// Bind operator. Applies f to the result of parser p.
let (>>=) (p : Parser<'a>) (f : 'a -> Parser<'b>) : Parser<'b> =
    fun s ->
        match p s with
        | Success(x, rest) -> (f x) rest
        | Failure cs -> Failure cs

/// Always returns a Success with x as result
let value x : Parser<'a> =
    fun s -> Success(x, s)

/// Always fails.
let empty : Parser<'a> =
    fun cs -> Failure cs

type ParserBuilder() =
    member x.Bind(p, f) = p >>= f
    member x.Return(y) = value y

let parse = new ParserBuilder()

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

/// Applies the first parser and if it fails, applies the second one.
let (<|>) (p1 : Parser<'a>) (p2 : Parser<'a>) : Parser<'a> =
    fun s ->
        match p1 s with
        | Failure cs -> p2 s
        | x -> x
    
/// If p is successful applies f to the parsed element.
let (|>>) p f : Parser<'b> =
    p >>= (fun x -> value (f x))

/// Runs phead and ptail, then create a list
let (>|>>) phead ptail : Parser<'a list> =
    phead .>>. ptail >>= (fun (h, t) -> value (h :: t))

/// Runs p as many times as posible, returning a list with the results.
let rec many p : Parser<list<'a>> =
    parse {
        let! x = p
        let! xs = many p
        return x :: xs
    } <|> value []

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

/// Runs the given parser against the given string.
let run (p : Parser<'a>) (s : string) = p (List.ofArray <| s.ToCharArray())

/// Tries to run p
let opt (p : Parser<'a>) : Parser<'a option> = 
    p |>> Some <|> value None

/// Applies the parsers popen, p and pclose in sequence. It returns the result of p.
let between popen p pclose =
    popen >>. p .>> pclose

/// Parses the given character.
let sym c : Parser<char> =
    function
    | x::xs when x = c -> Success(x, xs)
    | xs -> Failure xs
    
/// Parses characters which satisfy the given function.
let symf f : Parser<char> =
    function
    | x::xs when f x -> Success(x, xs)
    | xs -> Failure xs

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
let skipws p = pwhitesp >>. p .>> pwhitesp

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
        let! d = sym '.' >|>> many1 pdigit |> opt
        let ends = match d with
                   | Some x -> x
                   | None -> []
        return float (chars2str <| sign::(i @ ends))
    }

/// Parses p splitted by c as many times as possible
let splitter p spl =
    p >|>> many (spl >>. p) 

/// Parses p splitted by c as many times as possible, ignoring white space
let splitterws p spl = 
    skipws p >|>> many (skipws spl >>. skipws p)