open System.Text.RegularExpressions
open NUnit.Framework
open FsUnit

let letter = "a-zA-Z"
let digits = "0-9"
let specials = "_\-!#$%^&\*\(\)/"
let all = letter + digits + specials
let fstChar = "[_" + letter + "]"

let name = fstChar + "[" + all + "]{0,30}"
let fullName = "^" + name + "[.]?" + name + "@"
let domain2 = "([" + all + "]{1,30}\.)+"
let domain1 = "(name|info|yandex|museum|[a-zA-Z]{2,3})$"

let check = new Regex(fullName + domain2 + domain1)

let correct = [ "alllex.semin@gmail.com"
                "superman323@supersite.info"
                "victor.polozov@gmail.com"
                "victorp@math.spbu.ru"
                "victor.polozov@lanit-tercom.com" ]
              
let incorrect = [ ".!!!!!!@gmail.com"
                  "example@@mail.ru"
                  "mymail.ru"
                  "@gmail.com"
                  "example@.ru"
                  "example@mail.nasa"
                  "example@verylooooooooooooooooooooooooooongDomain.com"
                  "example@domain2..com"
                  "info@dom.c0m"
                  "me..my@gnail.com" ]

let test1 = List.fold (fun acc mail -> acc && (check.IsMatch(mail))) true correct
let test2 = List.fold (fun acc mail -> acc || (check.IsMatch(mail))) false incorrect


[<Test>]
let ``alllex.semin(at)gmail.com is correct eMail`` () =
    (check.IsMatch("alllex.semin@gmail.com")) |> should equal true

[<Test>]
let ``superman323(at)supersite.info is correct eMail`` () =
    (check.IsMatch("superman323@supersite.info")) |> should equal true

[<Test>]
let ``example(at)(at)mail.ru is NOT correct eMail`` () =
    (check.IsMatch("example@@mail.ru")) |> should equal false

[<Test>]
let ``info(at)dom.c0m is NOT correct eMail`` () =
    (check.IsMatch("info@dom.c0m")) |> should equal false

[<Test>]
let ``(at)gmail.com is NOT correct eMail`` () =
    check.IsMatch("@gmail.com") |> should equal false


