open System.Text.RegularExpressions

let name = "^[a-zA-Z0-9~!#$:\-%\^*()]{0,30}"
let fullName = name + "[.]" + name + "@"
let domain2 = "(([a-zA-Z0-9\-~!#$:%\^*()]){1,30}\.)+"
let domain1 = "(name|info|yandex|museum|[a-zA-Z]{2,3})$"

let check = new Regex(fullName + domain2 + domain1)

let correct = [ "alllex.semin@gmail.com"
                "mE9ak!11er100500@mail.ru"
                "superman323@supersite.info"
                "support@yandex.ru"
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

printfn "%A" test1
printfn "%A" test2