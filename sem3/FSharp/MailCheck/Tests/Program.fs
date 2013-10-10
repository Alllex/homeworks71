
open MailCheck
open FsUnit
open NUnit.Framework

[<TestFixture>]
type MailCheckFixture() =

    let correct = [ "a@b.cc"
                    "victor.polozov@mail.ru"
                    "my@domain.info"
                    "_.1@mail.com"
                    "coins_department@hermitage.museum" ]
              
    let incorrect = [ "a@b.c"
                      "a..b@mail.ru"
                      ".a@mail.ru"
                      "yo@domain.somedomain"
                      "1@mail.ru" ]

    let checker = new EMailChecker()

    [<Test>]
    member x.``Email: alllex.semin(at)gmail.com`` () =
        checker.Check("alllex.semin@gmail.com") |> should be True

    [<Test>]
    member x.AllCorrect() =
        correct |> List.iter (fun email -> checker.Check(email) |> should be True)
        
    [<Test>]
    member x.AllIncorrect() =
        incorrect |> List.iter (fun email -> checker.Check(email) |> should be False)