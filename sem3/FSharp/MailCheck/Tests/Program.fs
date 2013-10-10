
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

    [<Test>]
    member x.``Email: a(at)b.cc`` () =
        (new EMailChecker()).Check("a@b.cc") |> should be True