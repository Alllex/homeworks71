(*
    Project: MailCheck
    Author: Alex Semin Math-Mech 271 2013
    2013 (c)
*)

module MailCheck

open System.Text.RegularExpressions

type EMailChecker() =
    class
        let letters = "a-zA-Z"
        let digits = "0-9"
        let specials = "_\-!#$%^&)(/"
        let all = letters + digits + specials
        let fstChar = "[_" + letters + "]"

        let name = "[" + all + "]{0,30}"
        let fullName = "^" + fstChar + name + "[.]?" + name + "@"
        let domain2 = "([" + all + "]{1,30}\.)+"
        let domain1 = "(name|info|yandex|museum|[a-zA-Z]{2,3})$"

        let emailChecker = new Regex(fullName + domain2 + domain1)

        member this.Check(email : string) = emailChecker.IsMatch(email)
    end

