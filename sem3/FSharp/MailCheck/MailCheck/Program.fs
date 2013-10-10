(*
    Project: MailCheck
    Author: Alex Semin Math-Mech 271 2013
    2013 (c)
*)

module MailCheck

open System.Text.RegularExpressions

type EMailChecker() =
    class
        static let letters = "a-zA-Z"
        static let digits = "0-9"
        static let specials = "_\-!#$%^&)(/"
        static let all = letters + digits + specials
        static let fstChar = "[_" + letters + "]"

        static let name = "[" + all + "]{0,30}"
        static let fullName = "^" + fstChar + name + "[.]?" + name + "@"
        static let domain2 = "([" + all + "]{1,30}\.)+"
        static let domain1 = "(name|info|yandex|museum|[a-zA-Z]{2,3})$"

        static let emailChecker = new Regex(fullName + domain2 + domain1)

        member this.Check(email : string) = emailChecker.IsMatch(email)
    end

