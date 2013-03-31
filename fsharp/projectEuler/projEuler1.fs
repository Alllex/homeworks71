(*
    Problem: http://projecteuler.net/problem=1
    Author: Alex Semin
    Place: Math-Mech SPbU 171
    Time:  03.2013
*)

printfn "%A" (List.fold (+) 0 (List.filter (fun x -> x % 3 = 0 || x % 5 = 0) [1..999]))

