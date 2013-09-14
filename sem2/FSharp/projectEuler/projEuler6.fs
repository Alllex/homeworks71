(*
    Problem: http://projecteuler.net/problem=6
    Author: Alex Semin
    Place: Math-Mech SPbU 171
    Time:  03.2013
*)


let n = 100

let result = 2 * (List.fold (fun acc k -> acc + k * (List.fold (+) 0 [k + 1 .. n])) 0 [1..n - 1])

printfn "%A" result