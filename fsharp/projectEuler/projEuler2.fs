(*
    Problem: http://projecteuler.net/problem=2
    Author: Alex Semin
    Place: Math-Mech SPbU 171
    Time:  03.2013
*)

let sum = ref 0
let rec fib f1 f2 =
    if f1 < 4 * 1000 * 1000 + 1 then
        if f1 % 2 = 0 then sum := !sum + f1
        fib f2 (f1 + f2)
           
fib 1 2
printf "Sum of even fibs = %i" !sum

    