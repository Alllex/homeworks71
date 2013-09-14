(*
    Problem: http://projecteuler.net/problem=5
    Author: Alex Semin
    Place: Math-Mech SPbU 171
    Time:  03.2013
*)

let isPrime (n:int) =
  n = 2 ||
    not (Seq.exists (fun x -> x = 0) 
                    (Seq.append [n % 2] (seq {
                      for i in 3 .. 2 .. ((int (sqrt (floor (float n))) + 1)) do
                          yield n % i 
                    }))
        )

let divisiors = [11..20]

let isSuitable n = 
    List.fold (fun check x -> check && (n % x = 0)) true divisiors

let mutable n = List.fold (fun acc x -> acc * x) 1 (List.filter isPrime [2..20])

while (not(isSuitable n)) do
    n <- n + 2

printfn "%A" n