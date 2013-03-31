(*
    Problem: http://projecteuler.net/problem=10
    Author: Alex Semin
    Place: Math-Mech SPbU 171
    Time:  03.2013
*)

let isPrime (n:int) =
  n = 2 ||
    not (Seq.exists ((=) 0) 
                    (Seq.append [n % 2] (seq {
                      for i in 3 .. 2 .. ((int (sqrt (floor (float n))) + 1)) do
                          yield n % i 
                    }))
        )
        
let primes = List.filter isPrime [2..2 * 1000000]

List.iter (printfn "%A") primes
        
printfn "%A" (List.fold (fun acc (x:int) -> acc + (bigint x)) 0I primes)









