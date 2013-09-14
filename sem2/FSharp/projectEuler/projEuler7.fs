(*
    Problem: http://projecteuler.net/problem=7
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
        
let mutable counter = 1
let n = 10001
let mutable prime = 1

while counter < n do
     prime <- prime + 2
     if isPrime prime then
         counter <- counter + 1
        
printfn "%A" prime