(*
    Problem: http://projecteuler.net/problem=9
    Author: Alex Semin
    Place: Math-Mech SPbU 171
    Time:  03.2013
*)

let mutable a = 0;
let mutable b = 0;
let mutable c = 0;

for aa in 1..999 do
  for bb in aa + 1 .. 999 do
    let cc = 1000 - aa - bb
    if aa * aa + bb * bb = cc * cc then
      a <- aa; b <- bb; c <- cc;

printfn "%A" (a * b * c)









