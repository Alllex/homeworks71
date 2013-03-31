(*
    Problem: http://projecteuler.net/problem=3
    Author: Alex Semin
    Place: Math-Mech SPbU 171
    Time:  03.2013
*)

let modFloats n d =
    let div = n / d
    if div > floor(div) then
        1
    else 
        0
        
let eps = 1e-10
        
let rec gpd (n : float) (d : float) =
    if modFloats n d = 0 then
        if abs((n / d) - 1.0) < eps then
            d
        else
            gpd (n / d) d
    else
        gpd n (d + 1.0)
        
let N = 600851475143.0        
printfn "Greatest prime divisor of %.0f is %.0f" N (gpd N 2.0)
            