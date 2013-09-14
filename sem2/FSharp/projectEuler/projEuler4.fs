(*
    Problem: http://projecteuler.net/problem=4
    Author: Alex Semin
    Place: Math-Mech SPbU 171
    Time:  03.2013
*)

let rec getReverse n k =
    if n = 0 then
        k
    else
        getReverse (n / 10) (k * 10 + n % 10)
        
let isPalindrom n = n = (getReverse n 0)

let mutable max = 0

for i = 999 downto 100 do
    for j = i downto 100 do
        if (isPalindrom (i * j)) then
            if max < i * j then
                max <- i * j
              
              
printfn "%A" max            
    



    