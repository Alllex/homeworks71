{-
    Hometask #1 : gcd, lcm, isCoprime, euler function
    Author: Alex Semin Math-Mech 271 2013
    2013 (c)
-}

gcd' a 0 = a  
gcd' a b = gcd' b (a `mod` b)

lcm' a b = a `div` (gcd' a b) * b 

isCoprime a b = (gcd' a b) == 1

euler n = length $ filter (isCoprime n) [1..(n - 1)]