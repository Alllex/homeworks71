{-
    Hometask #2
    Author: Alex Semin Math-Mech 271 2013
    2013 (c)

    #1. Fibonacci numbers (using pairs)
    #2. Processing functions lists
    #3. Fractional arithmetic
    #4. Polynomial integer arithmetic (include gcd)
-}
-----------------------------------------------------------
-- fibonacci numbers

fib n = fib' n (0, 1)
 where fib' n (f1, f2) 
    | n > 0 = fib' (n - 1) (f2, f1 + f2)
    | n == 0 = f1

-----------------------------------------------------------
-- lists

len [] = 0
len (x:xs) = 1 + len xs

con [] xs = xs
con (x:xs) ys = x : con xs ys

rev [] = []
rev xs = rev' xs [] where 
    rev' [] ys = ys
    rev' (x:xs) ys = rev' xs (x:ys)

take' _ [] = []
take' 0 _ = []
take' n (x:xs) = x : take' (n - 1) xs

concat' [] = []
concat' (x:xs) = x ++ concat' xs

map' f [] = []
map' f (x:xs) = f x : map' f xs

infixl 7 ***
xs *** ys = concat' $ dec xs ys 
    where   
        dec _ [] = []
        dec [] _ = []
        dec (x:xs) ys = map' (*x) ys : dec xs ys

-----------------------------------------------------------
-- fraction

fr a = (a, 1)

reduce (p, q) = (div p d, div q d) where d = gcd p q

infixl 7 */
(p1, q1) */ (p2, q2) = reduce (p, q)
    where p = p1 * p2
          q = q1 * q2

infixl 7 //
(p1, q1) // (p2, q2) = (p1, q1) */ (q2, p2)

infixl 6 +/
(p1, q1) +/ (p2, q2) = reduce (p1 * q2 + p2 * q1, q1 * q2)

infixl 6 -/
(p1, q1) -/ (p2, q2) = (p1, q1) +/ (-p2, q2)

neg (p, q) = (-p, q)
isNegative (p, _) = p < 0
isZero (p, _) = p == 0
isOne (1, 1) = True
isOne (_, _) = False
isInt fr = let (_, q) = reduce fr in q == 1

showFraction fr@(p, q) =
    if (isInt fr) && (not $ isNegative fr) then show p
    else "(" ++ show' ++ ")"
    where show' = if isInt fr then show p 
                  else show p ++ "/" ++ show q

testShowFraction = map showFraction.reduce $ [(p,q) | p <- [-10..10], q <- [1..10]]

-----------------------------------------------------------
-- polynoms

showMonom (k, p) = 
        (if not $ isOne k then showFraction k
         else if p == 0 then "1" else "")
        ++ (if p /= 0 
            then "x" ++ (if p /= 1 then "^" ++ show p else "") 
            else "")

showPolynom [] = ""
showPolynom [m] = showMonom m
showPolynom (m:ms) = showMonom m ++ " + " ++ showPolynom ms

addMonom [] m = [m]
addMonom p@(ph@(pk,pp):ms) m@(mk, mp) =
    if mp < pp then ph : addMonom ms m
    else if mp > pp then m : p 
         else let k = pk +/ mk in if isZero k then ms 
                                  else (k, pp) : ms

subMonom pol (k,p) = addMonom pol (neg k, p)

addPolynom p [] = p
addPolynom p (m:ms) = addPolynom (addMonom p m) ms

subPolynom p [] = p
subPolynom p (m:ms) = subPolynom (subMonom p m) ms

multMonom [] _ = []
multMonom _ ((0, _), _) = []
multMonom ((pk, pp):ms) m@(mk, mp) = 
    (pk */ mk, pp + mp) : multMonom ms m

multPolynom _ [] = []
multPolynom p (m:ms) = 
    addPolynom (multMonom p m) (multPolynom p ms)

lessThanMonom (_, p) (_, p') = p < p'

lessThanPolynom [] [] = False
lessThanPolynom _ [] = False
lessThanPolynom [] _ = True
lessThanPolynom (m:ms) (m':ms') = 
    lessThanMonom m m' || (m == m' && lessThanPolynom ms ms')

divideMonom (k, p) (k', p') = (k // k', p - p')

dividePolynom dividend d@(hd:_) =
    divide [] dividend
    where divide q [] = (q, [])
          divide q r@(rh:_) = 
            if lessThanPolynom r d then (q, r) else divide q' r'
            where factor = divideMonom rh hd
                  r' = subPolynom r $ multMonom d factor
                  q' = addMonom q factor

divPolynom dividend divisor = fst $ dividePolynom dividend divisor
modPolynom dividend divisor = snd $ dividePolynom dividend divisor

testDivide p1 p2 = let sp = showPolynom in
    sp p1 ++ " = (" ++ sp p2 ++ ") * (" ++ sp q ++ ") + (" ++ sp r ++ ")"
     where (q, r) = dividePolynom p1 p2

gcdPolynom p [] = p
gcdPolynom p1 p2 = gcdPolynom p2 $ modPolynom p1 p2

p1 = [(fr (-1),2),(fr 3,1),(fr 1,0)]
p2 = [(fr 1,2),(fr 2,1),(fr 1,0)] 

test1 = showPolynom $ gcdPolynom p1 p2