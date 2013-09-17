{-
#1. Fibonacci numbers (using pairs)
#2. Fractional arithmetic
#3. Processing functions lists
#4. Polynomial integer arithmetic (include gcd)
-}
-----------------------------------------------------------
-- fibonacci numbers

fib n = fib' n (0, 1)
 where fib' n (f1, f2) 
    | n > 0 = fib' (n - 1) (f2, f1 + f2)
    | n == 0 = f1

-----------------------------------------------------------
-- fraction

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
-- polynoms with integer factors

showMonom (k, p) = 
        (if k /= 1 then showK k 
         else if p == 0 then "1" else "")
        ++ (if p /= 0 then "x" 
                ++ (if p /= 1 then "^" ++ show p else "") 
            else "")
        where showK k = if k < 0 then "(" ++ show k ++ ")" 
                        else show k

showPolynom [] = ""
showPolynom [m] = showMonom m
showPolynom (m:ms) = showMonom m ++ " + " ++ showPolynom ms

addMonom [] m = [m]
addMonom p@(ph@(pk,pp):ms) m@(mk, mp) =
    if mp < pp then ph : addMonom ms m
    else if mp > pp then m : p 
         else let k = pk + mk in if k == 0 then ms 
                                 else (k, pp) : ms

subMonom [] (k,p) = [(-k, p)]
subMonom pol (k,p) = addMonom pol (-k, p)

addPolynom p [] = p
addPolynom p (m:ms) = addPolynom (addMonom p m) ms

subPolynom p [] = p
subPolynom p (m:ms) = subPolynom (subMonom p m) ms

multMonom [] m = []
multMonom _ (0, _) = []
multMonom ((pk, pp):ms) m@(mk, mp) = 
    (pk * mk, pp + mp) : multMonom ms m

multPolynom _ [] = []
multPolynom p (m:ms) = 
    addPolynom (multMonom p m) (multPolynom p ms)