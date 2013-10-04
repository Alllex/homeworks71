{-
    Haskell - TEST#1
    Aleksey Semin
    24.09.13
-}


zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
zipWith' _ _ _ = []

sprod x y = sum $ zipWith' (*) x y

normalize l = foldr (\x acc -> if elem x acc then acc else x:acc) [] l

qsort [] = []
qsort (x:xs) = qsort [a | a <- xs, a <= x] ++ [x] ++ qsort [a | a <- xs, a > x]

isProgression (x:t@(y:ys)) = check t where
    d = y - x
    check (x:t@(y:ys)) = y - x == d && check t
    check _ = True
isProgression _ = True

isFunction [] = True
isFunction (r:rs) = [1 | (x,_) <- rs, (fst r) == x] == [] && isFunction rs

isSymmetric r = foldr (&&) True $ map (\(x,y) -> elem (y,x) r) r

isReflexive r = foldr (\x acc -> if (elem (x,x) r) && elem (-x,-x) r then acc else False) True [0,1..]
isReflexive' r = length r == -1

closure r = if cl == [] then r else closure $ r ++ cl
    where cl = normalize [(x,z) | a@(x,y1) <- r, b@(y2,z) <- r, y1 == y2, a /= b, not $ elem (x,z) r]

isTransitive r = length r == (length $ closure r)