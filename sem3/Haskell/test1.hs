{-
    Haskell - TEST#1
    Aleksey Semin
    24.09.13
-}


zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
zipWith' _ _ _ = []

sprod x y = sum $ zipWith' (*) x y

normilize l = foldr (\x acc -> if elem x acc then acc else x:acc) [] l

qsort [] = []
qsort (x:xs) = qsort [a | a <- xs, a <= x] ++ [x] ++ qsort [a | a <- xs, a > x]

isProgress [] = True
isProgress [x] = True
isProgress xs = 1 == (length $ normilize (zipWith' (-) xs $ tail xs))

isFunction [] = True
isFunction (r:rs) = (length [1 | (x,_) <- rs, (fst r) == x]) == 0 && isFunction rs

isSymmetric r = foldr (&&) True $ map (\(x,y) -> elem (y,x) r) r

isReflexive r = foldr (\x acc -> if (elem (x,x) r) && elem (-x,-x) r then acc else False) True [0,1..]

closure r = if cl == [] then r else closure $ r ++ cl
    where cl = normilize [(x,z) | a@(x,y1) <- r, b@(y2,z) <- r, y1 == y2, a /= b, not $ elem (x,z) r]

isTransitive r = foldr (&&) True [elem (x,z) r | a@(x,y1) <- r, b@(y2,z) <- r, y1 == y2, a /= b]