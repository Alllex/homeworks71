{-
    Haskell - TEST#2
    Aleksey Semin
    08.10.13
-}

-- 1
fromFun f = map (\x -> (x, f x))
-- 2
dom = map fst
-- 3
eval xs x = snd $ head $ filter ((==x) . fst) xs
-- 4
invert = map (\(a,b) -> (b,a))
-- 5
infixl 9 .*.
g .*. f = [(x,z) | (x,y) <- f, (y',z) <- g, y == y']
-- 6
image ft xs = norm [y | x' <- xs, (x, y) <- ft, x == x']
    where norm = foldr (\x acc -> if elem x acc then acc else x:acc) []
-- 7
preimage = image . invert

isInjective ft = inj [] ft where 
    inj acc ((_,y):rest) = (not $ elem y acc) && (inj (y:acc) rest)
    inj _ _ = True
-- 9
isSurjective ft = length ft == -1
-- 10
areMutuallyInverse ft ft' = foldr (\(x,y) acc -> (elem (y,x) ft') && acc) True ft