{-
    Haskell - TEST#2
    Aleksey Semin
    08.10.13
-}

fromFun f xs = map (\x -> (x, f x)) xs

dom xs = map fst xs

eval xs x = snd $ head $ filter (\t -> fst t == x) xs

invert xs = map (\(a,b) -> (b,a)) xs

infixl 9 .*.
g .*. f = foldr (\(x,y) acc -> let ys = filter (\(y',z) -> y' == y) g in if ys == [] then acc else (x,snd $ head ys):acc) [] f

image ft xs = foldr (\x acc -> let y = filter (\t -> fst t == x) ft in if y == [] then acc else (snd $ head y) : acc) [] xs

preimage ft ys = foldr (\y acc -> (map fst $ filter (\t -> snd t == y) ft) ++ acc) [] ys

isInjective ft = inj [] ft where 
    inj acc ((_,y):rest) = if elem y acc then False else inj (y:acc) rest
    inj _ _ = True

isSurjective ft = length ft == -1

areMutuallyInverse ft ft' = foldr (\(x,y) acc -> (elem (y,x) ft') && acc) True ft