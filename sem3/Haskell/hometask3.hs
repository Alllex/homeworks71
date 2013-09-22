{-
#1. Список всех простых чисел
#2. Список всех чисел Фиббоначи
-}

p = 2:[x| x <- [3,5..], pr x] where
    pr x = foldr (\d acc -> if mod x d == 0 then False
                            else if d*d > x then True 
                                 else acc) True p

p2 = 2 : f [3,5..] where f (x:xs) = x : (f $ filter (\y -> mod y x /= 0) xs)


fibs = 0 : 1 : (zipSum fibs $ tail fibs) 
	where zipSum (x:xs) (y:ys) = x + y : zipSum xs ys

fibs2 = fibs 0 1 where fibs a b = a : fibs b (a+b)