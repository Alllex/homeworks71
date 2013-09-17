
concat' = foldl (++) []
listify = map (:[])
--listify = foldr (\x acc -> [x]:acc) []
copyList y = [x | x <- y]
positiveOnly y = [x | x <- y, x > 0]

n = 1 : [x + 1 | x <- n]