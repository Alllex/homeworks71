
concat' = foldl (++) []

listify = map (:[])

-- List Comprehension

copyList y = [x | x <- y]

positiveOnly y = [x | x <- y, x > 0]

-- all natural numbers
n = 1 : [x + 1 | x <- n]