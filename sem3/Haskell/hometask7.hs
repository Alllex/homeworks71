type Parser a = String -> [(a, String)]

sym :: Char -> Parser Char
sym c (x:xs) | x == c = [(c,xs)]
sym c _               = []

val :: a -> Parser a
val a s = [(a,s)]

infixl 2 |||
(|||) :: Parser a -> Parser a -> Parser a
p1 ||| p2 = \x -> (p1 x) ++ (p2 x)


infixl 3 ||>
(||>) :: Parser a -> (a -> Parser b) -> Parser b
p ||> q = \s -> concat [q a s | (a, s) <- p s]

many :: Parser a -> Parser [a]
many p = p ||> (\x -> many p ||> val . (x:)) ||| val []

opt :: Parser a -> Parser (Maybe a)
opt a = a ||> val . Just ||| val Nothing
