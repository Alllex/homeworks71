
newtype Parser a = P (String -> [(a, String)])

empty :: Parser a
empty = P (\_ -> [])

sym :: Char -> Parser Char
sym c = P p where p (x:xs) | x == c = [(c, xs)]
                  p _               = []

val :: a -> Parser a
val a = P (\s -> [(a, s)])

infixl 2 |||
(|||) :: Parser a -> Parser a -> Parser a
(P p) ||| (P q) = P $ \s -> p s ++ q s

infixl 3 ||>
(||>) :: Parser a -> (a -> Parser b) -> Parser b
(P p) ||> q = P $ \s -> concat [apply (q a) s | (a, s) <- p s] 
               where apply (P p) = p

many :: Parser a -> Parser [a]
many a = a ||> (\ x -> many a ||> val . (x:)) ||| val []

opt :: Parser a -> Parser (Maybe a)
opt a = a ||> val . Just ||| val Nothing

eof :: [(a, String)] -> [a]
eof = map fst . filter ((==[]) . snd) 

instance Monad Parser where
  return = val
  (>>=) = (||>)

data E = X String -- 
       | N Integer -- number
       | MUL  E E -- a  * b
       | DIV  E E -- a  / b
       | ADD  E E -- a  + b
       | SUB  E E -- a  - b
       | RGT  E E -- a  > b
       | RLT  E E -- a  < b
       | REQ  E E -- a == b
       | RNGT E E -- a <= b
       | RNLT E E -- a >= b
       | RNE  E E -- a != b
       | LAND E E -- a && b
       | LOR  E E -- a || b

instance Show E where
   show (X s) = "x("++s++")"
   show (N x) = show x
   show (MUL e1 e2)  = "("++(show e1)++"*" ++(show e2)++")"
   show (DIV e1 e2)  = "("++(show e1)++"/" ++(show e2)++")"
   show (ADD e1 e2)  = "("++(show e1)++"+" ++(show e2)++")"
   show (SUB e1 e2)  = "("++(show e1)++"-" ++(show e2)++")"
   show (RGT e1 e2)  = "["++(show e1)++">" ++(show e2)++"]"
   show (RLT e1 e2)  = "["++(show e1)++"<" ++(show e2)++"]"
   show (REQ e1 e2)  = "["++(show e1)++"=="++(show e2)++"]"
   show (RNGT e1 e2) = "["++(show e1)++"<="++(show e2)++"]"
   show (RNLT e1 e2) = "["++(show e1)++">="++(show e2)++"]"
   show (RNE e1 e2)  = "["++(show e1)++"!="++(show e2)++"]"
   show (LAND e1 e2) = "{"++(show e1)++"&&"++(show e2)++"}"
   show (LOR e1 e2)  = "{"++(show e1)++"||"++(show e2)++"}"

oneOf = foldl (\ a b -> a ||| sym b) empty 

sym1 c con = sym c ||> (\_ -> val con)
sym2 c1 c2 con = sym c1 ||> (\_ -> (sym c2 ||> (\_ -> val con)))

letter = oneOf $ ['_'] ++ ['a'..'z'] ++ ['A'..'Z']
digit = oneOf ['0'..'9']
--ident = letter ||> (\ x -> many (letter ||| digit) ||> (\ xs -> val $ X (x:xs)))
ident = do x <- letter
           xs <- many $ letter ||| digit
           return $ X $ x:xs

--literal = digit ||> (\ x -> many digit ||> (\ xs -> val $ N $ read (x:xs)))
literal = do x <- digit
             xs <- many digit
             return $ N $ read $ x:xs
             
inbrackets = do sym '('
                e <- expr
                sym ')'
                return e

primary = ident ||| literal ||| inbrackets

expr = primary

apply (P p) s = eof $ p s
