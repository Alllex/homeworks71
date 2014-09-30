{-
    Hometask #7
    Author: Alex Semin Math-Mech 271 2013
    2013 (c)

    #1 C logical expressions parser
-}

type Parser a = String -> [(a, String)]

empty :: Parser a
empty _ = []

sym :: Char -> Parser Char
sym c (x:xs) | x == c = [(c, xs)] 
sym c _               = []

val :: a -> Parser a
val a s = [(a, s)]

infixl 2 |||
(|||) :: Parser a -> Parser a -> Parser a
p ||| q = \ s -> p s ++ q s

infixl 3 ||>
(||>) :: Parser a -> (a -> Parser b) -> Parser b
p ||> q = \s -> concat [q a s | (a, s) <- p s] 

many :: Parser a -> Parser [a]
many a = a ||> (\ x -> many a ||> val . (x:)) ||| 
         val []

opt :: Parser a -> Parser (Maybe a)
opt a = a ||> val . Just ||| val Nothing

eof :: [(a, String)] -> [a]
eof = map fst . filter ((==[]) . snd) 

data E = X String -- 
       | N Integer -- number
       | UNPLUS E  -- (+a)
       | UNMINUS E -- (-a)
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
   show (UNPLUS e)   = "(+"++(show e)++")"
   show (UNMINUS e)  = "(-"++(show e)++")"
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
ident = letter ||> (\ x -> many (letter ||| digit) ||> (\ xs -> val $ X (x:xs)))
literal = digit ||> (\ x -> many digit ||> (\ xs -> val $ N $ read (x:xs)))
inbrackets = sym '(' ||> (\_ -> expr ||> (\e -> sym1 ')' e))

primary' = 
  ident   ||| 
  literal ||| 
  inbrackets

unary = op ||> (\o -> primary' ||> (\e -> val $ o e))
    where op = sym1 '+' UNPLUS |||
               sym1 '-' UNMINUS

primary = primary' ||| unary

left_assoc p op = p ||> (\x -> (many (op ||> (\o -> p ||> (\y -> val $ (o, y))))) 
                               ||> (\xs -> val $ foldl (\acc (o, y) -> acc `o` y) x xs))

bin_op l r op = l ||> (\x -> op ||> (\o -> r ||> (\y -> val $ x `o` y))) ||| l

multi = left_assoc primary op
    where op = sym1 '*' MUL ||| sym1 '/' DIV

addi = left_assoc multi op
    where op = sym1 '+' ADD ||| sym1 '-' SUB              

reli = bin_op addi addi op
    where op = sym2 '>' '=' RNLT |||
               sym2 '<' '=' RNGT |||
               sym2 '=' '=' REQ  |||
               sym2 '!' '=' RNE  |||
               sym1 '<' RLT      ||| 
               sym1 '>' RGT 

logi = bin_op reli logi op
    where op = sym2 '&' '&' LAND |||
               sym2 '|' '|' LOR

expr = logi

test_small = eof $ expr "2/3/4-1-2-3"
test_big   = eof $ expr "((2*3*4)<(1-2-3))&&(5==7)||(2+3/8>=(4/7-5))"