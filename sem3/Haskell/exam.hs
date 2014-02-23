
data E = X String       -- variable
       | N Integer      -- number
       | MUL  E E       -- a * b
       | DIV  E E       -- a / b
       | ADD  E E       -- a + b
       | SUB  E E       -- a - b
       | EXP  E Integer -- e ^ c 
          deriving Show

wrap s = "(" ++ s ++ ")"
showp e@(X _) = show e
showp e@(N _) = show e
showp e = wrap $ show e
priority (ADD _ _) = 1
priority (SUB _ _) = 1
priority (MUL _ _) = 2
priority (DIV _ _) = 2
priority (EXP _ _) = 3
priority _         = 4
showpr p e = let s = show e in if p > priority e then wrap s else s

showsim op e e1 e2 = let p = priority e in (showpr p e1)++op++(showpr p e2)
shownsim op e e1 e2@(X _) = let p = priority e in (showpr p e1)++op++(showpr p e2)
shownsim op e e1 e2@(N _) = let p = priority e in (showpr p e1)++op++(showpr p e2)
shownsim op e e1 e2       = let p = priority e in (showpr p e1)++op++(wrap $ showpr p e2)
{-}
instance Show E where
   show (X s) = s
   show (N x) = show x
   show e@(MUL e1 e2)  = showsim "*" e e1 e2
   show e@(ADD e1 e2)  = showsim "+" e e1 e2
   show e@(DIV e1 e2)  = shownsim "/" e e1 e2
   show e@(SUB e1 e2)  = shownsim "-" e e1 e2
   show (EXP e1 x)  = (showp e1)++"^"++(show x)
-}
newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> [Char] -> a
parse (P p) s = case eof $ p s of
                    [] -> error "Cannot parse program"
                    vs -> head vs

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

oneOf :: String -> Parser Char
oneOf = foldl (\a b -> a ||| sym b) empty

instance Monad Parser where
  return = val
  (>>=) = (||>)

letter = oneOf ['a'..'z']
digit = oneOf ['0'..'9']

sym1 op con = do sym op
                 return con

ident = do x <- letter
           xs <- many $ letter ||| digit
           return $ X $ x:xs

literal = do x <- digit
             xs <- many digit
             return $ N $ read $ x:xs

inbrackets = do sym '('
                e <- expr
                sym ')'
                return e

primary = ident ||| literal ||| inbrackets

left p op = do x <- p
               xs <- many other
               return $ foldl (\acc (o, y) -> acc `o` y) x xs
            where other = do o <- op 
                             y <- p 
                             return $ (o, y)
expi = ex ||| primary 
    where ex = do x <- primary
                  sym '^'
                  (N y) <- literal
                  return $ EXP x y
multi = left expi op
            where op = sym1 '*' MUL ||| sym1 '/' DIV
addi = left multi op
            where op = sym1 '+' ADD ||| sym1 '-' SUB
expr = addi
pe = parse expr

reduce (ADD e1 e2) = case (reduce e1, reduce e2) of
                        (N x, N y) -> N $ x + y
                        (N 0, e)   -> e
                        (e, N 0)   -> e
                        (e, N x)   -> ADD (N x) e
                        (N x, ADD (N y) e) -> ADD (N $ x + y) e
                        (e1, ADD (N y) e2) -> ADD (N y) (ADD e1 e2)
                        (ADD x y, ADD z w) -> ADD x (ADD y (ADD z w))

                        (ADD (ADD x y), z) -> 

                        (a, b)     -> ADD a b
reduce o@(SUB e1 e2) = case (reduce e1, reduce e2) of
                        (X x, X y) -> if x == y then N 0 else o
                        (N x, N y) -> N $ x - y
                        (e, N 0)   -> e
                        (e, N x)   -> ADD (N x) (SUB (N 0) e)
                        (a, b)     -> SUB a b
reduce (DIV e1 e2) = case (reduce e1, reduce e2) of
                        (N a, N b) -> let g = gcd a b in if a `mod` b /= 0 
                                                         then DIV (N $ a `div` g) (N $ b `div` g)
                                                         else N $ a `div` b
                        (e, N 1) -> e
                        (e, N 0) -> error "Division by zero!!!"
                        (N 0, e) -> N 0
                        (a, b)     -> DIV a b
reduce (MUL e1 e2) = case (reduce e1, reduce e2) of
                        (N 0, _)   -> N 0
                        (_, N 0)   -> N 0
                        (N 1, e)   -> e
                        (e, N 1)   -> e
                        (N x, N y) -> N $ x * y
                        (e, N x)   -> MUL (N x) e
                        (a, b)     -> MUL a b
reduce (EXP e1 p) = case (reduce e1, p) of
                        (_, 0)   -> N 1
                        (e, 1)   -> e
                        (N x, _) -> N $ x ^ p
                        (a, b)     -> EXP a b
reduce e = e

showr s = reduce $ pe s

diff x (EXP e p) = MUL (N p) (MUL (EXP e (p-1)) (diff x e))
diff x (MUL f g) = ADD (MUL (diff x f) g) (MUL f (diff x g))
diff x (DIV f g) = DIV (SUB (MUL (diff x f) g) (MUL f (diff x g))) (EXP g 2)
diff x (ADD f g) = ADD (diff x f) (diff x g)
diff x (SUB f g) = SUB (diff x f) (diff x g)
diff x (X y) = if x == y then N 1 else N 0
diff x (N _) = N 0
    
dx s = reduce $ diff "x" $ showr s


