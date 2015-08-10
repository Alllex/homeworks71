
-- Non-strict semantic of { (+), (/), (==), (&&) }

type Var = String

data E = Add E E    -- Addition
       | Div E E    -- Division
       | EqE E E    -- (==)
       | And E E    -- (&&)
       | V Var      -- Variable
       | C Int      -- Const
       deriving Eq

showIt l op r = "(" ++ show l ++ " " ++ op ++ " " ++ show r ++ ")"

instance Show E where
    show (C c) = show c
    show (V x) = x
    show (Add l r) = showIt l "+" r
    show (Div l r) = showIt l "/" r
    show (EqE l r) = showIt l "==" r
    show (And l r) = showIt l "&&" r

allExprs xs ns = al [] [] bits where
    merge (x:xs) (n:ns) = V x : C n : merge xs ns
    merge xs [] = map V xs
    merge [] ns = map C ns
    bits = merge xs ns
    ops = [Add, Div, EqE, And]
    gen xs ys = [ op l r | l <- xs, r <- ys, op <- ops ]
    al _ tmp [] = alt where
        alt = tmp ++ [ op l r | l <- alt, r <- alt, op <- ops ]
    al ans tmp (x:xs) = ntmp ++ al (ans ++ ntmp) new xs where
        ntmp = x:tmp
        new = gen ans ntmp ++ gen ntmp ans ++ gen ntmp ntmp

--check (x:xs) = not (x `elem` xs) && check xs
--check [] = True

eval :: (Var -> Maybe Int) -> E -> Maybe Int
eval f e = ev e where
    mrg l r op = case (eval f l, eval f r) 
                 of (Just x, Just y) -> Just $ x `op` y
                    _ -> Nothing
    b2i b = if b then 1 else 0
    ev (C c) = Just c
    ev (V v) = f v
    ev (Add l r) = mrg l r (+)
    ev (Div l r) = mrg l r div -- division by zero
    ev (EqE l r) = mrg l r (\x y -> b2i $ (==) x y)
    ev (And l r) = nd (nd (Just 1) $ ev r) $ ev l
        where nd def Nothing = Nothing
              nd def (Just 0) = Just 0
              nd def _ = def  

-- strict version of eval
eval_s :: (Var -> Maybe Int) -> E -> Maybe Int
eval_s f e = ev e where
    mrg l r op = case (eval f l, eval f r) 
                 of (Just x, Just y) -> Just $ x `op` y
                    _ -> Nothing
    b2i b = if b then 1 else 0
    ev (C c) = Just c
    ev (V v) = f v
    ev (Add l r) = mrg l r (+)
    ev (Div l r) = mrg l r div
    ev (EqE l r) = mrg l r (((.).(.)) b2i (==))
    ev (And l r) = mrg l r (\x y -> b2i $ (x /= 0 && y /= 0))

main = do
    let exprs = take 10 $ allExprs [] [1..]
    --mapM_ (putStrLn . show) exprs
    let ctx = \x -> if x == "x" then Just 0 else Nothing
    let expr = And (V "x") (V "y")
    (putStrLn . show) $ (eval ctx expr, eval_s ctx expr)
    ----putStrLn $ show $ 
    --mapM_ (putStrLn . show) $ map (\e -> (e, eval ctx e, eval_s ctx e)) exprs

    putStrLn "Hi"