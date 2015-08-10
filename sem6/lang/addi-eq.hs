

-- Equivalence of additive expressions 
-- through (left) normalization

data Equable = E Equable Equable   -- (==)
             | V String            -- Variable
             | C Bool              -- Constant
             deriving Eq
              
instance Show Equable where
    show (V v) = v
    show (C c) = if c then "1" else "0"
    show (E l r) = "(" ++ show l ++ " + " ++ show r ++ ")"

norm :: Equable -> Equable
norm (E l r) = merge nl nr where
    nl = norm l
    nr = norm r
    merge c@(C _) (E l r) = E l $ merge c r
    merge (C c) (C c') = C $ c + c'
    merge (E ) (E l r) = E (merge e l) r
    merge e lm = E e lm
    merge _ _ = error "Illegal state"
norm (V x) = E (V x) (C True)
norm e = e

-- Equivalent additive expressions 
eqe :: Equable -> Equable -> Bool
e1 `eqe` e2 = norm e1 == norm e2 

samples = 
    [
        C 1,
        V "x",
        E (C 1) (V "x"),
        E (C 1) (E (C 2) (C 3)),
        E (E (E (C 1) (C 2)) (C 3)) (E (E (E (C 4) (C 5)) (C 6)) (C 7))
    ]

nsamples = map norm samples 

main = do
    mapM_ (putStrLn . show) $ samples `zip` nsamples
    putStrLn "Hi"