{- 
    Hometask #5
    Balanced trees
-}

--            -empty-      -height- -key-  -left-  -right-
data Tree a =    E    | T  Integer    a   (Tree a) (Tree a)

-- simple way for displaying trees
instance Show a => Show (Tree a) where
    show E = "E"
    show (T _ k E E) = "leaf["++(show k)++"]"
    show (T h k l r) = "T(h"++(show h)++" k["++(show k)++"] "++(show l)++" "++(show r)++")"

-----------------------------------------------------------


fh E = 0
fh (T h _ _ _) = h
mk k l r = T (max (fh l) (fh r) + 1) k l r
diff (T _ _ l r) = (fh r) - (fh l)
rotateR (T _ key (T _ lkey ll lr) r) = mk lkey ll (mk key lr r)
rotateL (T _ key l (T _ rkey rl rr)) = mk rkey (mk key l rl) rr
balance p@(T h k l r) =
    if diff p > 1  then rotateL $ mk k l (if diff r > 0 then r else rotateR r) else 
    if diff p < -1 then rotateR $ mk k (if diff l < 0 then l else rotateL l) r else p

insert :: Tree Integer -> Integer -> Tree Integer
insert E x = T 1 x E E
insert t@(T h k l r) x
    | x > k     = balance $ mk k l (insert r x)
    | x < k     = balance $ mk k (insert l x) r
    | otherwise = t

remove :: Tree Integer -> Integer -> Tree Integer
remove E _ = E
remove t@(T h k l r) x
    | x > k     = balance $ mk k l (remove r x)
    | x < k     = balance $ mk k (remove l x) r
    | otherwise = case r of 
        E -> l
        _ -> let (k', r') = rm r in balance $ mk k' l r'
    where
        rm (T _ k E r) = (k, r)
        rm (T _ k l r) = let (k', l') = rm l in (k', balance $ mk k l' r)


-----------------------------------------------------------
-- auxiliary functions

-- making leaf
lf :: Integer -> Tree Integer
lf x = T 1 x E E
-- making node with the only left child
lp :: Integer -> Integer -> Tree Integer
lp x y = T 2 x (lf y) E
-- making node with the only right child
rp :: Integer -> Integer -> Tree Integer
rp x y = T 2 x E (lf y)

insertList :: Tree Integer -> [Integer] -> Tree Integer
insertList t = foldl (\acc x -> insert acc x) t
-- make tree
mt :: [Integer] -> Tree Integer
mt = insertList E

-----------------------------------------------------------

test1 = mt [8,9,4,6,2,1]
test2 = mt [8,9,4,6,2,3]
test3 = mt [8,9,4,6,2,5]
test4 = mt [8,9,4,6,2,7]

test5 = mt [3,2,4,1]
test6 = remove test5 1
test7 = remove test5 2
test8 = remove test5 3
test9 = remove test5 4

test10 = mt [2,1,5,0,4,6,3]
test11 = remove test10 6
test12 = remove test10 2

test13 = mt [5,3,9,2,4,7,10,1,8]
test14 = remove test13 5

