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

-- height
fh :: Tree Integer -> Integer
fh E = 0
fh (T h _ _ _) = h

-- difference between children's height
diff :: Tree Integer -> Integer
diff (T _ _ l r) = (fh r) - (fh l)

-- fixing of node height
fixh :: Tree Integer -> Tree Integer
fixh (T _ k l r) = T (max (fh l) (fh r) + 1) k l r

-- simple right and left rotation
rotateR :: Tree Integer -> Tree Integer
rotateR (T _ key (T _ lkey ll lr) r) = fixh $ T 0 lkey ll (fixh $ T 0 key lr r)
rotateL :: Tree Integer -> Tree Integer
rotateL (T _ key l (T _ rkey rl rr)) = fixh $ T 0 rkey (fixh $ T 0 key l rl) rr

-- balancing of a node
balance :: Tree Integer -> Tree Integer
balance q = let p@(T h k l r) = fixh q in
    if diff p > 1 then rotateL $ T h k l (if diff r > 0 then r else rotateR r) else 
    if diff p < -1 then rotateR $ T h k (if diff l < 0 then l else rotateL l) r else p

insert :: Tree Integer -> Integer -> Tree Integer
insert E x = T 1 x E E
insert t@(T h k l r) x
    | x == k    = t
    | x < k     = balance $ T h k (insert l x) r
    | otherwise = balance $ T h k l (insert r x)


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