{-
    Hometask #6
    Author: Alex Semin Math-Mech 271 2013
    2013 (c)

    #1 Map based on AVL trees
    #2 Map based on Lists
-}


class Map m a b where
    empty :: m a b
    insert :: m a b -> a -> b -> m a b
    find :: m a b -> a -> Maybe b
    remove :: m a b -> a -> m a b
    fold :: (a -> b -> c -> c) -> m a b -> c -> c

----------- implementation via AVL tree --------------

--                                 (   -pair-   )
--           -empty-      -height- -key- -values- -left-  -right-
data M a b =    E    | T  Integer   (a,    [b])   (M a b) (M a b)

instance (Show a, Show b) => Show (M a b) where
    show E = "E"
    show (T h p E E) = "leaf[h("++(show h)++") p["++(show p)++"]"
    show (T h p l r) = "T(h"++(show h)++" p["++(show p)++"] "++(show l)++" "++(show r)++")"

fh E = 0
fh (T h _ _ _) = h
mk p l r = T (max (fh l) (fh r) + 1) p l r
diff (T _ _ l r) = (fh r) - (fh l)
rotateR (T _ p (T _ lp ll lr) r) = mk lp ll (mk p lr r)
rotateL (T _ p l (T _ rp rl rr)) = mk rp (mk p l rl) rr
balance node@(T h p l r) =
    if diff node >  1 then rotateL $ mk p l (if diff r > 0 then r else rotateR r) else 
    if diff node < -1 then rotateR $ mk p (if diff l < 0 then l else rotateL l) r else node

instance Ord a => Map M a b where

    empty = E

    insert E k v = T 1 (k, [v]) E E
    insert t@(T h p@(k, vs) l r) k' v'
        | k' > k    = balance $ mk p l (insert r k' v')
        | k' < k    = balance $ mk p (insert l k' v') r
        | otherwise = T h (k, v':vs) l r

    find E _ = Nothing
    find (T _ (k, v:_) l r) k'
        | k' > k    = find r k'
        | k' < k    = find l k'
        | otherwise = Just v

    remove E _ = E
    remove t@(T h p@(k, vs) l r) k'
        | k' > k    = balance $ mk p l (remove r k')
        | k' < k    = balance $ mk p (remove l k') r
        | otherwise = 
            case vs of 
                [v]   -> case r of E -> l; _ -> let (k', r') = rm r in balance $ mk k' l r'
                v:vs' -> T h (k, vs') l r   
        where
            rm (T _ k E r) = (k, r)
            rm (T _ k l r) = let (k', l') = rm l in (k', balance $ mk k l' r)

    fold f m acc = foldr (\(k, v) acc' -> f k v acc') acc $ elems m []
        where elems (T _ (k, v:_) l r) acc = elems l $ (k, v):elems r acc
              elems _ acc = acc

----------- implementation via List ------------

data ML a b = ML [(a, b)] deriving Show

instance Ord a => Map ML a b where
    empty = ML []
    insert (ML m) k v = ML $ (k, v):m
    find (ML m) k = case filter ((k ==) . fst) m of [] -> Nothing; (_, v):_ -> Just v
    remove (ML m) k' = ML $ rm [] m
        where rm acc [] = acc
              rm acc (x@(k, _):xs) = if k == k' then acc ++ xs else rm (acc ++ [x]) xs
    fold f (ML m) acc = foldl (\acc' (k, v) -> f k v acc') acc m
