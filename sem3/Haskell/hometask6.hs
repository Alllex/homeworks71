
-- empty, insert, find, remove, fold

{- 
    Hometask #5
    Balanced trees
-}
--                                 (   -pair-   )
--           -empty-      -height- -key- -values- -left-  -right-
data M a b =    E    | T  Integer   (a,    [b])   (M a b) (M a b)

-- simple way for displaying trees
instance (Show a, Show b) => Show (M a b) where
    show E = "E"
    show (T _ (k, v:_) E E) = "leaf["++(show k)++", "++(show v)++"]"
    show (T h (k, v:_) l r) = "T(h"++(show h)++" p["++(show k)++", "++(show v)++"] "++(show l)++" "++(show r)++")"

-----------------------------------------------------------

empty = E

fh E = 0
fh (T h _ _ _) = h
mk p l r = T (max (fh l) (fh r) + 1) p l r
diff (T _ _ l r) = (fh r) - (fh l)
rotateR (T _ p (T _ lp ll lr) r) = mk lp ll (mk p lr r)
rotateL (T _ p l (T _ rp rl rr)) = mk rp (mk p l rl) rr
balance node@(T h p l r) =
    if diff node >  1 then rotateL $ mk p l (if diff r > 0 then r else rotateR r) else 
    if diff node < -1 then rotateR $ mk p (if diff l < 0 then l else rotateL l) r else node

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

fold f m acc = foldr (\(k, v) acc' -> f k v acc') acc $ elements m
    where elements m = elems m [] 
              where elems (T _ (k, v:_) l r) acc = elems l $ (k, v):elems r acc
                    elems _ acc = acc

-----------------------------------------------------------

elems m = fold (\a b c -> (a,b):c) m []