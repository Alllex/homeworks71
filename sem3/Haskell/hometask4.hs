{-
    Hometask #4
    Author: Alex Semin Math-Mech 271 2013
    2013 (c)

    #1 Binary Trees
-}

data Tree a = E | T (Tree a) a (Tree a)

nnodes :: Tree Integer -> Integer
nnodes E = 0
nnodes (T l _ r) = nnodes l + nnodes r + 1

nleafs :: Tree Integer -> Integer
nleafs E = 0
nleafs (T E _ E) = 1
nleafs (T l _ r) = nleafs l + nleafs r

depth :: Tree Integer -> Integer
depth E = 0
depth (T l _ r) = 1 + max (depth l) (depth r)

leafes :: Tree Integer -> [Integer]
leafes = leafes' [] where
  leafes' acc E = acc
  leafes' acc (T E x E) = x:acc
  leafes' acc (T l x r) = leafes' (leafes' acc l) r

maxn :: Tree Integer -> Integer
maxn E = error "!!!"
maxn (T l x r) = maxn' (maxn' x l) r where
  maxn' x E = x
  maxn' x (T l y r) = maxn' (maxn' (max x y) l) r  
  
factor :: Integer -> Tree Integer -> (Integer, Integer)
factor n E = (0,0)
factor n (T l x r) = (factor n l !+ factor n r) !+ if x < n then (1,0) else (0,1) where
  (a,b) !+ (c,d) = (a + c,b + d)

insert :: Tree Integer -> Integer -> Tree Integer 
insert E a = T E a E
insert t@(T l x r) a
    | x == a = t
    | x < a = T l x $ insert r a
    | otherwise = T (insert l a) x r

find :: Tree Integer -> Integer -> Maybe (Tree Integer)
find E _ = Nothing
find t@(T l x r) a
    | x == a = Just t
    | x > a = find l a
    | otherwise = find r a

-- make leaf
lf :: Integer -> Tree Integer
lf x = T E x E

isTree :: Tree Integer -> Bool
isTree t = isT t Nothing Nothing where 
    isT E _ _ = True
    isT (T l x r) mn mx = let jx = Just x in 
        mn < jx && (mx == Nothing || jx < mx) && isT l mn jx && isT r jx mx
             

elements :: Tree Integer -> [Integer]
elements t = elems t [] where
    elems (T l x r) acc = elems l $ x:elems r acc
    elems _ acc = acc