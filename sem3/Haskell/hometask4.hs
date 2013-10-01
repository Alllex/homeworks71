

data Color = Red|Green|Blue

colors = [Red, Green, Blue]

colorsString x = map (\s -> case s of
                              Red   -> "Red"
                              Green -> "Green"
                              Blue  -> "Blue"
                     ) x                               
                     
data Sound = Sound Int Int

sounds = map (Sound 1) [3,4,5,6] 
inRange (Sound _ j) = j >= 20 && j <= 2000 

data L a = Cons a (L a) | Nil
-- data List a = a ':' (List a) | []

len Nil = 0
len (Cons _ l) = len l + 1

data Tree a = E | T (Tree a) a (Tree a)

nnodes E = 0
nnodes (T l _ r) = nnodes l + nnodes r + 1

nleafs E = 0
nleafs (T E _ E) = 1
nleafs (T l _ r) = nleafs l + nleafs r

depth E = 0
depth (T l _ r) = 1 + max (depth l) (depth r)

leafes = leafes' [] where
  leafes' acc E = acc
  leafes' acc (T E x E) = x:acc
  leafes' acc (T l x r) = leafes' (leafes' acc l) r

maxn E = error "!!!"
maxn (T l x r) = maxn' (maxn' x l) r where
  maxn' x E = x
  maxn' x (T l y r) = maxn' (maxn' (max x y) l) r  
  
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

isTree :: Tree Integer -> Bool
isTree E = True
isTree (T E _ E) = True
isTree (T E x (T E y E)) = x < y
isTree (T (T E y E) x E) = x > y
isTree (T l@(T _ y _) x r@(T _ z _)) = y < x && x < z && isTree l && isTree r

elements :: Tree Integer -> [Integer]
elements E = []
elements (T l x r) = (elements l) ++ [x] ++ (elements r)