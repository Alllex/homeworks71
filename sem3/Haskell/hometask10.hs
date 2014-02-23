
class Array a where
    getA    :: a x -> Int -> a
    putA    :: a x -> Int -> a -> a x
    subA    :: a x -> Int -> Int -> a x
    foldA   :: (b -> a -> b) -> b -> a x -> b
    mapA    :: (a -> b) -> a x -> a b
    lengthA :: a x -> Int

data A a = A [a] deriving Show

instance Array A a where

    getA (A arr) i = get arr i
        where get (x:_) 0  = x
              get (_:xs) n = get xs (n-1)
              get _ _    = error "Out of bound"

    putA (A arr) i x' = A $ put [] arr i
        where put acc (x:xs) 0 = acc ++ (x':xs)
              put acc (x:xs) i = put (acc ++ [x']) xs (i-1)
              put _ _ _ = error "Out of bound"

    subA (A arr) s f = A $ sub [] arr 0
        where sub acc (x:xs) i = if i > f then acc else sub (if i >= s then acc ++ [x] else acc) xs (i+1)
              sub _ _ _ = error "Out of bound"

    foldA f acc (A arr) = foldl f acc arr

    mapA f (A arr) = A $ map f arr

    lengthA (A arr) = length arr


makeA n f = A [ f x | x <- [0..(n-1)] ]

arr = makeA 5 id