
{-
    Generate lists of (n - 1) element.
    Each list is the original one with only skipped item.
-}

ff :: [a] -> [[a]]
ff [] = error "f :: Cannot handle empty list"
ff [x] = [[]]
ff (x:xs) = xs : map (x:) (ff xs)

{-

-}

ss [] = [[]]
ss (x:xs) = let s = ss xs in s ++ map (x:) s