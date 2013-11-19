{-
    Haskell - TEST#3
    Aleksey Semin
    19.12.13
-}

-- 1

data Goods = Wolf | Goat | Cole | Empty deriving Eq
          -- Wolf | Goat | Cole | Nothing
data Coast = L | R deriving (Show, Eq)
data Side = Si (Goods, Goods, Goods)
data State = St Coast Side Side

instance Show Goods where
    show Wolf  = "Wolf"
    show Goat  = "Goat"
    show Cole  = "Cole"
    show Empty = "Empt"

good (Wolf, Goat, _) = False
good (_, Goat, Cole) = False
good _ = True

start = (L, ((Wolf, Goat, Cole), (Empty, Empty, Empty)))
end = (R, ((Empty, Empty, Empty), (Wolf, Goat, Cole)))
switch a b = if a == Empty then b else Empty
triples = [(a, b, c) | a <- [Wolf, Empty], b <- [Goat, Empty], c <- [Cole, Empty]]
turns = foldl (\acc tr -> (map ((,) tr) $ f tr) ++ acc) [] triples
    where f (a, b, c) = (switch a Wolf, b, c):
                        (a, switch b Goat, c):
                        (a, b, switch c Cole):
                        (a, b, c):[]

cuts = [ (coast, (l, r)) | tr@(a, b, c) <- triples, 
                           coast <- [L, R], 
                           let l = tr, let r = (switch a Wolf, switch b Goat, switch c Cole),
                           case coast of L -> good r; R -> good l]

f solve@(state@(side, (l, r)):_) =
    if state == end then [solve]
    else foldl (\acc next -> acc ++ (f $ next:solve) ) [] nexts
         where vars = map snd $ filter ((l ==) . fst) turns
               nexts = [ cut | cut@(s, (lc, _)) <- cuts, 
                                s /= side, 
                                elem lc vars, not $ elem cut solve]

-- list of solves
-- A solve is a number of consequent world states from start to finish
-- I wish there were more than only 2 solutions...
problemSolve = map reverse $ f [start]

-- 2
data Gen = A | T | G | C deriving (Show, Eq)

swap = map f
    where f A = T
          f T = A
          f G = C
          f C = G
-- 3
fold f (g1:shift@(g2:g3:gs)) =
    case f (g1:g2:g3:[]) of 
        Just n -> n : fold f gs
        Nothing -> fold f shift
fold f _ = []
-- 4
expand f xs = concat $ map f xs