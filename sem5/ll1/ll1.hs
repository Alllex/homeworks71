
import System.Environment
import Data.Char
import qualified Data.List as List
import qualified Data.Set  as Set
import qualified Data.Map  as Map
import Debug.Trace

data Rule = R String [String] deriving (Show, Eq)
data Grammar = G [Rule] deriving (Show, Eq) 
type SS = Set.Set String
type FF = Map.Map String SS
type Symbol = String
type Symbols = [Symbol]
type ParsingTable = Map.Map String (Maybe Rule) 


newtype SetShow = SetShow SS
newtype FFShow = FFShow FF
newtype PTShow = PTShow ParsingTable

--putList 

--instance Show Rule where
--    show (R hd tl) = 

instance Show SetShow where
    show (SetShow s) = "{ " ++ inner (Set.toList s) ++ " }"
        where inner [] = ""
              inner (x:xs) = foldl (\acc x -> acc ++ ", " ++ x) x xs

instance Show FFShow where
    show (FFShow m) = foldl (\a (k, v) -> concat [a, "\n", k, " -> ", show v]) "" (Map.toList m)

instance Show PTShow where
    show (PTShow pt) = "Parsing Table: " ++ foldl (\a (k, v) -> concat [a, "\n(", k, ") -> ", show v]) "" lst
        where lst = [(pair, w) | (pair, v) <- (Map.toList pt), v /= Nothing, let (Just w) = v]

sUni = Set.union
sEmp = Set.empty
sMem = Set.member
sSin = Set.singleton
sDel = Set.delete
sLst x = Set.fromList x
mIns k v m = Map.insert k v m
mEmp = Map.empty
mLst = Map.fromList
(!) = (Map.!)

fktrace s f x = trace (s ++ show (f x)) x

mkRule :: Symbols -> Rule
mkRule [hd, _] = R hd [""]
mkRule (hd:_:body) = R hd body
mkRule _ = error "FAIL"

isTerm :: String -> Bool
--isTerm "" = error "Epses is not allowed"
isTerm (c:_) = not $ isUpper c
isTerm _ = False

eps :: String
eps = ""

endmarker :: String
endmarker = "$"

mkKey :: Symbol -> Symbol -> String
mkKey nt t = nt ++ ", " ++ t

isEpsRule :: Rule -> Bool
isEpsRule (R _ tl) = tl == [eps]

syms :: Grammar -> Symbols
syms (G g) = List.nub $ concat [hd:tl | (R hd tl) <- g, tl /= [""]]

nonterms :: Grammar -> Symbols
nonterms g = filter (not . isTerm) $ syms g

terms :: Grammar -> Symbols
terms g = filter isTerm $ syms g

axiom :: Grammar -> Symbol
axiom (G ((R a _):_)) = a
axiom _ = error "There is no axiom in empty grammar"

initFF :: Symbols -> FF
initFF syms = mLst $ map (\s -> (s, sEmp)) syms

firstSeq :: FF -> Symbols -> SS
firstSeq _ [""] = sSin eps
firstSeq _ [] = sEmp
firstSeq ff (s:ss) = sUni ofS (if maySkip then firstSeq ff ss else sEmp)
    where ofS = ff ! s
          maySkip = sMem eps ofS

mkFirsts :: Grammar -> FF
mkFirsts g = mk empt where
    empt = initFF $ syms g
    mk ff = if next == ff then next else mk next where
        first (G g) ff sym = 
            if isTerm sym then sSin sym
            else foldl sUni (ff ! sym) $ map (firstSeq ff) $ prods sym 
            where prods s = [tl | (R hd tl) <- g, hd == s]
        next = Map.mapWithKey (\s _ -> first g ff s) ff

mkFollows :: Grammar -> FF -> FF
mkFollows gr@(G g@((R st _):_)) ff = upd start where
    empt = initFF $ nonterms gr
    start = mIns st (sSin endmarker) empt
    fromProd flws _ [] = flws
    fromProd flws a [b] 
        | isTerm b = flws 
        | otherwise = mIns b (get b `sUni` get a) flws where get = (flws !)
    fromProd flws a (b:beta)
        | isTerm b = fromProd flws a beta
        | otherwise = fromProd flws' a beta where 
            fstBeta = firstSeq ff beta
            maySkip = sMem eps fstBeta
            flwB = flws ! b
            extra = if maySkip then flws ! a else sEmp
            flwB' = flwB `sUni` (sDel eps fstBeta) `sUni` extra
            flws' = mIns b flwB' flws
    woEpses = filter (not . isEpsRule) g
    upd flws = if flws == next then next else upd next where
        next = foldl (\acc (R hd tl) -> fromProd acc hd tl) flws woEpses

mkParseTable :: Grammar -> ParsingTable
mkParseTable g@(G rs) = foldl fromRule empt rs where
    nts = nonterms g
    ts = terms g ++ [endmarker]
    empt = mLst [(mkKey nt t, Nothing) | nt <- nts, t <- ts]  -- foldl (\m nt -> foldl (\m' t -> mIns (nt, t) Nothing m') m ts) mEmp nts
    fs = mkFirsts g
    --fs = fktrace "fs: " FFShow fs'
    fl = mkFollows g fs 
    --fl = fktrace "fl: " FFShow fl'
    fromRule :: ParsingTable -> Rule -> ParsingTable
    fromRule m r@(R a alpha) = newTable where
        trms = Set.filter isTerm
        tsInFstAlpha = firstSeq fs alpha
        tsInFlwA = trms $ fl ! a
        maySkip = sMem eps tsInFstAlpha
        upd = Set.fold (\t pt -> mIns (mkKey a t) (Just r) pt)
        m' = upd m $ trms tsInFstAlpha
        m'' = if not maySkip then m' else upd m' tsInFlwA
        newTable = m''


parse :: Grammar -> Symbols -> IO ()
parse g input = pp (input ++ [endmarker]) [axiom g, endmarker] where
    pt' = mkParseTable g
    pt = fktrace "" PTShow pt'
    m x a = (!) pt $ mkKey x a
    getHead Nothing = error "Cannot get head of Nothing"
    getHead (Just (R hd _)) = hd
    pp [e] _
        | e == endmarker =
            putStrLn "{END}"
        | otherwise = error "Illegal last stack symbol" 
    pp w@(a:next) (x:st)
        | a == x = do
            putStrLn $ "match " ++ a
            pp next st
        | isTerm x = error $ "Incorrect input terminal: " ++ a
        | m x a == Nothing = error $ "There is no such rule in parsing table m" ++ show (x, a)
        | x == (getHead $ m x a) = do
                let mr = m x a
                case mr of
                    Nothing -> error "Illegal state"
                    Just r@(R _ tl) -> do
                        putStrLn $ show r
                        if isEpsRule r then pp w st
                        else pp w $ tl ++ st 
        | otherwise = error "Unknown error"
    pp _ _ = error "Unknown error: Inconsistent state of stack and input"

main' grammarText inputText = do
    let rules = map words (filter (""/=) $ lines grammarText)
    let g = G $ map mkRule rules
    let input = words inputText
    --putStrLn $ "Symbols:" ++ (show $ syms g)
    --let firsts = mkFirsts g
    --let follows = mkFollows g firsts
    --putStrLn $ "Grammar: " ++ show g
    --putStrLn $ "FIRSTS: " ++ (show (FFShow firsts))
    --putStrLn $ "FOLLOWS: " ++ (show (FFShow follows))
    --let pt = mkParseTable g
    --putStrLn $ show (PTShow pt)
    parse g input


local_grammar = "\
\   E -> T E'       \n\
\   E' -> + T E'    \n\
\   E' ->           \n\
\   T -> F T'            \n\
\   T' -> * F T'             \n\
\   T' ->            \n\
\   F -> ( E )           \n\
\   F -> id            "

local_input = "id + id * id"

local_grammar2 = "\
\   S -> i E t S S'       \n\
\   S -> a    \n\
\   S' -> e S          \n\
\   S' ->             \n\
\   E -> b            "

stupid = "S -> "

main = do
    args <- getArgs
    case args of
        [fileName] -> do
            f <- readFile fileName
            main' f []
        _ -> do
            main' local_grammar local_input
