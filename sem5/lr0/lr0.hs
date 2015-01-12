
import System.Environment
import Data.Char
import Data.List
import Debug.Trace

data Rule = R String [String] deriving (Show, Eq)
data Grammar = G [Rule] deriving (Show, Eq) 
data Situation = S String [String] [String] deriving Eq
type State = [Situation]
data DotGraph = DG [State] [(Int, Int, Symbol)] 
type Symbol = String
type Symbols = [Symbol]

fktrace s f x = trace (s ++ show (f x)) x

instance Show Situation where
    show (S hd bef aft) = "{" ++ hd ++ " -> " ++ concat bef ++ "." ++ concat aft ++ "}"

instance Show DotGraph where
    show (DG ns es) = "nodes:" ++ concat (map (("\n"++) . show) ns) ++ "\nedges:" ++ concat (map (("\n"++) . show) es)

mkRule :: Symbols -> Rule
mkRule [hd, _] = R hd [""]
mkRule (hd:_:body) = R hd body
mkRule _ = error "FAIL"

isTerm :: String -> Bool
isTerm (c:_) = not $ isUpper c
isTerm _ = False

eps :: String
eps = ""

isEpsRule :: Rule -> Bool
isEpsRule (R _ tl) = tl == [eps]

syms :: Grammar -> Symbols
syms (G g) = nub $ concat [hd:tl | (R hd tl) <- g, tl /= [""]]

nonterms :: Grammar -> Symbols
nonterms g = filter (not . isTerm) $ syms g

terms :: Grammar -> Symbols
terms g = filter isTerm $ syms g

axiom :: Grammar -> Symbol
axiom (G ((R a _):_)) = a
axiom _ = error "There is no axiom in empty grammar"

-----------------------------------------------------------------------------

closure :: Grammar -> [Situation] -> State
closure (G g) sits = cl [] sits where --fktrace ("closure of " ++ show sits ++ " is \n") id (cl [] sits) where
     compl ok (S _ _ (b:_)) | isTerm b = []
                            | True = filter ok [S hd [] tl | (R hd tl) <- g, b == hd]
     compl _ _ = []
     nelem one two e = not $ (elem e one || elem e two)
     cl acc [] = nub acc
     cl acc (s:ss) = cl (acc ++ [s]) (ss ++ compl (nelem acc ss) s)

goto :: Grammar -> State -> Symbol -> State
goto g sits x = closure g moved where
    move (S hd bef (b:aft)) | b /= x = Nothing
                            | True = Just $ S hd (bef ++ [b]) aft
    move _ = Nothing
    moved = foldr (\s acc -> case move s of Nothing -> acc; Just a -> a:acc) [] sits

mkGraph :: Grammar -> DotGraph
mkGraph g@(G ((R hd tl):_)) = mkg [] [] [closure g [S hd [] tl]] where
    syms' = syms g
    mkg :: [State] -> [(Int, Int, Symbol)] -> [State] -> DotGraph
    mkg nodes edges [] = DG nodes edges
    mkg ns es (st:ss) = mkg nns (es ++ nes) (ss ++ newStates) where
        togo = filter (([]/=) . snd) $ map (\sym -> (sym, goto g st sym)) syms'
        --togo = trace ("state: " ++ show st) togo'
        newStates = filter (\s -> not $ elem s ns || elem s ss) $ map snd togo
        --newStates = fktrace "new states: " id newStates'
        nns = ns ++ (if elem st ns then [] else [st]) ++ newStates
        --nns = fktrace "nns: " id nns'
        ind s = case elemIndex s nns of Nothing -> error ("State without number: " ++ show s); Just n -> n
        stInd = ind st
        nes = map (\(sym, s) -> (stInd, ind s, sym)) togo
        --nes = fktrace "new edges: " id nes'
mkGraph _ = error "Grammar has no rules"

-----------------------------------------------------------------------------

lexer :: String -> [String]
lexer inputText = lx inputText where
    lx [] = []
    lx ('\\':'\\':cs) = "\\\\" : lx cs
    lx ('\\':' ':cs) = "\\ " : lx cs
    lx ('\\':'n':cs) = "\\n" : lx cs
    lx "\\" = ["\\ "] -- error "Incorrect input: found lonely backslash \'\\\'"
    lx (c:cs) | elem c " \n\t\r" = lx cs
              | otherwise = [c] : lx cs

showNode (i, state) = "\n\tnode_" ++ show i ++ " [label=\"" ++ intercalate "\\n" (map sn state) ++ "\"];" where
    sn (S hd bef aft) = hd ++ " -> " ++ concat bef ++ "&bull;" ++ concat aft

showEdge i j lbl = "\n\tnode_" ++ show i ++ " -> node_" ++ show j ++ " [label=\"" ++ lbl ++ "\"]";

printGraph :: String -> DotGraph -> IO()
printGraph filename (DG ns es) = do
    let text =  "digraph G {\n\trankdir=LR;\n" ++
                concat (map showNode $ zip [1..] ns) ++ "\n" ++
                concat (map (\(i, j, sym) -> showEdge (i + 1) (j + 1) sym) es) ++ "\n}"
    writeFile filename text
-----------------------------------------------------------------------------

main' dotname grammarText = do
    let rules = map words $ filter (""/=) $ lines grammarText
    let g = G $ map mkRule rules
    --let (G rs) = g
    --putStrLn $ show $ closure g [head rs]
    printGraph dotname (mkGraph g)

main = do
    args <- getArgs
    case args of
        [grammarFile] -> do
            grammarText <- readFile grammarFile
            main' (takeWhile ((/=) '.') grammarFile ++ ".dot") grammarText
        --_ -> do
        --    grammarText <- readFile "in"
        --    main' ("in.dot") grammarText

        _ -> putStrLn "Wrong parameters count.\nUsage: $ runhaskell {this file name} {grammar file name}"
