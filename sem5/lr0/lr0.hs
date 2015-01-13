
import System.Environment
import Data.Char
import Data.List
import Debug.Trace

data Rule = R Int String [String] deriving (Show, Eq)
data Grammar = G [Rule] deriving (Show, Eq) 
data Situation = S Int String [String] [String] deriving Eq
type State = [Situation]
data DotGraph = DG [State] [(Int, Int, Symbol)] 
type Symbol = String
type Symbols = [Symbol]

fktrace s f x = trace (s ++ show (f x)) x

instance Show Situation where
    show (S i hd bef aft) = "{" ++ hd ++ " -> " ++ concat bef ++ "." ++ concat aft ++ "}"

instance Ord Situation where
    compare (S i _ _ _) (S j _ _ _) = compare i j 

instance Show DotGraph where
    show (DG ns es) = "nodes:" ++ concat (map (("\n"++) . show) ns) ++ "\nedges:" ++ concat (map (("\n"++) . show) es)

mkRule :: (Int, Symbols) -> Rule
mkRule (i, [hd, _]) = R i hd [""]
mkRule (i, (hd:_:body)) = R i hd body
mkRule _ = error "FAIL"

isTerm :: String -> Bool
isTerm (c:_) = not $ isUpper c
isTerm _ = False

syms :: Grammar -> Symbols
syms (G g) = nub $ concat [hd:tl | (R i hd tl) <- g, tl /= [""]]

-----------------------------------------------------------------------------

closure :: Grammar -> [Situation] -> State
closure (G g) sits = cl [] sits where --fktrace ("closure of " ++ show sits ++ " is \n") id (cl [] sits) where
     compl ok (S _ _ _ (b:_)) | isTerm b = []
                              | True = filter ok [S i hd [] tl | (R i hd tl) <- g, b == hd]
     compl _ _ = []
     nelem one two e = not $ (elem e one || elem e two)
     cl acc [] = nub acc
     cl acc (s:ss) = cl (acc ++ [s]) (ss ++ compl (nelem acc ss) s)

goto :: Grammar -> State -> Symbol -> State
goto g sits x = closure g moved where
    move (S i hd bef (b:aft)) | b /= x = Nothing
                              | True = Just $ S i hd (bef ++ [b]) aft
    move _ = Nothing
    moved = foldr (\s acc -> case move s of Nothing -> acc; Just a -> a:acc) [] sits

mkGraph :: Grammar -> DotGraph
mkGraph g@(G rs@((R _ ax _):_)) = mkg [] [] [fstClosure] where
    fstClosure = nub $ concat [closure g [S i hd [] tl] | (R i hd tl) <- rs, ax == hd]
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

showNode (i, state) = "\n\tnode_" ++ show i ++ " [label=\"" ++ intercalate "\\n" (map sn $ sort state) ++ "\"];" where
    sn (S i hd bef aft) = hd ++ " -> " ++ concat bef ++ "&bull;" ++ concat aft

showEdge i j lbl = "\n\tnode_" ++ show i ++ " -> node_" ++ show j ++ " [label=\"" ++ lbl ++ "\"]";

printGraph :: DotGraph -> IO()
printGraph dg@(DG ns es) = do
    let text =  "digraph G {\n\trankdir=LR;\n" ++
                concat (map showNode $ zip [1..] ns) ++ "\n" ++
                concat (map (\(i, j, sym) -> showEdge (i + 1) (j + 1) sym) es) ++ "\n}"
    putStrLn text

-----------------------------------------------------------------------------

main' grammarText = do
    let rules = map words $ filter (""/=) $ lines grammarText
    let g = G $ map mkRule $ zip [1..] rules
    printGraph (mkGraph g)

main = do
    args <- getArgs
    case args of
        [grammarFile] -> do
            grammarText <- readFile grammarFile
            main' grammarText
        --_ -> do
        --    let grammarFile = "session.g"
        --    grammarText <- readFile grammarFile
        --    main' grammarText

        _ -> putStrLn "Wrong parameters count.\nUsage: $ runhaskell {this file name} {grammar file name}"
