\begin{code}
{-# LANGUAGE ViewPatterns #-}
module ReachingDefinition(formatEquations, 
                          ReachingDefinitions, 
                          ReachingDefinition,
                          reachingDefinitions, 
                          formatReachingDefinitions) where

import AST
import ControlFlow

import Data.List(intercalate)
import qualified Data.Map as M
import qualified Data.Set as S

-- A ReachingDefinition is a set of String variable names to
-- Maybe Int where Just l is the last known label assignment and
-- Nothing indicates that it is unknown when the element was last
-- assigned.
type ReachingDefinition = S.Set (String, Maybe Int)

-- A ReachingDefinitions contains two maps from Int to ReachingDefinitions.
-- The Int key is the label and the ReachingDefinition is the definition
-- associated with that label.
data ReachingDefinitions = RDS {entry :: M.Map Int ReachingDefinition,
                                exit  :: M.Map Int ReachingDefinition}

type EntryDefs = M.Map Int ReachingDefinition
type ExitDefs = M.Map Int ReachingDefinition

type KillSet = ReachingDefinition
type GenSet = ReachingDefinition
type ExitEquation = (Int, KillSet, GenSet)

type EntryEquation = (Int, S.Set Int)

-- Given a ControlFlowGraph, recahingDefinitions returns the 
-- ReachingDefinitions for the provided ControlFlowGraph. It 
-- is assumed that for each key in labels, there is also a key 
-- in outEdges and inEdges. If this condition is not met, it is 
-- unknown what the result of this function will be.
reachingDefinitions :: ControlFlowGraph -> ReachingDefinitions
reachingDefinitions cfg = RDS entries exits where
  (entries, exits) = reachingDefinitions' (empties, empties) cfg
  empties = M.unions . map ((flip M.singleton) S.empty) $ lbls
  lbls = M.keys . labels $ cfg


-- Given a ControlFlowGraph, formatEquations returns a human
-- readable String showing the entry, RD○(x), and exit, RD●(x), 
-- equations for each label in the ControlFlowGraph. For example,
-- > putStrLn . formatEquations $ simpleGraph
-- RD○(0) = {(a, ?), (b, ?), (x, ?), (y, ?)} ∪ {}
-- RD○(1) = RD●(0)
-- RD○(2) = RD●(1) ∪ RD●(4)
-- RD○(3) = RD●(2)
-- RD○(4) = RD●(3)
-- RD○(5) = RD●(2)
-- RD●(0) = RD○(0) ∖ {(x, ?), (x, 0), (x, 1), (x, 2), 
--                    (x, 3), (x, 4), (x, 5)} ∪ {(x, 0)}
-- RD●(1) = RD○(1) ∖ {(y, ?), (y, 0), (y, 1), 
--                    (y, 2), (y, 3), (y, 4), (y, 5)} ∪ {(y, 1)}
-- RD●(2) = RD○(2) ∖ {} ∪ {}
-- RD●(3) = RD○(3) ∖ {(x, ?), (x, 0), (x, 1), 
--                    (x, 2), (x, 3), (x, 4), (x, 5)} ∪ {(x, 3)}
-- RD●(4) = RD○(4) ∖ {(a, ?), (a, 0), (a, 1), 
--                    (a, 2), (a, 3), (a, 4), (a, 5)} ∪ {(a, 4)}
-- RD●(5) = RD○(5) ∖ {(b, ?), (b, 0), (b, 1), 
--                    (b, 2), (b, 3), (b, 4), (b, 5)} ∪ {(b, 5)}
formatEquations :: ControlFlowGraph -> String
formatEquations cfg = entries ++ "\n" ++ exits where
  entries = intercalate "\n" . map (formatEntryE vars) . 
             entryEquations $ cfg
  exits = intercalate "\n" . map formatExitE . exitEquations $ cfg
  vars = determineVars cfg
  
-- Given the ReachingDefinitions of a ControlFlowGraph, 
-- formatReachingDefinitions returns a human readable String 
-- showing the entry, RD○(x), and exit, RD●(x), ReachingDefinition 
-- for each label. For example,
-- > putStrLn . formatReachingDefinitions . 
--       reachingDefinitions $ simpleGraph
-- RD○(0) = {(a, ?),(b, ?),(x, ?),(y, ?)}
-- RD○(1) = {(a, ?),(b, ?),(x, 0),(y, ?)}
-- RD○(2) = {(a, ?),(a, 4),(b, ?),(x, 0),(x, 3),(y, 1)}
-- RD○(3) = {(a, ?),(a, 4),(b, ?),(x, 0),(x, 3),(y, 1)}
-- RD○(4) = {(a, ?),(a, 4),(b, ?),(x, 3),(y, 1)}
-- RD○(5) = {(a, ?),(a, 4),(b, ?),(x, 0),(x, 3),(y, 1)}
-- RD●(0) = {(a, ?),(b, ?),(x, 0),(y, ?)}
-- RD●(1) = {(a, ?),(b, ?),(x, 0),(y, 1)}
-- RD●(2) = {(a, ?),(a, 4),(b, ?),(x, 0),(x, 3),(y, 1)}
-- RD●(3) = {(a, ?),(a, 4),(b, ?),(x, 3),(y, 1)}
-- RD●(4) = {(a, 4),(b, ?),(x, 3),(y, 1)}
-- RD●(5) = {(a, ?),(a, 4),(b, 5),(x, 0),(x, 3),(y, 1)}
formatReachingDefinitions :: ReachingDefinitions -> String
formatReachingDefinitions (RDS entries exits) = 
  (formatEntryDefs entries) ++ "\n" ++ (formatExitDefs exits)

-- simpleGraph:
-- 0: [x := 0]
-- 1: [y := 1]
-- while 2: [x < a + b] do
--   3: [x := x + a]
--   4: [a := a - b]
-- od
-- 5: [b := b + x]
simpleGraph :: ControlFlowGraph
simpleGraph = CFG labels outEdges inEdges where
  labels = M.fromList [(0, Left (Assign "x" (Number 0))),
                       (1, Left (Assign "y" (Number 1))),
                       (2, Right (RelOp Less (Var "x") 
                                  (BinOp Plus (Var "a") (Var "b")))),
                       (3, (Left (Assign "x" 
                                  (BinOp Plus (Var "x") (Var "a"))))),
                       (4, (Left (Assign "a" 
                                  (BinOp Minus (Var "a") (Number 1))))),
                       (5, (Left (Assign "b" 
                                  (BinOp Plus (Var "b") (Var "x")))))]
  outEdges = M.fromList [(0, S.singleton 1),
                         (1, S.singleton 2),
                         (2, S.fromList [3, 5]),
                         (3, S.singleton 4),
                         (4, S.singleton 2),
                         (5, S.empty)]
  inEdges = M.fromList [(0, S.empty),
                        (1, S.singleton 0),
                        (2, S.fromList [1, 4]),
                        (3, S.singleton 2),
                        (4, S.singleton 3),
                        (5, S.singleton 2)]

reachingDefinitions' :: (EntryDefs, ExitDefs) -> ControlFlowGraph -> 
                        (EntryDefs, ExitDefs)
reachingDefinitions' (entries, exits) cfg = 
  if entries == entries' && exits == exits' 
    then (entries', exits') 
    else  reachingDefinitions' (entries', exits') cfg where
  (entries', exits') = pass cfg (entries, exits)

pass :: ControlFlowGraph -> (EntryDefs, ExitDefs) -> 
        (EntryDefs, ExitDefs)
pass cfg (entries, exits) = 
  pass' 0 vars lbls (S.empty) cfg (entries, exits) where
  lbls = S.fromList . M.keys . labels $ cfg
  vars = determineVars cfg

pass' :: Int -> S.Set String -> S.Set Int -> 
         S.Set Int -> ControlFlowGraph -> 
         (EntryDefs, ExitDefs) -> (EntryDefs, ExitDefs)
pass' l vars lbls marked cfg (entries, exits) =  
  if S.null nextLabels then (entries', exits') 
                     else (entries'', exits'') where
  (_,kill,gen) = getExitEquation lbls l (labels cfg M.! l)
  (_, entEq) = entryEquation l cfg
  exitSets = map (exits M.!) (S.toList entEq)
  nextEntry = if l == 0 then initialEntry vars else S.unions exitSets
  nextExit = nextEntry `S.difference` kill `S.union` gen
  nextLabels = (outEdges cfg M.! l) `S.difference` marked
  entries' = M.insert l nextEntry entries
  exits' = M.insert l nextExit exits
  recurse n = pass' n vars lbls (S.insert l marked) 
              cfg (entries', exits')
  branches = S.toList . S.map recurse $ nextLabels
  entries'' = mergeSets . map fst $ branches
  exits'' = mergeSets . map snd $ branches

mergeSets :: [M.Map Int ReachingDefinition] 
             -> M.Map Int ReachingDefinition
mergeSets maps = sets where
  set i = S.unions . map (M.! i) $ maps
  lbls = head . map M.keys $ maps 
  sets = M.unions . zipWith (M.singleton) lbls . map set $ lbls

initialEntry :: S.Set String -> ReachingDefinition
initialEntry = S.map (\str -> (str, Nothing))
   
formatEntryDefs :: EntryDefs -> String           
formatEntryDefs entries = intercalate "\n" defs where
  keys = M.keys entries
  defs = zipWith formatEntryDef keys (map (entries M.!) keys)

formatEntryDef :: Int -> ReachingDefinition -> String
formatEntryDef l def = "RD○(" ++ (show l) ++ ") = " ++ 
                       (formatReachingDef def)

formatReachingDef :: ReachingDefinition -> String
formatReachingDef (S.toList -> defs) = 
  "{" ++ (intercalate "," . map formatElement $ defs) ++ "}"

formatExitDefs :: ExitDefs -> String
formatExitDefs exits = intercalate "\n" defs where
  keys = M.keys exits
  defs = zipWith formatExitDef keys (map (exits M.!) keys)
  
formatExitDef :: Int -> ReachingDefinition -> String
formatExitDef l def = "RD●(" ++ (show l) ++ ") = " ++ 
                      (formatReachingDef def)
           

entryEquations :: ControlFlowGraph -> [EntryEquation]
entryEquations cfg = zip lbls sets where
  x = inEdges cfg
  lbls = M.keys . labels $ cfg
  sets = map (x M.!) lbls
  
entryEquation :: Int -> ControlFlowGraph -> EntryEquation
entryEquation l cfg = (l, (inEdges cfg) M.! l)
  
formatEntryE :: S.Set String -> EntryEquation -> String  
formatEntryE (S.toList -> vars) (l, es) 
  | l == 0 = "RD○(0) = {" ++ intercalate ", " 
              (map formatVar vars) ++ "} ∪ " ++ (formatEntries es)
  | otherwise = "RD○(" ++ (show l) ++ ") = " ++ (formatEntries es) 
                       
formatEntries :: S.Set Int -> String
formatEntries (S.toList -> es)  
  | null es = "{}"
  | otherwise = intercalate " ∪ " . map format $ es 
     where
       format i = "RD●(" ++ (show i) ++ ")"
  
formatVar :: String -> String      
formatVar s = "(" ++ s ++ ", ?)"
      
formatExitE :: ExitEquation -> String
formatExitE (l, kill, gen) = "RD●(" ++ (show l) ++ ") = " ++
                          "RD○(" ++ (show l) ++ ") " ++ 
                          "∖ {" ++ (formatDef kill) ++ "} " ++
                          "∪ {" ++ (formatDef gen) ++ "}"
                          
formatDef :: ReachingDefinition -> String
formatDef (S.toList -> elems) = intercalate ", " . 
                                 map formatElement $ elems

formatElement :: (String, Maybe Int) -> String
formatElement (str, Nothing) = "(" ++ str ++ ", ?)"
formatElement (str, Just x) = "(" ++ str ++ ", " ++ (show x) ++ ")"

exitEquations :: ControlFlowGraph -> [ExitEquation]
exitEquations cfg = [ getExitEquation set i (map M.! i) | i <- lbls ] 
  where
   map = labels cfg
   set = S.fromList lbls
   lbls = M.keys map  
  
getExitEquation :: S.Set Int -> Int -> Block -> ExitEquation
getExitEquation labels l block = (l, killSet labels block, 
                                  genSet l block)

killSet :: S.Set Int -> Block -> KillSet
killSet labels (Left (Assign var _)) = S.union 
 (S.singleton (var, Nothing)) . S.fromList . 
 zipWith (\ s i -> (s, Just i)) (repeat var) . S.toList $ labels
killSet _ _ = S.empty

genSet :: Int -> Block -> GenSet
genSet l (Left (Assign var _)) = S.singleton (var, Just l)
genSet _ _ = S.empty

determineVars :: ControlFlowGraph -> S.Set String
determineVars (labels -> M.elems -> cfg) = S.unions . map getVars $ cfg

getVars :: Block -> S.Set String
getVars (Left (Assign label arith)) = S.singleton label `S.union` 
                                       (getArithVars arith)
getVars (Right bool) = getBoolVars bool
getVars _ = S.empty

getBoolVars :: Boolean -> S.Set String
getBoolVars (BoolOp _ b0 b1) = S.union (getBoolVars b0) 
                               (getBoolVars b1)
getBoolVars (RelOp _ a0 a1) = S.union (getArithVars a0) 
                              (getArithVars a1)
getBoolVars _ = S.empty

getArithVars :: Arith -> S.Set String
getArithVars (Var label) = S.singleton label
getArithVars (BinOp _ a0 a1) = S.union (getArithVars a0) 
                               (getArithVars a1)
getArithVars _ = S.empty


\end{code}