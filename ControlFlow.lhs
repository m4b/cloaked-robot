\section{Control Flow Diagrams}

In this section we compute the control flow graph for a given AST.

\begin{code}
{-# LANGUAGE TupleSections, ViewPatterns #-}
module ControlFlow where

import AST
import Control.Applicative
import Control.Monad.State
import Data.List(intercalate)
import qualified Data.Map as M
import qualified Data.Set as S
import Input
import Test.HUnit hiding (State)

\end{code}

A block is any part of a program that must be labeled.  For this
language, it's either a boolean expression, an assignment, or 
a skip.

\begin{code}
type Block = Either Statement Boolean

\end{code}

A Control Flow Graph is a collection of labeled blocks and edges.

\begin{code}
data ControlFlowGraph = CFG { labels :: M.Map Int Block, 
                              outEdges  :: M.Map Int (S.Set Int),
                              inEdges :: M.Map Int (S.Set Int)} deriving (Show, Eq)

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

\end{code}

|formatCFGasDot| takes a |ControlFlowGraph| and formats it as a DOT graph string.
For example:

'formatCFGasDOT simpleGraph' returns:

\begin{verbatim}
digraph {
l_0 [label="0: [x := 0]"]
l_1 [label="1: [y := 1]"]
l_2 [label="2: [x < a + b]"]
l_3 [label="3: [x := x + a]"]
l_4 [label="4: [a := a - 1]"]
l_5 [label="5: [b := b + x]"]
l_0 -> l_1
l_1 -> l_2
l_2 -> l_3
l_2 -> l_5
l_3 -> l_4
l_4 -> l_2
}
\end{verbatim}

Once compiled:

\begin{verbatim}dot -Tpng "simpleGraph.dot" > "simpleGraph.png"\end{verbatim}

Produces the image:

\begin{center}
\includegraphics[width=0.5\textwidth]{tests/simpleGraph.png}
\end{center}

\begin{code}
formatCFGasDOT :: ControlFlowGraph -> String
formatCFGasDOT cfg = "digraph {\n" ++ nodes ++ "\n" ++ edges ++ "\n}" 
  where
    nodes = formatLabels (labels cfg)
    edges = intercalate "\n" . zipWith formatEdges outks $ 
            (map (outE M.!) outks)
    outE = outEdges cfg
    outks = M.keys outE
 
formatLabels :: M.Map Int Block -> String
formatLabels (M.assocs -> labels) = intercalate "\n" . 
                                    map (uncurry formatLabel) $ labels
                                  
formatEdges :: Int -> S.Set Int -> String
formatEdges from toSet = intercalate "\n" . map (formatEdge from) $ 
                         S.toList toSet
                                  
formatEdge :: Int -> Int -> String                                  
formatEdge from to = "l_" ++ (show from) ++ " -> l_" ++ (show to)

formatLabel :: Int -> Block -> String
formatLabel i (Left state) = "l_" ++ (show i) ++ 
                             " [label=\"" ++ (show i) ++ ": [" 
                             ++ (pettyShowStatement state) ++ "]\"]"
formatLabel i (Right bool) = "l_" ++ (show i) ++ " [label=\"" ++
                             (show i) ++ ": [" ++ (pettyShowBool bool) 
                             ++ "]\"]"


\end{code}

The |decorate| function takes a program, and returns a map from a
label int to a block.

\begin{code}
decorate :: Statement -> M.Map Int Block
decorate = M.fromList . flip evalState 0 . decorate'

\end{code}

If a statement is an assignment, label it.

\begin{code}
decorate' :: Statement -> State Int [(Int,Block)]
decorate' a@(Assign s arith) = (:[]) <$> (,Left a) <$> getIncrement

\end{code}

If a statement is a |Skip|, label it.

\begin{code}
decorate' Skip = (:[]) <$> (,Left Skip) <$> getIncrement

\end{code}

If a statement is a sequence of statements, then label each one.

\begin{code}
decorate' (Seq s1 s2) = decorate2 s1 s2

\end{code}

If a statement is an if then else, label the then
and else statements, as well as the boolean expression.

\begin{code}
decorate' con@(If bool s1 s2) = (:) <$> (,Right bool) 
                                    <$> getIncrement
                                    <*> decorate2 s1 s2

\end{code}

If a statement is a while loop, label the boolean expression
and the statement in the loop.

\begin{code}
decorate' whl@(While bool s) = (:) <$> (,Right bool) <$>
                                    getIncrement <*> decorate' s

\end{code}

Decorate two statements and collect their results.

\begin{code}
decorate2 :: Statement -> Statement -> State Int [(Int,Block)]
decorate2 s1 s2 = (++) <$> (decorate' s1) <*> (decorate' s2)

\end{code}

Get the next label and then increment it for the next get call.

\begin{code}
getIncrement :: Num s => State s s
getIncrement = get >>= \i -> put (i+1) >> return i
  
\end{code}

Computes a control flow graph for a program.

\begin{code}
controlFlowGraph :: Statement -> ControlFlowGraph
controlFlowGraph g = CFG dec outs ins where
  dec = decorate g
  outs = computeSuccessors g (M.keysSet dec)
  ins = computePredecessors outs

\end{code}

|basicBlocks| calculates the first level of basic blocks for 
a program and returns them in a list.

\begin{code}
basicBlocks :: Statement -> [Statement]
basicBlocks (Seq s1 s2) = basicBlocks s1 ++ basicBlocks s2
basicBlocks s = [s]

\end{code}

|computeSuccessors| is a wrapper for |computeSuccessors'| which filters
out nonexistant labels.

\begin{code}
computeSuccessors s dec = M.map (S.filter (flip S.member dec))
                           (computeSuccessors' 0 (basicBlocks s))

\end{code}

|computeSuccessors| computes the successor sets for a block.
If a block is an assignment or skip, then the successor is the
next block. 

\begin{code}
computeSuccessors' i ((Assign _ _):ss) = 
 M.union m (computeSuccessors' (i+1) ss) where
  m = M.singleton i (S.singleton (i+1))
  
computeSuccessors' i (Skip:ss) = 
 M.union m (computeSuccessors' (i+1) ss) where
  m = M.singleton i (S.singleton (i+1))

\end{code}

If the block is an ``if'' statement, then compute successors of the ``then'' and ``else'', and
adjust the last blocks in both the ``then'' and ``else'' to be succeeded by the next basic block.

The boolean expression in the ``if'' statement is succeeded by both the ``then'' and ``else'' blocks.

\begin{code}
computeSuccessors' i ((If _ s1 s2):ss) = 
 M.unions [m,m1',m2',computeSuccessors' c2 ss] where
  m = M.singleton i (S.fromList [i+1,i+2])
  c1 = countBlocks s1
  c2 = countBlocks s2
  b1 = basicBlocks s1
  b2 = basicBlocks s2
  m1 = computeSuccessors' (i+1) b1
  m1' = M.adjust (\_ -> S.singleton (i+c1+c2+1)) (length b1 + i) m1 
  m2 = computeSuccessors' (i+c1+1) b2
  m2' = M.adjust (\_ -> S.singleton (i+c1+c2+1)) (length b2 + i) m2

\end{code}

If the block is a While loop then it's boolean expression is succeeded by
the first basic block in the loop, and the next first block after the loop.
The successor of the last basic block in the loop is adjusted to be succeeded
by the loop expression.

\begin{code}

computeSuccessors' i ((While _ s):ss) = 
 M.unions [m,m1',computeSuccessors' (c+i+1) ss] where
  m = M.singleton i (S.fromList [i+1,c+i+1])
  c = countBlocks s
  bs = basicBlocks s
  m1 = computeSuccessors' (i+1) bs
  m1' = updateLast m1 (length bs + i) i (last bs)
  
computeSuccessors' _ [] = M.empty

updateLast m i v (If _ s1 s2) = i2 where
  i1 = M.adjust (\_ -> S.singleton v) (i+1) m
  i2 = M.adjust (\_ -> S.singleton v) (i+2) i1
  
updateLast m i v _ = M.adjust (\_ -> S.singleton v) i m

\end{code}

|countBlocks| returns the number of total blocks (not basic) in a program.

\begin{code}

countBlocks (Seq s1 s2) = countBlocks s1 + countBlocks s2
countBlocks (If _ s1 s2) = 1 + (countBlocks s1 + countBlocks s2)
countBlocks (While _ s) = 1 + countBlocks s
countBlocks _ = 1

\end{code}

|computePredecessors| takes a collection of successor edges and returns
a collection of predecessor edges.  The predecessors of some block \emph b is
equivalent to the set of blocks with b as a successor.

\begin{code}

computePredecessors :: M.Map Int (S.Set Int)
                    -> M.Map Int (S.Set Int)
computePredecessors outs = M.fromList . map go . M.keys $ outs where
  go i = (i,labelsWith i)
  labelsWith i = S.fromList [j | (j,set) <- M.toList outs, S.member i set]
  
displayLabeledGraph :: M.Map Int Block -> IO ()
displayLabeledGraph = mapM_ (putStrLn . showBlock) . M.toList where
  showBlock (i,Left s) = "[" ++ (pettyShowStatement s) ++ "]" ++ (show i)
  showBlock (i,Right b) = "[" ++ (pettyShowBool b) ++ "]" ++ (show i)
  
parseGraph :: FilePath -> IO ControlFlowGraph
parseGraph fp = do
  eG <- sparse <$> (readFile fp)
  return $ case eG of
    Right s -> controlFlowGraph s
    Left e -> error $ "A horrible parse error occurred: " ++ (show e)
  
makeTest :: String -> FilePath -> ControlFlowGraph -> Test
makeTest name fp expected = TestLabel name $ TestCase $ (expected @=?) =<< parseGraph fp
    
ifExampleTest = makeTest "ifexample" "tests\\ifexample.txt" expectedIf
  
expectedIf = CFG labels outs ins where
  labels = M.fromList [(0,Left (Assign "x" (BinOp Times (Number 2) (Number 200)))),
                       (1,Right (RelOp Leq (Var "x") (Number 400))),
                       (2,Left Skip),(3,Left (Assign "y" (Number 10)))]
  outs = M.fromList [(0,S.singleton 1),(1,S.fromList [2,3]),
                     (2,S.empty),(3,S.empty)]
  ins = M.fromList [(0,S.empty),(1,S.singleton 0),(2,S.singleton 1),(3,S.singleton 1)]
  
whileTest = makeTest "while" "tests\\while.txt" expectedWhile

expectedWhile = CFG labels outs ins where
  labels = M.fromList [(0,Left (Assign "y" (Var "x"))),
                       (1,Left (Assign "z" (Number 1))),
                       (2,Right (RelOp Greater (Var "y") (Number 0))),
                       (3,Left (Assign "z" (BinOp Times (Var "z") (Var "y")))),
                       (4,Left (Assign "y" (BinOp Minus (Var "y") (Number 1)))),
                       (5,Left (Assign "y" (Number 0)))]
  outs = M.fromList [(0,S.singleton 1),(1,S.singleton 2),(2,S.fromList [3,5]),
                     (3,S.singleton 4),(4,S.singleton 2),(5,S.empty)]
  ins = M.fromList [(0,S.empty),(1,S.singleton 0),(2,S.fromList [1,4]),
                     (3,S.singleton 2),(4,S.singleton 3),(5,S.singleton 2)]

whileTest2 = makeTest "while" "tests\\while2.txt" expectedWhile2

expectedWhile2 = CFG labels outs ins where
  labels = M.fromList [(0,Right (RelOp Greater (Var "y") (Number 0))),
                       (1,Left (Assign "z" (BinOp Times (Var "z") (Var "y"))))]
  outs = M.fromList [(0,S.singleton 1),(1,S.singleton 0)]
  ins = M.fromList [(0,S.singleton 1),(1,S.singleton 0)]
                     
printd f = mapM_ print . M.toList . f
  
tests :: Test
tests = TestList [ifExampleTest,
                  whileTest,
                  whileTest2,
                  TestLabel "ast" $ (controlFlowGraph ast) ~?= cfg,
                  TestLabel "ast2" $ (controlFlowGraph ast2) ~?= cfg2]
    
doTestsPass :: IO Bool
doTestsPass = do
  counts <- runTestTT tests
  let errs = errors counts
      fails = failures counts
  return $ (errs == 0) && (fails == 0)

ast :: Statement
ast = Seq (Assign "x" (BinOp Plus (Number 5) (Number 3))) s where
  s = Seq (Assign "y" (Number 3)) s2
  s2 = Seq (While (RelOp Less (Var "y") (Var "x")) s4) s3
  s3 = Skip
  s4 = Assign "y" (BinOp Plus (Var "y") (Number 1))

ast2 :: Statement
ast2 = Seq (Assign "x" (BinOp Plus (Number 5) (Number 3))) s where
  s = Seq (Assign "y" (Number 3)) s2
  s2 = Seq (While (RelOp Less (Var "y") (Var "x")) s4) s3
  s3 = Skip
  s4 = If T (Assign "y" (BinOp Plus (Var "y") (Number 1))) s5
  s5 = Assign "y" (BinOp Plus (Var "y") (Number 2))
  
cfg :: ControlFlowGraph
cfg = CFG (decorate ast) out ins where
  ins = M.fromList [(0,S.empty),(1,set 0),(2,flist [1,3]),
         (3,set 2),(4,set 2)]
  out = M.fromList [(0,set 1),(1,set 2),(2,flist [3,4]),
          (3,set 2),(4,S.empty)]
  set = S.singleton
  flist = S.fromList

cfg2 :: ControlFlowGraph
cfg2 = CFG (decorate ast2) out ins where
  ins = M.fromList [(0,S.empty),(1,set 0),(2,flist [1,4,5]),
         (3,set 2),(4,set 3),(5,set 3),(6,set 2)]
  out = M.fromList [(0,set 1),(1,set 2),(2,flist [3,6]),
          (3,flist [4,5]),(4,set 2),(5,set 2),(6,S.empty)]
  set = S.singleton
  flist = S.fromList

\end{code}



