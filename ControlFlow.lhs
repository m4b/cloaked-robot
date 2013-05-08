\section{Control Flow Diagrams}

In this section we compute the control flow graph for a given AST.

\begin{code}
{-# LANGUAGE TupleSections #-}
module ControlFlow where

import AST
import Control.Applicative
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S

type Block = Either Statement Boolean

data ControlFlowGraph = CFG { labels :: M.Map Int Block, 
                              outEdges  :: M.Map Int (S.Set Int),
                              inEdges :: M.Map Int (S.Set Int)} deriving (Show, Eq)

decorate :: Statement -> M.Map Int Block
decorate = M.fromList . flip evalState 0 . decorate'

decorate' :: Statement -> State Int [(Int,Block)]
decorate' a@(Assign s arith) = (:[]) <$> (,Left a) <$> getIncrement

decorate' Skip = (:[]) <$> (,Left Skip) <$> getIncrement

decorate' (Seq s1 s2) = decorate2 s1 s2

decorate' con@(If bool s1 s2) = (:) <$> (,Right bool) 
                                    <$> getIncrement
                                    <*> decorate2 s1 s2

decorate' whl@(While bool s) = (:) <$> (,Right bool) <$> getIncrement <*> decorate' s

decorate2 :: Statement -> Statement -> State Int [(Int,Block)]
decorate2 s1 s2 = (++) <$> (decorate' s1) <*> (decorate' s2)

getIncrement :: Num s => State s s
getIncrement = get >>= \i -> put (i+1) >> return i

getIncrement2 :: Num s => State (s,a) (s,a)
getIncrement2 = get >>= \(i,s) -> put ((i+1),s) >> return (i,s)

displayLabeledGraph :: M.Map Int Block -> IO ()
displayLabeledGraph = mapM_ (putStrLn . showBlock) . M.toList where
  showBlock (i,Left s) = "[" ++ (pettyShowStatement s) ++ "]" ++ (show i)
  showBlock (i,Right b) = "[" ++ (pettyShowBool b) ++ "]" ++ (show i)

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
  

controlFlowGraph :: Statement -> ControlFlowGraph
controlFlowGraph g = init where
  init = CFG dec outs ins
  dec = decorate g
  outs = snd $ computeSuccessors 0 1 dec g
  ins = computePredecessors outs

computeSuccessors :: Int 
                  -> Int 
                  -> M.Map Int Block 
                  -> Statement 
                  -> (Int,M.Map Int (S.Set Int))   
computeSuccessors i n dec (Seq s1 s2) = (n2,M.union m1 m2) where
  (n1,m1) = computeSuccessors i n dec s1
  (n2,m2) = computeSuccessors (n1+1) (n1+2) dec s2
  
computeSuccessors i n dec (If _ s1 s2) = (n2,M.unions [m,m1,m2]) where
  m = M.singleton i (S.fromList [i+1,i+2])
  (n1,m1) = computeSuccessors (i+1) n dec s1
  (n2,m2) = computeSuccessors (i+2) n dec s2
  
computeSuccessors i n dec (While _ s) = (n1,M.union m m1)where
  m = M.singleton i set
  set = case M.lookup n1 dec of
    Nothing -> S.singleton n  
    _ -> S.fromList [i+1,n1+1]
  (n1,m1) = computeSuccessors n i dec s
  
computeSuccessors i n dec _ = (i,M.singleton i set) where
  set = case M.lookup n dec of
    Nothing -> S.empty
    _ -> S.singleton n
    
computePredecessors :: M.Map Int (S.Set Int)
                    -> M.Map Int (S.Set Int)
computePredecessors outs = M.fromList . map go . M.keys $ outs where
  go i = (i,labelsWith i)
  labelsWith i = S.fromList [j | (j,set) <- M.toList outs, S.member i set]

\end{code}

The dot printer for control flow graphs is as follows:

\begin{code}



\end{code}