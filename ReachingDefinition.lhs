\begin{code}
{-# LANGUAGE ViewPatterns #-}
module ReachingDefinition where

import AST
import ControlFlow

import qualified Data.Map as M
import qualified Data.Set as S

type ReachingDefinition = S.Set (String, Maybe Int)

determineVars :: ControlFlowGraph -> S.Set String
determineVars (labels -> M.elems -> cfg) = S.unions . map getVars $ cfg

getVars :: Statement -> S.Set String
getVars (Assign label arith) = S.singleton label `S.union` (getArithVars arith)
getVars _ = S.empty

getArithVars :: Arith -> S.Set String
getArithVars (Var label) = S.singleton label
getArithVars (BinOp _ a0 a1) = S.union (getArithVars a0) (getArithVars a1)
getArithVars _ = S.empty

simpleGraph :: ControlFlowGraph
simpleGraph = CFG labels edges where
  labels = M.fromList [(0, Assign "x" (Var "y")), 
                       (1, Assign "y" (Number 0)),
                       (2, Assign "z" (BinOp Plus (Var "x") (Var "y"))),
                       (3, While (RelOp Geq (Var "b") (Var "c")) (Assign "a" (Var "b")))]
  edges = M.fromList [(0, S.singleton 1)]

\end{code}