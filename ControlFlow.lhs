\begin{code}

module ControlFlow(controlFlowGraph, decorate) where

import AST

import qualified Data.Map as M
import qualified Data.Set as S

data ControlFlowGraph = CFG { labels :: M.Map Int Statement, 
                              edges  :: M.Map Int (S.Set Int)}

decorate :: Statement -> M.Map Int Statement
decorate = undefined

controlFlowGraph :: Statement -> ControlFlowGraph
controlFlowGraph = undefined

\end{code}