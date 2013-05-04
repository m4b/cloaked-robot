\begin{code}

module ControlFlow(Block, ControlFlowGraph(..), controlFlowGraph, decorate) where

import AST

import qualified Data.Map as M
import qualified Data.Set as S

type Block = Either Statement Boolean

data ControlFlowGraph = CFG { labels :: M.Map Int Block, 
                              edges  :: M.Map Int (S.Set Int)}

decorate :: Statement -> M.Map Int Statement
decorate = undefined

controlFlowGraph :: Statement -> ControlFlowGraph
controlFlowGraph = undefined

\end{code}