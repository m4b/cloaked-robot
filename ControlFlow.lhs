\begin{code}
{-# LANGUAGE TupleSections #-}
module ControlFlow(Block, ControlFlowGraph(..), controlFlowGraph, decorate) where

import AST
import Control.Applicative
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S

type Block = Either Statement Boolean

data ControlFlowGraph = CFG { labels :: M.Map Int Block, 
                              outEdges  :: M.Map Int (S.Set Int),
                              inEdges :: M.Map Int (S.Set Int)}

decorate :: Statement -> M.Map Int Block
decorate = M.fromList . flip evalState 0 . decorate'

decorate' :: Statement -> State Int [(Int,Block)]
decorate' a@(Assign s arith) = (:[]) <$> (,Left a) <$> getIncrement
decorate' Skip = (:[]) <$> (,Left Skip) <$> getIncrement
decorate' (Seq s1 s2) = decorate2 s1 s2

decorate' con@(If bool s1 s2) = (:) <$> (,Right bool) <$> getIncrement
                                    <*> decorate2 s1 s2

decorate' whl@(While bool s) = undefined

getIncrement :: Num s => State s s
getIncrement = get >>= \i -> put (i+1) >> return i

decorate2 :: Statement -> Statement -> State Int [(Int,Block)]
decorate2 s1 s2 = (++) <$> (decorate' s1) <*> (decorate' s2)

controlFlowGraph :: Statement -> ControlFlowGraph
controlFlowGraph = undefined

\end{code}