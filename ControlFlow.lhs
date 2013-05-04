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

decorate' con@(If bool s1 s2) = (:) <$> (,Right bool) 
                                    <$> getIncrement
                                    <*> decorate2 s1 s2

decorate' whl@(While bool s) = (:) <$> (,Right bool) <$> getIncrement <*> decorate' s

getIncrement :: Num s => State s s
getIncrement = get >>= \i -> put (i+1) >> return i

decorate2 :: Statement -> Statement -> State Int [(Int,Block)]
decorate2 s1 s2 = (++) <$> (decorate' s1) <*> (decorate' s2)

displayLabeledGraph :: M.Map Int Block -> IO ()
displayLabeledGraph = mapM_ (putStrLn . showBlock) . M.toList where
  showBlock (i,(Left (Assign s a))) = "[" ++ (s ++ " := " ++ (show a)) ++ "]" ++ (show i)
  showBlock (i,(Left s)) = "[" ++ (show s) ++ "]" ++ (show i)
  showBlock (i,(Right b)) = "[" ++ (show b) ++ "]" ++ (show i)

ast :: Statement
ast = Seq (Assign "x" (BinOp Plus (Number 5) (Number 3))) s where
  s = Seq (Assign "y" (Number 3)) s2
  s2 = Seq (While (RelOp Less (Var "y") (Var "x")) s4) s3
  s3 = Skip
  s4 = If (T) (Assign "y" (BinOp Plus (Var "y") (Number 1))) Skip

controlFlowGraph :: Statement -> ControlFlowGraph
controlFlowGraph = undefined

\end{code}