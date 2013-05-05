\begin{code}
{-# LANGUAGE TupleSections,
             ViewPatterns #-}
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
  
cfg :: ControlFlowGraph
cfg = CFG (decorate ast) out ins where
  ins = M.fromList [(0,S.empty),(1,set 0),(2,flist [1,3]),
         (3,set 2),(4,set 2)]
  out = M.fromList [(0,set 1),(1,set 2),(2,flist [3,4]),
          (3,set 2),(4,S.empty)]
  set = S.singleton
  flist = S.fromList  

controlFlowGraph :: Statement -> ControlFlowGraph
controlFlowGraph g = execState (computeGraph ((-1),0,1) g) $ init where
  init = CFG (decorate g) M.empty M.empty
  
isAssignOrSkip :: Statement -> Bool
isAssignOrSkip (Assign _ _) = True
isAssignOrSkip Skip = True
isAssignOrSkip _ = False

computeGraph :: (Int,Int,Int) -> Statement -> State ControlFlowGraph (Int,Int,Int)
computeGraph (p,c,n) (Assign _ _) = do
  g <- get
  let ins = inEdges g
      outs = outEdges g
      ins' = case M.lookup p outs of
        Nothing -> S.empty
        Just _ -> S.singleton p
      outs' = case M.lookup n (labels g) of
        Nothing -> S.empty
        Just _ -> S.singleton n
  put g{inEdges = M.insert c ins' ins,outEdges = M.insert c outs' outs}
  return (c,n,n+1)

computeGraph (p,c,n) Skip = do
  g <- get
  let ins = inEdges g
      outs = outEdges g
      ins' = case M.lookup p outs of
        Nothing -> S.empty
        Just _ -> S.singleton p
      outs' = case M.lookup n (labels g) of
        Nothing -> S.empty
        Just _ -> S.singleton n
  put g{inEdges = M.insert c ins' ins,outEdges = M.insert c outs' outs}
  return (c,n,n+1)
  
computeGraph pos (Seq s1 s2) = do
  pos' <- computeGraph pos s1
  computeGraph pos' s2
  
{--computeGraph (p,c,n) (If _ s1 s2) = do
  g <- get
  let ins = inEdges g
      outs = outEdges g
      ins' = case M.lookup p outs of
        Nothing -> S.empty
        Just _ -> S.singleton p
      outs' = case M.lookup n (labels g) of
        Nothing -> S.empty
        Just _ -> S.singleton n
  put g{inEdges = M.insert c ins' ins,outEdges = M.insert c outs' outs}--}

computeGraph (p,c,n) (While _ s) = do
  g <- get
  let ins = inEdges g
      outs = outEdges g
      ins' = case M.lookup p outs of
        Nothing -> S.empty
        Just _ -> S.singleton p
      outs' = case M.lookup n (labels g) of
        Nothing -> S.empty
        Just _ -> S.singleton n
  put g{inEdges = M.insert c ins' ins,outEdges = M.insert c outs' outs}
  (x,y,z) <- computeGraph (c,n,c) s
  g' <- get
  let insa = inEdges g'
      outa = outEdges g'
  {--let insa = inEdges g'
      outa = outEdges g'
      ins'' = insa M.! p 
      outs'' = outa M.! n--}
  let t@(p',c',n') = (y,z+1,z+2)
  put g'{inEdges = M.insert c (S.insert n ins') insa,
        outEdges = M.insert c (S.insert c' outs') outa}
  return t

\end{code}