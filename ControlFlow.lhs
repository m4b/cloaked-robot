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
         (3,set 2),(4,set 3),(5,set 3)]
  out = M.fromList [(0,set 1),(1,set 2),(2,flist [3,6]),
          (3,flist [4,5]),(4,set 2),(5,set 2)]
  set = S.singleton
  flist = S.fromList
  

controlFlowGraph :: Statement -> ControlFlowGraph
controlFlowGraph g = init where
  init = CFG dec (snd $ computeSuccessors 0 1 dec g) M.empty
  dec = decorate g

computeSuccessors :: Int -> Int -> M.Map Int Block -> Statement -> (Int,M.Map Int (S.Set Int))
computeSuccessors i n dec (Assign s a) = (i,M.singleton i set) where
  set = case M.lookup n dec of
    Nothing -> S.empty
    _ -> S.singleton n

computeSuccessors i n dec Skip = (i,M.singleton i set) where
  set = case M.lookup n dec of
    Nothing -> S.empty
    _ -> S.singleton n
    
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

{--
{-
p,c,n
ins = p
outs = n
c,n,n+1
-}
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

{-
p,c,n
ins = p
outs = n
c,n,n+1
-}
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
  
{-2,3,2
p,c,n
ins = p
outs = n,n+1
s1 = c,n,n+2
s1 -> 
s2 = c,n+1,n+2
-}
computeGraph (p,c,n) (If _ s1 s2) = do
  g <- get
  let ins = inEdges g
      outs = outEdges g
      ins' = case M.lookup p outs of
        Nothing -> S.empty
        Just _ -> S.singleton p
      outs' = case M.lookup n (labels g) of
        Nothing -> S.empty
        Just _ -> S.fromList [n,n+1]
  put g{inEdges = M.insert c ins' ins,outEdges = M.insert c outs' outs}
  (p2,c2,n2) <- computeGraph (c,n,n+2) s1
  computeGraph (c,n+1,n+2) s2
  

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
  let t@(p',c',n') = (y,z+1,z+2)
  put g'{inEdges = M.insert c (S.insert n ins') insa,
        outEdges = M.insert c (S.insert c' outs') outa}
  return t
--}
\end{code}