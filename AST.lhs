\section{Abstract Syntax}

In this module we define the abstract syntax (AST) for statements written in a simple imperative language.

\begin{code}

module AST where

import Data.Maybe

data AOP = 
     Plus | 
     Times | 
     Minus deriving (Eq, Show, Enum)

data BOP = 
     And | 
     Or deriving (Eq, Show, Enum)

data REL = 
     Equal |
     Less | 
     Leq | 
     Greater | 
     Geq deriving (Eq, Show, Enum)

data Arith = 
     Var String | 
     Number Int | 
     BinOp AOP Arith Arith deriving (Eq, Show)

data Boolean = 
     T | 
     F | 
     Not Boolean | 
     BoolOp BOP Arith Arith | 
     RelOp REL Arith Arith deriving (Eq, Show)

data Statement = 
     Assign String Arith | 
     Skip | 
     Seq Statement Statement | 
     If Boolean Statement Statement | 
     While Boolean Statement deriving (Eq, Show)

pettyShowAOP :: AOP -> String
pettyShowAOP aop = fromJust . lookup aop $ ops where
  ops = zip [Plus .. Minus] ["+","*","-"]

pettyShowBOP :: BOP -> String
pettyShowBOP And = "/\\"
pettyShowBOP Or = "\\/"

pettyShowREL :: REL -> String
pettyShowREL rel = fromJust . lookup rel $ rels where
  rels = zip [Equal .. Geq] ["==","<","<=",">",">="]

pettyShowArith :: Arith -> String
pettyShowArith (Var s) = s
pettyShowArith (Number i) = show i
pettyShowArith (BinOp aop a1 a2) = pettyShowArith a1 ++ " "
                                ++ pettyShowAOP aop ++ " "
                                ++ pettyShowArith a2  

pettyShowBool :: Boolean -> String
pettyShowBool T = "true"
pettyShowBool F = "false"
pettyShowBool (Not b) = "~" ++ pettyShowBool b
pettyShowBool (BoolOp bop a1 a2) = pettyShowArith a1 ++ " "
                                ++ pettyShowBOP bop ++ " "
                                ++ pettyShowArith a2 
pettyShowBool (RelOp rel a1 a2) = pettyShowArith a1 ++ " "
                               ++ pettyShowREL rel ++ " "
                               ++ pettyShowArith a2     

pettyShowStatement :: Statement -> String
pettyShowStatement (Assign s a) = s ++ " := " ++ pettyShowArith a ++ ";"
pettyShowStatement Skip = "Skip;"
pettyShowStatement (Seq s1 s2) = pettyShowStatement s1 ++ ['\n']
                              ++ pettyShowStatement s2 ++ ['\n']
pettyShowStatement (If b s1 s2) = "if " ++ pettyShowBool b ++ ['\n']
                               ++ "then " ++ pettyShowStatement s1 ++ ['\n']
                               ++ "else " ++ pettyShowStatement s2
pettyShowStatement (While b s) = "while " ++ pettyShowBool b ++ ['\n']
                              ++ pettyShowStatement s                               
                               

\end{code}

