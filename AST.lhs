\section{Abstract Syntax}

In this module we define the abstract syntax (AST) for statements written in a simple imperative language.

\begin{code}

module AST where

data AOP = 
     Plus | 
     Times | 
     Minus deriving (Eq, Show)

data BOP = 
     And | 
     Or deriving (Eq, Show)

data REL = 
     Equal |
     Less | 
     Leq | 
     Greater | 
     Geq deriving (Eq, Show)

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

\end{code}

