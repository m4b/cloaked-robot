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
     BoolOp BOP Boolean Boolean | 
     RelOp REL Arith Arith deriving (Eq, Show)

data Statement = 
     Assign String Arith | 
     Skip | 
     Seq Statement Statement | 
     If Boolean Statement Statement | 
     While Boolean Statement deriving (Eq, Show)

\end{code}

As can be seen, the abstract syntax, thanks to Haskell's recursive data types, almost exactly mirrors the Backus-Naur form given in the assignment.

In addition, we wrote a pretty printer for ASTs, as follows:

\begin{code}

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
pettyShowBool (Not b) = "not" ++ pettyShowBool b
pettyShowBool (BoolOp bop b1 b2) = pettyShowBool b1 ++ " "
                                ++ pettyShowBOP bop ++ " "
                                ++ pettyShowBool b2 
pettyShowBool (RelOp rel a1 a2) = pettyShowArith a1 ++ " "
                               ++ pettyShowREL rel ++ " "
                               ++ pettyShowArith a2     

pettyShowStatement :: Statement -> String
pettyShowStatement (Assign s a) = s ++ " := " ++ pettyShowArith a
pettyShowStatement Skip = "Skip"
pettyShowStatement (Seq s1 s2) = pettyShowStatement s1 ++";" ++ ['\n']
                              ++ pettyShowStatement s2 ++ ['\n']
pettyShowStatement (If b s1 s2) = "if " ++ pettyShowBool b ++ ['\n']
                               ++ "then " ++ pettyShowStatement s1 ++ ['\n']
                               ++ "else " ++ pettyShowStatement s2
pettyShowStatement (While b s) = "while " ++ pettyShowBool b ++ ['\n']
                              ++ pettyShowStatement s                               
                               

\end{code}

Lastly, We also wrote a printer that outputs dot syntax for drawing pretty graphs for the abstract syntax of a program.

\begin{code}

dotPrinter' :: (Num t, Show t) => Statement -> t -> ([Char], t)
dotPrinter' (While b s1) counter = 
   let whilelabel = "s_" ++ (show counter)
       (boolean,counter') = dotPrinterBool b (counter+1)
       (statement1,counter'') = dotPrinter' s1 (counter')
       string = ( whilelabel ++ " [label=\"while\"];\n" ++
                whilelabel ++ " -> " ++ "b_" ++ (show (counter+1))
                 ++ ";\n" ++  
                whilelabel ++ " -> " ++ "s_" ++ (show (counter'))
                 ++ ";\n" ++
                boolean ++ statement1) in
   (string,(counter''))
dotPrinter' (If b s1 s2) counter = 
   let iflabel = "s_" ++ (show counter)
       (boolean,counter') = dotPrinterBool b (counter+1)
       (statement1,counter'') = dotPrinter' s1 (counter')
       (statement2,counter''') = dotPrinter' s2 (counter'')
       string = ( iflabel ++ " [label=\"if\"];\n" ++
                  iflabel ++ " -> " ++ "b_" ++ (show (counter+1)) ++ ";\n" ++  
                  iflabel ++ " -> " ++ "s_" ++ (show (counter')) ++ ";\n" ++
                  iflabel ++ " -> " ++ "s_" ++ (show (counter'')) ++ ";\n" ++
                  boolean ++ statement1 ++ statement2 ) in
   (string,(counter'''))
dotPrinter' (Seq s1 s2) counter = 
   let seq = "s_" ++ (show counter)
       (statement1,counter') = dotPrinter' s1 (counter+1)
       (statement2,counter'') = dotPrinter' s2 (counter')
       string = ( seq ++ " [label=\";\"];\n" ++
                  seq ++ " -> " ++ "s_" ++ (show (counter+1)) ++ ";\n" ++  
                  seq ++ " -> " ++ "s_" ++ (show (counter')) ++ ";\n" ++
                  statement1 ++ statement2 ) in
   (string,(counter''))
dotPrinter' (Skip) counter = 
   let s1 = "s_" ++ (show counter)
       string = ( s1 ++ " [label=\"skip\"];\n" ) in
   (string,(counter+1))
dotPrinter' (Assign name a) counter =          
   let s1 = "s_" ++ (show counter)
       s2 = "s_" ++ (show (counter+1))
       s3 = "s_" ++ (show (counter+2))
       (arith,counter') = dotPrinterArith a (counter+3)
       string = ( s1 ++ " [label=\":=\"];\n" ++ 
                  s2 ++ " [label=\"variable\"];\n" ++
                  s3 ++ " [label=\"" ++ name ++ "\"];\n" ++
                  s1 ++ " -> " ++ s2 ++ " -> " ++ s3 ++ ";\n" ++  
                  s1 ++ " -> " ++ "a_" ++ (show (counter+3)) ++ ";\n" ++
                  arith) in
    (string,counter')

dotPrinterArith :: (Num t, Show t) => Arith -> t -> ([Char], t)
dotPrinterArith (Var s) counter = 
   let v1 = "a_" ++ (show counter)
       v2 = "a_" ++ (show (counter +1))
       string = ( v1 ++ " [label=\"variable\"];\n" ++
                  v2 ++ " [label=\"" ++ s ++ "\"];\n" ++
                  v1 ++ " -> " ++ v2 ++ ";\n"
                 ) in
   (string,(counter+2))
dotPrinterArith (Number i) counter =
   let n1 = "a_" ++ (show counter)
       n2 = "a_" ++ (show (counter +1))
       string = ( n1 ++ " [label=\"number\"];\n" ++
                  n2 ++ " [label=\"" ++ (show i) ++ "\"];\n" ++
                  n1 ++ " -> " ++ n2 ++ ";\n"
                 ) in
   (string,(counter+2))
dotPrinterArith (BinOp aop a1 a2) counter =
   let op = "a_" ++ (show counter)
       (s1,counter') = dotPrinterArith a1 (counter+1)
       (s2,counter'') = dotPrinterArith a2 counter'
       string = ( op ++ " [label=\"" ++ (dotAOP aop) ++ "\"];\n" ++
                  op ++ " -> " ++ "a_" ++ (show (counter+1)) ++ ";\n" ++
                  op ++ " -> " ++ "a_" ++ (show counter') ++ ";\n" ++
                  s1 ++ s2
                 ) in
   (string,(counter''))

dotPrinterBool :: (Num t, Show t) => Boolean -> t -> ([Char], t)
dotPrinterBool (T) counter = 
   let b1 = "b_" ++ (show counter)
       string = ( b1 ++ " [label=\"⊤\"];\n" ) in
   (string,(counter+1))
dotPrinterBool (F) counter = 
   let b1 = "b_" ++ (show counter)
       string = ( b1 ++ " [label=\"⊥\"];\n" ) in
   (string,(counter+1))
dotPrinterBool (Not b) counter =
   let b1 = "b_" ++ (show counter)
       (s1,counter') = dotPrinterBool b (counter+1)
       string = ( b1 ++ " [label=\"not\"];\n" ++
                  b1 ++ " -> " ++ (show (counter +1)) ++ ";\n" ++
                  s1
                 ) in
   (string,counter')
dotPrinterBool (BoolOp bop b1 b2) counter =
   let op = "b_" ++ (show counter)
       (s1,counter') = dotPrinterBool b1 (counter+1)
       (s2,counter'') = dotPrinterBool b2 counter'
       string = ( op ++ " [label=\"" ++ (dotBOP bop) ++ "\"];\n" ++
                  op ++ " -> " ++ "b_" ++ (show (counter+1)) ++ ";\n" ++
                  op ++ " -> " ++ "b_" ++ (show counter') ++ ";\n" ++
                  s1 ++ s2
                 ) in
   (string,(counter''))
dotPrinterBool (RelOp rel a1 a2) counter =
   let op = "b_" ++ (show counter)
       (s1,counter') = dotPrinterArith a1 (counter+1)
       (s2,counter'') = dotPrinterArith a2 counter'
       string = ( op ++ " [label=\"" ++ (dotREL rel) ++ "\"];\n" ++
                  op ++ " -> " ++ "a_" ++ (show (counter+1)) ++ ";\n" ++
                  op ++ " -> " ++ "a_" ++ (show counter') ++ ";\n" ++
                  s1 ++ s2
                 ) in
   (string,(counter''))

dotAOP :: AOP -> [Char]
dotAOP (Plus)   = "+"
dotAOP (Minus)  = "-"
dotAOP (Times)  = "*"

dotBOP (And)    = "∧"
dotBOP (Or)     = "∨"

dotREL (Equal)   = "=="
dotREL (Less)    = "<"
dotREL (Leq)     = "≤"
dotREL (Greater) = ">"
dotREL (Geq)     = "≥"

dotPrinter :: Statement -> [Char]
dotPrinter x = 
   ("digraph graphname{\n" ++ (fst (dotPrinter' x 0)) ++ "}")

main' = do
 putStrLn $ dotPrinter (Assign "x" (BinOp Plus ((BinOp Times (Var "y") (Number 3))) (Number 3)))
 putStrLn $ fst (dotPrinterBool (BoolOp And (RelOp Less (Number 3) (Number 4)) F) 0)
      


\end{code}

