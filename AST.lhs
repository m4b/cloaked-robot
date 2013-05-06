\section{Abstract Syntax}

In this module we define the abstract syntax (AST) for statements written in a simple imperative language.

Its Backus-Naur form is as follows:

\begin{equation*}
\begin{aligned}
\text{Arithmetic expression } a\ &::=\ x\ \vert \ n\ \vert \ a_1\ o_a\ a_2\\
\text{Boolean expression } b\ &::=\ true \vert \ false\ \vert \ not\ b\ \vert \ b_1\ o_b\ b_2\ \vert \ a_1 \ o_r \ a_2\\
\text{Statement } S\ &::=\ x\ :=\ a\ \vert \ skip\ \vert \ S_1;S_2\ \vert \ \\
                &if\ b\ then\ S_1\ else \ S_2\ fi\ \vert \ while\ b \ do\ S\ od\ 
\end{aligned}
\end{equation*}

The Haskell code will more or less precisely mirror the mathematical definition just given.

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

In addition, for bug testing, and for nice output, we wrote a pretty printer for ASTs, as follows:

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

Lastly, as previously mentioned, we also wrote a printer that outputs dot syntax for drawing pretty graphs for the abstract syntax of a program.  For example, the output for the example program in \S\ref{intro} is as follows:

\begin{verbatim}
digraph graphname{
s_0 [label=";"];
s_0 -> s_1;
s_0 -> s_9;
s_1 [label=":="];
s_2 [label="variable"];
s_3 [label="x"];
s_1 -> s_2 -> s_3;
s_1 -> a_4;
a_4 [label="*"];
a_4 -> a_5;
a_4 -> a_7;
a_5 [label="number"];
a_6 [label="2"];
a_5 -> a_6;
a_7 [label="number"];
a_8 [label="200"];
a_7 -> a_8;
s_9 [label="if"];
s_9 -> b_10;
s_9 -> s_15;
s_9 -> s_16;
b_10 [label="<="];
b_10 -> a_11;
b_10 -> a_13;
a_11 [label="variable"];
a_12 [label="x"];
a_11 -> a_12;
a_13 [label="number"];
a_14 [label="400"];
a_13 -> a_14;
s_15 [label="skip"];
s_16 [label=":="];
s_17 [label="variable"];
s_18 [label="y"];
s_16 -> s_17 -> s_18;
s_16 -> a_19;
a_19 [label="number"];
a_20 [label="10"];
a_19 -> a_20;
}
\end{verbatim}


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
                  iflabel ++ " -> " ++ "b_" ++ (show (counter+1))
                   ++ ";\n" ++  
                  iflabel ++ " -> " ++ "s_" ++ (show (counter'))
                   ++ ";\n" ++
                  iflabel ++ " -> " ++ "s_" ++ (show (counter''))
                   ++ ";\n" ++
                  boolean ++ statement1 ++ statement2 ) in
   (string,(counter'''))
dotPrinter' (Seq s1 s2) counter = 
   let seq = "s_" ++ (show counter)
       (statement1,counter') = dotPrinter' s1 (counter+1)
       (statement2,counter'') = dotPrinter' s2 (counter')
       string = ( seq ++ " [label=\";\"];\n" ++
                  seq ++ " -> " ++ "s_" ++ (show (counter+1))
                   ++ ";\n" ++
                  seq ++ " -> " ++ "s_" ++ (show (counter'))
                   ++ ";\n" ++
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
                  s1 ++ " -> " ++ "a_" ++ (show (counter+3))
                   ++ ";\n" ++ arith) in
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
       string = ( op ++ " [label=\"" ++ (dotAOP aop)
                  ++ "\"];\n" ++
                  op ++ " -> " ++ "a_" ++ (show (counter+1))
                   ++ ";\n" ++
                  op ++ " -> " ++ "a_" ++ (show counter')
                   ++ ";\n" ++
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

\end{code}

