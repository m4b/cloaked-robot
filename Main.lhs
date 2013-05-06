\documentclass[11pt]{article}

%include polycode.fmt

%format `union` = "\cup"
%format alpha = "\alpha"
%format gamma = "\gamma"
%format delta = "\delta"
%format capGamma = "\Gamma"
%format tau = "\tau"
%format tau1 = "\tau_{1}"
%format tau2 = "\tau_{2}"
%format tau11 = "\tau_{11}"
%format tau12 = "\tau_{12}"
%format t12 = "t_{12}"
%format t1 = "t_{1}"
%format t1' = "t_{1}^{\prime}"
%format t2 = "t_{2}"
%format t2' = "t_{2}^{\prime}"
%format t3 = "t_{3}"
%format nv1 = "nv_{1}"

\author{\textsc{S. Patel, J. Collard, M. Barney}}
\title{Final Project: Data Flow Analysis}
\date{\today}

\begin{document}

\maketitle

\section{Introduction}

This report contains our implementation of a scanner and parser for a basic programming language, and our data flow graph generation tools.

It is divided up into several sections, roughly corresponding to the problems given in the specification, each a Haskell module. 


%include AST.lhs
%include Input.lhs
%include ControlFlow.lhs
%include ReachingDefinition.lhs

\section{Main module}

The main module puts everything together.


\begin{code}

module Main where

import System.Environment
import AST
import Input
import ControlFlow
import ReachingDefinition

main = do 
     [file] <- getArgs
     contents <- readFile file
     case sparse contents of
          Right ast -> putStrLn . dotPrinter $ ast
          Left err -> print err

\end{code}

\end{document}
