\documentclass[11pt]{article}
\usepackage{graphicx}
\usepackage{amsmath}
\usepackage{latexsym}
\usepackage{fontspec}

% \setmainfont[Ligatures=TeX]{DejaVu Sans Mono}
\setmainfont[Ligatures=TeX]{Linux Libertine O}
\newfontfamily\mathfont{Asana Math}
\newfontfamily\fallbackfont{DejaVu Sans Mono}
\usepackage{newunicodechar}

\newunicodechar{∖}{\mathfont ∖}
\newunicodechar{●}{\mathfont ●}
\newunicodechar{○}{\mathfont ○}
\newunicodechar{∪}{\mathfont ∪}


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

\section{Introduction}\label{intro}

This report contains our implementation of a scanner and parser for a basic programming language, and our data flow graph generation tools.  Also included is a printer for the AST of a program that outputs {\tt .gv} files to be used with a tool like dot to create graph-based images.

For example, the program:

\begin{center}
\begin{verbatim}
x:= 2 * 200;
if x <= 400 then skip else y:=10 fi
\end{verbatim}
\end{center}

yields the following AST:

\begin{center}
\includegraphics[width=1.0\textwidth]{tests/ifexample.png}
\end{center}

Our implementation is divided up into several sections, roughly corresponding to the problems given in the specification, and each its own Haskell module.

We spent approximately 40 man hours on the project.  This was slightly shorter than the previous assignments, but we feel that over the course of the semester, our ability to code together as a team has substantially improved.

In addition, since we were coding in Haskell, tasks like parsing, printing, and the even some of the algorithms have become familiar to us, and thus allowed us a faster development time.


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
     let result = sparse contents
     case result of
          Right ast -> do
                writeFile "ast.gv" (dotPrinter ast)
                print $ controlFlowGraph ast
          Left err -> print err
     

\end{code}

\section{Example: while.txt Program}

Given the following simple program:

\begin{center}
\begin{verbatim}
y := x;
z := 1;
while y > 0 do
      z := z * y;
      y := y - 1
od;
y := 0
\end{verbatim}
\end{center}

After scanning and parsing, our dot printer gives its abstract syntax as:

\begin{center}
\includegraphics[width=1.0\textwidth]{tests/dotwhile.png}
\end{center}

\end{document}
