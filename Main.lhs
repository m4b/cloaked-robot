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

Our implementation is divided up into several sections, roughly corresponding to the problems given in the specification, and each its own Haskell module.  Each group member was more or less given a module to work on, but there was coding across modules happening on occasion.

We spent approximately 50 man hours on the project.  We feel that over the course of the semester, our ability to code together as a team substantially improved, and we were able to work together in an efficient and robust manner.

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
                let cfg = controlFlowGraph ast
                print cfg
                putStrLn . formatReachingDefinitions . 
                         reachingDefinitions $ cfg
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

The control flow graph for the above is:

ENTER TEXT BITCHES

And finally, the entry and exit points for reaching definitions is:

\begin{equation*}
\begin{aligned}
RD○(0) &= \{(x, ?),(y, ?),(z, ?)\}\\
RD○(1) &= \{(x, ?),(y, 0),(z, ?)\}\\
RD○(2) &= \{(x, ?),(y, 0),(z, 1),(z, 3)\}\\
RD○(3) &= \{(x, ?),(y, 0),(z, 1),(z, 3)\}\\
RD○(4) &= \{\}\\
RD○(5) &= \{(x, ?),(y, 0),(z, 1),(z, 3)\}\\
RD●(0) &= \{(x, ?),(y, 0),(z, ?)\}\\
RD●(1) &= \{(x, ?),(y, 0),(z, 1)\}\\
RD●(2) &= \{(x, ?),(y, 0),(z, 1),(z, 3)\}\\
RD●(3) &= \{(x, ?),(y, 0),(z, 3)\}\\
RD●(4) &= \{\}\\
RD●(5) &= \{(x, ?),(y, 5),(z, 1),(z, 3)\}
\end{aligned}
\end{equation*}


\end{document}
