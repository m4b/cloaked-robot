\section{Scanner and Parser}

We decided to use the Parsec library for parsing files containing the simple imperative language.

This gave us a great amount of flexibility for parsing input files.  MENTION UNICODE

\begin{code}

module Input(sparse) where

import AST 

import Text.ParserCombinators.Parsec

type Program = [Statement]

statement :: GenParser Char st Statement
statement = do
    s1 <- statement'
    seq <- optionMaybe (char ';' >> spaces >> statement)
    case seq of
          Nothing -> return s1
          Just s2 -> return $ Seq s1 s2

-- assignment must be last to preserve keywords
statement' = skip <|> ifstatement <|> whilestatement <|> assignment

assignment = do
           identifier <- many1 letter
           spaces
           string ":="
           spaces
           expression <- arithmetic
           return $ Assign identifier expression

expr   = term   `chainl1` addop
term   = factor `chainl1` mulop

varParser :: GenParser Char st Arith
varParser = do
  v <- many1 letter
  spaces
  return (Var v)

numParser :: GenParser Char st Arith
numParser = do
  n <- many1 digit
  spaces
  return (Number ((read n) :: Int))

factor = 
    varParser <|> numParser <|> 
        do 
      char '('
      spaces
      n <- expr
      spaces
      char ')'
      spaces
      return n

addop  = do {char '+'; spaces; return (BinOp Plus)} 
       <|> do {char '-'; spaces ;return (BinOp Minus)}
mulop  = do {char '*'; spaces; return (BinOp Times)}

arithmetic = do
  e <- expr
  return e
           
optionalParens p = between (char '(') (char ')') p <|> p
               
skip = do
     string "skip"
     spaces
     return Skip

ifstatement = do
            string "if"
            many1 space
            b <- boolean
            string "then"
            many1 space
            s1 <- statement
            string "else"
            many1 space
            s2 <- statement
            string "fi"
            return $ If b s1 s2

notParser = do
        string "not" <|> string "¬" <|> string "~"
        spaces
        b <- boolean
        return $ Not b

andParser = do
        string "/\\" <|> string "∧"
        spaces
        b2 <- boolean
        return $ (\ x -> BoolOp And x b2)

orParser = do
        string "\\/" <|> string "∨"
        spaces
        b2 <- boolean
        return $ (\ x -> BoolOp Or x b2)

relation = 
         do {string ">"; return $ RelOp Greater} <|>
         do {string "<"; return $ RelOp Less} <|>
         do {string "=="; return $ RelOp Equal} <|>
         do {string ">=" <|> string "≥"; return $ RelOp Geq} <|>
         do {string "<=" <|> string "≤"; return $ RelOp Leq}

relopParser = do
        a1 <- arithmetic
        spaces
        relop <- relation
        spaces
        a2 <- arithmetic
        return $ relop a1 a2

tfParser =
        do {string "true"; spaces; return T} <|> 
        do {string "false"; spaces; return F}

boolean = do
  b <- notParser <|> tfParser <|> relopParser
  bexpr <- optionMaybe $ andParser <|> orParser
  case bexpr of
    Nothing -> return b
    Just bFun -> return $ bFun b

whilestatement = do
  string "while"
  many1 space
  b <- boolean
  string "do"
  many1 space
  s <- statement
  string "od"
  return $ While b s
                 

sparse = parse statement "(syntax error)"


\end{code}