{
module Eqparser where

import Data.Char 
import Part2

}

%name eqparse
%tokentype {Token}
%error { parseError }

{- tokens for the parser -}
%token
    roo                     {TokRoot}
    neg                     {TokNeg}
    equ                     {TokEqu}
    add                     {TokAdd}
    min                     {TokSub}
    mul                     {TokMul}
    pow                     {TokPow}
    lbracket                {TokLBracket}
    rbracket                {TokRBracket}
    nat                     {TokInt $$}
    var                     {TokVar $$}

%nonassoc equ lbracket rbracket
%left pow neg roo
%left add min
%left mul 
%%

{- Productions -}

Eq 
    : LPart equ RPart {Eq $1 $3}

LPart 
    : ArExpr {$1}

RPart 
    : ArExpr {$1}

ArExpr
    : ArExpr pow nat                       {Pow $1 (Nat $3)}
    | ArExpr add ArExpr                    {Add $1 $3}
    | ArExpr min ArExpr                    {Sub $1 $3}
    | ArExpr mul ArExpr                    {Mul $1 $3}
    | lbracket ArExpr rbracket             {Bra $2}
    | lbracket ArExpr rbracket pow nat     {Bra (Pow $2 (Nat $5))}
    | var                                  {Var 1 $1}
    | nat var                              {Var $1 $2}
    | roo nat rbracket var                 {VarR (Root $2) $4}
    | nat                                  {Nat $1}
    | neg ArExpr                           {Neg $2}

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
      = TokInt Integer
      | TokVar String 
      | TokEqu
      | TokAdd
      | TokSub
      | TokMul
      | TokPow
      | TokLBracket
      | TokRBracket
      | TokNeg
      | TokRoot
 deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | isAlpha c = lexerVar (c:cs)
    | isDigit c = lexNum (c:cs)
lexer ('=':cs) = TokEqu : lexer cs
lexer ('!':cs) = TokNeg : lexer cs
lexer ('-':cs) = TokSub : lexer cs 
lexer ('+':cs) = TokAdd : lexer cs
lexer ('*':cs) = TokMul : lexer cs
lexer ('(':cs) = TokLBracket : lexer cs
lexer (')':cs) = TokRBracket : lexer cs
lexer ('^':cs) = TokPow : lexer cs
lexer ('~':cs) = TokRoot : lexer cs

lexNum cs = TokInt (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexerVar cs = 
   case span isAlpha cs of 
      (var,rest) -> TokVar var : lexer rest
}

