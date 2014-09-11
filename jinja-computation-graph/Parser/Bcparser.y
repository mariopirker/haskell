{
module Parser.Bcparser where

import Char
import ComputationGraph.Program
import ComputationGraph.Instructions
import ComputationGraph.State

}

%name bcparse
%tokentype {Token}
%error { parseError }

{- tokens for the parser -}
%token
    class                   {TokClass}
    name                    {TokName}
    classbody               {TokClassBody}
    superType               {TokSuper}
    superTypeNone           {TokNone}
    tokLess                 {TokLess}
    tokGreater              {TokGreater}
    method                  {TokMethod}
    methods                 {TokMethods}
    fields                  {TokFields}
    parameter               {TokParameters}
    lbracket                {TokLBracket}
    rbracket                {TokRBracket}
    maxstack                {TokMaxStack}
    maxvars                 {TokMaxVars}
    bytecode                {TokByteCode}
    methodbody              {TokMethodBody}
    load                    {TokLoad}
    store                   {TokStore}
    push                    {TokPush}
    new                     {TokNew}
    getfield                {TokGetField}
    putfield                {TokPutField}
    checkcast               {TokCheckcast}
    invoke                  {TokInvoke}
    return                  {TokReturn}
    pop                     {TokPop}
    iadd                    {TokIadd}
    isub                    {TokIsub}
    goto                    {TokGoto}
    cmpeq                   {TokCmpeq}
    cmpneq                  {TokCmpNeq}
    cmpgeq                  {TokCmpGeq}
    not                     {TokNot}
    and                     {TokAnd}
    or                      {TokOr}
    iffalse                 {TokIffalse}
    id                      {TokVar $$}
    int                     {TokInt $$}
    colon                   {TokColon} 
    min                     {TokMinus}
    unit                    {TokUnit}
    null                    {TokNull}                

%nonassoc Low
%nonassoc Mid
%nonassoc Hi
%%

{- Productions -}

Prog
    : ClassDecl Prog     {$1 : $2}
    |                    {[]}

ClassDecl
    : class colon name colon id ClassBody       {ClassDecl ($5,$6)}


ClassBody
    : classbody colon superType colon tokLess superTypeNone tokGreater fields colon FDecls methods colon MDecls  {ClassBody "" $10 $13}
    | classbody colon superType colon id fields colon FDecls methods colon MDecls  {ClassBody $5 $8 $11}

FDecls
    : FDecl FDecls    {$1 : $2}
    |                 {[]}

FDecl
    : id id {FDecl ($1,$2)}

MDecls
    : MDecl MDecls    {$1 : $2}
    |                     {[]}


MDecl
    : method colon id id parameter colon ParMeths methodbody colon MBody {MDecl ($4,$7,$3,$10)}


ParMeths
    : ParMeth ParMeths {$1 : $2}
    |                        {[]}


ParMeth
    : id id {ParMeth ($1,$2)}
    | id lbracket rbracket id {ParMeth ($1,$4)}


MBody
    : maxstack colon int maxvars colon int bytecode colon ByteCode {MBody ($3,$6,$9)}



ByteCode
    : Instr ByteCode                 {$1 : $2}
    |                                {[]}

Instr
    : int colon load int {Load $4}
    | int colon store int {Store $4}
    | int colon push null {Push Null}
    | int colon push unit {Push Unit}
    | int colon push int {Push (IntVal $4)}
    | int colon new id {New $4}
    | int colon getfield id id {Getfield $4 $5}
    | int colon putfield id id {Putfield $4 $5}
    | int colon checkcast id {Checkcast $4}
    | int colon invoke id int {Invoke $4 $5}
    | int colon return {Return}
    | int colon pop {Pop}
    | int colon iadd {IAdd}
    | int colon isub {ISub}
    | int colon goto int {Goto $4}
    | int colon goto min int {Goto (negate $5)}
    | int colon cmpeq {CmpEq}
    | int colon cmpneq {CmpNeq}
    | int colon cmpgeq {CmpGeq}
    | int colon not {BNot}
    | int colon and {BAnd}
    | int colon or {BOr}
    | int colon iffalse int {IfFalse $4}

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
      = TokClass
      | TokName
      | TokClassBody
      | TokSuper
      | TokNone
      | TokLess
      | TokGreater
      | TokMethod
      | TokMethods
      | TokFields
      | TokParameters
      | TokLBracket 
      | TokRBracket
      | TokMaxStack
      | TokMaxVars
      | TokByteCode
      | TokMethodBody
      | TokLoad
      | TokStore
      | TokPush
      | TokNew
      | TokGetField
      | TokPutField
      | TokCheckcast
      | TokInvoke
      | TokReturn
      | TokPop
      | TokIadd
      | TokIsub
      | TokGoto
      | TokCmpeq
      | TokCmpGeq
      | TokNot
      | TokAnd
      | TokOr
      | TokIffalse
      | TokVar String
      | TokInt Int
      | TokColon
      | TokMinus
      | TokNull
      | TokUnit
      | TokCmpNeq
 deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | isAlpha c = lexerVar (c:cs)
    | isDigit c = lexNum (c:cs)
lexer (':':cs) = TokColon : lexer cs
lexer ('-':cs) = TokMinus : lexer cs
lexer ('<':cs) = TokLess : lexer cs
lexer ('>':cs) = TokGreater : lexer cs
lexer ('[':cs) = TokLBracket : lexer cs
lexer (']':cs) = TokRBracket : lexer cs

lexNum cs = TokInt (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexerVar cs =
    case span isAlpha cs of
        ("Class",rest) -> TokClass : lexer rest
        ("Name",rest) -> TokName : lexer rest
        ("Classbody",rest) -> TokClassBody : lexer rest
        ("Bytecode",rest) -> TokByteCode : lexer rest
        ("Superclass",rest) -> TokSuper : lexer rest
        ("None",rest) -> TokNone : lexer rest
        ("Fields",rest) -> TokFields : lexer rest
        ("Methods",rest) -> TokMethods : lexer rest
        ("Method",rest) -> TokMethod : lexer rest
        ("Parameters",rest) -> TokParameters : lexer rest
        ("Methodbody",rest) -> TokMethodBody : lexer rest
        ("MaxStack",rest) -> TokMaxStack : lexer rest
        ("MaxVars",rest) -> TokMaxVars : lexer rest
        ("Load",rest) -> TokLoad : lexer rest
        ("Store",rest) -> TokStore : lexer rest
        ("Push",rest) -> TokPush : lexer rest
        ("New",rest) -> TokNew : lexer rest
        ("Getfield",rest) -> TokGetField : lexer rest
        ("Putfield",rest) -> TokPutField : lexer rest
        ("Checkcast",rest) -> TokCheckcast : lexer rest
        ("Invoke",rest) -> TokInvoke : lexer rest
        ("Return",rest) -> TokReturn : lexer rest
        ("Pop",rest) -> TokPop : lexer rest
        ("Iadd",rest) -> TokIadd : lexer rest
        ("Isub",rest) -> TokIsub : lexer rest
        ("Goto",rest) -> TokGoto : lexer rest
        ("CmpEq",rest) -> TokCmpeq : lexer rest
        ("CmpGeq",rest) -> TokCmpGeq : lexer rest
        ("CmpNeq",rest) -> TokCmpNeq : lexer rest
        ("INot",rest) -> TokNot : lexer rest
        ("BAnd",rest) -> TokAnd : lexer rest
        ("BOr",rest) -> TokOr : lexer rest
        ("IfFalse",rest) -> TokIffalse : lexer rest
        ("null",rest) -> TokNull : lexer rest
        ("unit",rest) -> TokUnit : lexer rest
        (var,rest) -> TokVar var : lexer rest

}

