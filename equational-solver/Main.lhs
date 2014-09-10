> module Main where

> import Eqparser
> import Part2

The function parseString parses an input String and returns an Equation.
The lexer function consumes a String and generates a list of Tokens. 
The eqparse function parses this list of Tokens and generates the 
Equation out of it. 

> parseString :: String -> Equation
> parseString s = (eqparse . lexer) s

This function consumes the equation as input String and returns 
the solutions for this equation.

> solve :: String -> EquationSol
> solve s = solveEquation (parseString s)
