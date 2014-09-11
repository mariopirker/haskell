import Parser.Bcparser
import ComputationGraph.Program

parseString s = (bcparse . lexer) s

parseFile f = do
    input <- readFile f
    let ast = parseString input
    let programmTest = Prog ast
    putStrLn $ show ast

