module Parser.Parsing where

import Parser.Bcparser
import ComputationGraph.Program

-- | Parses a File and writes it into an output File
parseToFile inputFile outPutFile = do
    input <- readFile inputFile 
    let parsed = parseString input
    let programm = Prog parsed
    let str = show programm 
    writeFile outPutFile str

-- | Parses the input file
parseString s = (bcparse . lexer) s
