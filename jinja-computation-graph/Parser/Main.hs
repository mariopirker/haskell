module Main (
    main
)where

import System.Environment (getArgs)
import ComputationGraph.Instructions
import ComputationGraph.State
import ComputationGraph.HelperFunctions
import ComputationGraph.Program
import ComputationGraph.Refinements
import Parser.Parsing
import qualified Data.Map as Map
import Parser.Bcparser

main = mainWith
    where
        mainWith  = do
            args <- getArgs
            case args of
		[input] -> interactWith input
                [input,output] -> interactWith' input output
                _       -> putStrLn usage



interactWith' inputFile outputFile=parseToFile inputFile outputFile

interactWith inputFile = do
    input <- readFile inputFile
    let parsedinp = parseString input
    let program   = Prog parsedinp
    putStrLn $ show program

usage =
    "Usage: \n" ++
    "#########\n" ++
    "1.) saving the parsed input in an outputfile: \n"
    "./bcparser <inputfile> <outputfile>\n" ++
    "Example: \n" ++
    "./bcparser \"aachen-jinja/ListAppend.jbc\" \"aachen-jinja/ListAppend.Prog\"" ++ "\n\n" ++
    "2.) printing the parsed input to the console: \n" ++
    "./bcparser <inputfile>\n" ++
     "Example: \n" ++
    "./bcparser \"aachen-jinja/ListAppend.jbc\""  
