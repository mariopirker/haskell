-- File: Main.hs
-- author: Mario Pirker
-- This has the main function. It parses the input file and runs the programm
module Main (
    main
)where

import System.Environment (getArgs)
import ComputationGraph.Instructions
import ComputationGraph.State
import ComputationGraph.HelperFunctions
import ComputationGraph.Program
import ComputationGraph.Refinements
import qualified Data.Map as Map
import qualified System.Cmd as Cmd
import Parser.Bcparser
--import GraphViz.GraphVizTest

-- | main
main = mainWith 
    where
        mainWith  = do
            args <- getArgs
            case args of
                [input,classname,methodname] -> interactWith input classname methodname
		[input,output,classname,methodname] -> interactWith' input output classname methodname
                _       -> putStrLn usage

-- | Console output
interactWith inputFile className methodname= do
    input <- readFile inputFile
    let parsedinp = parseString input
    let program   = Prog parsedinp 
    let result    = runComputationGraph program className methodname 0
    putStrLn $ show result

-- | Output file
interactWith' inputFile outputFile className methodname= do
    input <- readFile inputFile
    let parsedinp = parseString input
    let program   = Prog parsedinp
    let result    = runComputationGraph program className methodname 0
    writeFile outputFile $ show result

-- | Parses a given string (lexical analysation and parsin)
parseString s = (bcparse . lexer) s

-- | Usage function
usage =
    "Usage: \n" ++
    "#########\n" ++
    "1.) writing output to the console: \n"++
    "./compgraph <inputfile> <classname> <methodname> \n" ++
    "Example: \n" ++
    "./compgraph \"aachen-jinja/ListAppend.jbc\" \"List\" \"append\"" 
    ++ "\n\n" ++
    "2.) writing output into a file: \n" ++
    "./compgraph <inputfile> <outputfile> <classname> <methodname> \n" ++
    "Example: \n" ++
    "./compgraph \"aachen-jinja/ListAppend.jbc\" \"aachen-jinja/ListAppend.Graph\" \"List\" \"append\""
