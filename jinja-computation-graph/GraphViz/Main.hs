-- File: GraphViz/Main.hs
-- author: Mario Pirker
-- This has the main function for making a dot file out of the computation graph. It parses the input file and runs the programm
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
import GraphViz.GraphVizTest

main = mainWith
    where
        mainWith  = do
            args <- getArgs
            case args of
		[input,output,classname,methodname] -> interactWith'' input output classname methodname
--		_       -> putStrLn usage

-- | prints the output to a dot file
interactWith'' inputFile outputFile className methodname= do
    input <- readFile inputFile
    let parsedinp = parseString input
    let program   = Prog parsedinp
    let result    = runComputationGraph program className methodname 0
    let result'   = compGraphToGraph program result
    printToFile outputFile result'

-- | Parses a given string (lexical analysation and parsin)
parseString s = (bcparse . lexer) s

usage =
    "Usage: \n" ++
    "#########\n" ++
    "Converting the output into a dot file\n" ++
    "./compgraph <inputfile> <outputfile> <classname> <methodname> \n" ++
    "./compgraph \"aachen-jinja/ListAppend.jbc\" \"listoutput\" \"List\" \"append\" \n"
