-- file: GraphViz/GraphVizTest.hs
-- author: Mario Pirker
-- This file provides the final functions for converting the ComputationGraph into a .dot file
module GraphViz.GraphVizTest where

import Data.Graph.Inductive
import Data.GraphViz
import Data.GraphViz.Attributes
import ComputationGraph.HelperFunctions
import ComputationGraph.State
import ComputationGraph.ComputationGraph
import ComputationGraph.PrettyState
import ComputationGraph.Program
import ComputationGraph.Instructions
--import TestCases.RunList
--import Testing.TestProgram
import qualified Data.List as List
import qualified Data.Map as Map

------------------------------------------------

-- | Predefined Test Graph
gnomes :: Gr String String
gnomes = mkGraph [(1, "Collect underpants"), (3, "Profit")] [(1, 3, "?")] :: Gr String String


-- || Paramethers for the Graph 
graphParameters :: GraphvizParams a String String () String
graphParameters = nonClusteredParams {globalAttributes = ga,	
                                      fmtNode = fn,
		                      fmtEdge = fe
                                     }
  where fn (n,l) = [toLabel l]
	fe (f,t,l) = [toLabel l]
 

-- | Global Attributes for printing the Dot Graph
ga :: [GlobalAttributes]
ga = [ GraphAttrs []
         , NodeAttrs  [shape BoxShape]
         , EdgeAttrs  [color Black]
     ]


-- || Converts the Computationgraph output into a default Graph for further conversion into DotGraph
compGraphToGraph :: Prog -> [(State,([State],[Constraint]))] -> Gr String String
compGraphToGraph prog compGraph = mkGraph (Map.toList nodes) edges :: Gr String String
 where edges = foldr(\x -> getEdges x) [] compGraph
                 where getEdges x =  do 
                                      let fstnode  = getStateNumber (fst x)
                                      let sndnodes = foldr(\y -> (:)(getStateNumber y)) [] (fst (snd x))
                                      let fstnodeLst = replicate (length sndnodes) fstnode 
                                      let labels = snd (snd x)
                                      let edges = zip3 fstnodeLst sndnodes labels
                                      (++)edges
       nodes = foldr(\x -> getNodes x) Map.empty compGraph
                 where getNodes x   = do
                                       let node = Map.insert (getStateNumber (fst x)) (showState (fst x) prog) Map.empty
                                       let succs = foldr(\y -> Map.insert (getStateNumber y) (showState y prog)) Map.empty (fst (snd x))

                                       let ret = Map.union node succs
                                       Map.union ret
       getStateNumber x = getStateId x

-- | Converts the Graph into a Dot Graph
convGrToDotGraph g = graphToDot graphParameters g

-- | Runs the GraphViz and write the Output into a dot file
printToFile fp g = runGraphviz (convGrToDotGraph g) DotOutput fp



