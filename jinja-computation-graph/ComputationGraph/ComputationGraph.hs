module ComputationGraph.ComputationGraph 
where 

import ComputationGraph.State
import ComputationGraph.HelperFunctions
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Graph.Inductive

-- | CompGraph Node is a list of tuples from the form (Integer,String)
type CompGraphNodes = [(Int,String)]
-- | CompGraphEdges is a list of triples of the form (Integer,Integer,Constraint) where Constraint is a String
type CompGraphEdges = [(Int, Int, Constraint)]

-- | Nodes of the ComputationGraphs is a list of states
type NodesCG = [State]
-- | Each Edge goes from one state to another and may have a nonempty constraint
type EdgesCG = Map.Map (State,State) Constraint

-- | Is the type for a Path within the computationgraph
--type Path = [(State,Bool)]
type Path = [State]

-- | Pushes an element on the Path Stack
pushPath v stk = v:stk

-- | Pops an element from the Path Stack
popPath stk = tail stk
