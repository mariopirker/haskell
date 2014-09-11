module ComputationGraph.RefinementsTest where

import qualified Data.Map as Map
import qualified Data.List as List
import ComputationGraph.HelperFunctions
import ComputationGraph.State
import ComputationGraph.Program
import ComputationGraph.Refinements
import ComputationGraph.StateGraph
import ComputationGraph.Morphism
import ComputationGraph.HeapGraph
import ComputationGraph.Unification
import ComputationGraph.Instructions

heapsb = Heaps heapb 2
heapb' = Map.singleton (Addr 1) (ObjPair ("TreeList", ftb))
heapb = Map.insert (Addr 3) (AbsVariable (ClassVar "TreeList")) heapb'
ftb' = Map.singleton ("TreeList","next") (AddVal (Addr 3))
ftb = Map.insert ("Tree","value") (Null) ftb'
stkb = [Null, (AddVal (Addr 1))]
locb = [(AddVal (Addr 1)),(AddVal (Addr 3)), Unit, Unit, Null, Unit]
cnb = "Flatten"
mnb = "flatten"
frmsb = [Frame stkb locb cnb mnb 0]
iub = []
stateb = State heapsb frmsb iub 0
