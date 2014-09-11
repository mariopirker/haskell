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

progFlatten = Prog [ClassDecl ("Flatten",ClassBody {superclass = "Object", fielddeclarations = [], methods = [MDecl ("flatten",[ParMeth ("TreeList","list")],"IntList",MBody (2,4,[Load 1,Store 2,Push Unit,Pop,Push Null,Store 3,Push Unit,Pop,Load 2,Push Null,CmpNeq,IfFalse 56,Load 2,Getfield "value" "TreeList",Store 4,Push Unit,Pop,Load 4,Push Null,CmpNeq,IfFalse 41,New "IntList",Store 3,Push Unit,Pop,Load 3,Load 4,Getfield "value" "Tree",Putfield "value" "IntList",Push Unit,Pop,Load 3,Load 5,Putfield "next" "IntList",Push Unit,Pop,Load 2,Store 5,Push Unit,Pop,New "TreeList",Store 2,Push Unit,Pop,Load 2,Load 4,Getfield "left" "Tree",Putfield "value" "TreeList",Push Unit,Pop,Load 2,Load 5,Putfield "next" "TreeList",Push Unit,Pop,Load 5,Load 4,Getfield "right" "Tree",Putfield "value" "TreeList",Push Unit,Goto 5,Load 2,Getfield "next" "TreeList",Store 2,Push Unit,Pop,Goto (-58),Push Unit,Pop,Load 3,Return]))]}),ClassDecl ("IntList",ClassBody {superclass = "Object", fielddeclarations = [FDecl ("IntList","next"),FDecl ("int","value")], methods = []}),ClassDecl ("Object",ClassBody {superclass = "Object", fielddeclarations = [], methods = []}),ClassDecl ("Tree",ClassBody {superclass = "Object", fielddeclarations = [FDecl ("Tree","left"),FDecl ("Tree","right"),FDecl ("int","value")], methods = []}),ClassDecl ("TreeList",ClassBody {superclass = "Object", fielddeclarations = [FDecl ("TreeList","next"),FDecl ("Tree","value")], methods = []})]


heapsb = Heaps heapb 3
heapb' = Map.singleton (Addr 1) (ObjPair ("TreeList", ftb))
heapb = Map.insert (Addr 3) (AbsVariable (ClassVar "TreeList")) heapb'
ftb' = Map.singleton ("TreeList","next") (AddVal (Addr 3))
ftb = Map.insert ("Tree","value") (Null) ftb'
stkb = [Null, (AddVal (Addr 3))]
locb = [(AddVal (Addr 1)),(AddVal (Addr 3)), Unit, Unit, Null, Unit]
cnb = "Flatten"
mnb = "flatten"
frmsb = [Frame stkb locb cnb mnb 0]
iub = []
stateb = State heapsb frmsb iub 0
