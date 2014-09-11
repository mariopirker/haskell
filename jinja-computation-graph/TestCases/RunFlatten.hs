module TestCases.RunFlatten where

import ComputationGraph.Instructions
import ComputationGraph.Unification
import ComputationGraph.Program
import ComputationGraph.State
import GraphViz.GraphVizTest
import qualified Data.Map as Map

-- Flatten test programm
-- Parsing the Flatten Programm with:
-- parseToFile "aachen-jinja/Flatten.jbc" "aachen-jinja/Flatten.Prog"
flattenProg = Prog [ClassDecl ("Flatten",ClassBody {superclass = "Object", fielddeclarations = [], methods = [MDecl ("flatten",[ParMeth ("TreeList","list")],"IntList",MBody (2,5,[Load 1,Store 2,Push Unit,Pop,Push Null,Store 3,Push Unit,Pop,Load 2,Push Null,CmpNeq,IfFalse 60,Load 2,Getfield "value" "TreeList",Store 4,Push Unit,Pop,Load 4,Push Null,CmpNeq,IfFalse 45,Load 3,Store 5,Push Unit,Pop,New "IntList",Store 3,Push Unit,Pop,Load 3,Load 4,Getfield "value" "Tree",Putfield "value" "IntList",Push Unit,Pop,Load 3,Load 5,Putfield "next" "IntList",Push Unit,Pop,Load 2,Store 6,Push Unit,Pop,New "TreeList",Store 2,Push Unit,Pop,Load 2,Load 4,Getfield "left" "Tree",Putfield "value" "TreeList",Push Unit,Pop,Load 2,Load 6,Putfield "next" "TreeList",Push Unit,Pop,Load 6,Load 4,Getfield "right" "Tree",Putfield "value" "TreeList",Push Unit,Goto 5,Load 2,Getfield "next" "TreeList",Store 2,Push Unit,Pop,Goto (-62),Push Unit,Pop,Load 3,Return])),MDecl ("main",[ParMeth ("String","args")],"void",MBody (2,4,[New "TreeList",Store 2,Push Unit,Pop,New "Flatten",Store 3,Push Unit,Pop,Load 2,Invoke "createCyclicList" 0,Store 4,Push Unit,Pop,Load 3,Load 4,Invoke "flatten" 1,Store 5,Push Unit,Pop,Return]))]}),ClassDecl ("IntList",ClassBody {superclass = "Object", fielddeclarations = [FDecl ("IntList","next"),FDecl ("int","value")], methods = []}),ClassDecl ("Object",ClassBody {superclass = "", fielddeclarations = [], methods = []}),ClassDecl ("String",ClassBody {superclass = "Object", fielddeclarations = [], methods = [MDecl ("length",[],"int",MBody (1,0,[Push (IntVal 0),Return]))]}),ClassDecl ("Tree",ClassBody {superclass = "Object", fielddeclarations = [FDecl ("Tree","left"),FDecl ("Tree","right"),FDecl ("int","value")], methods = []}),ClassDecl ("TreeList",ClassBody {superclass = "Object", fielddeclarations = [FDecl ("TreeList","next"),FDecl ("Tree","value")], methods = [MDecl ("createCyclicList",[],"TreeList",MBody (1,2,[New "TreeList",Store 1,Push Unit,Pop,New "TreeList",Store 2,Push Unit,Pop,Load 1,Load 2,Putfield "next" "TreeList",Push Unit,Pop,Load 2,Load 1,Putfield "next" "TreeList",Push Unit,Pop,Load 1,Return]))]})] 


-- Runs the programm with input:
-- Flatten-Programm
-- Start-Class: Flatten
-- Start-Method: flatten
runFlatten n = runComputationGraph flattenProg "Flatten" "flatten" n


runFlattenDot n = printToFile "flattenout.dot" graph
 where progOutput = runFlatten n
       graph = compGraphToGraph flattenProg progOutput

