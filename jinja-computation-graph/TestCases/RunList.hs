module TestCases.RunList where

import ComputationGraph.Instructions
import ComputationGraph.Program
import ComputationGraph.State
import GraphViz.GraphVizTest

-- List test programm
-- Parsing the List Programm with:
-- parseToFile "aachen-jinja/ListAppend.jbc" "aachen-jinja/ListAppend.Prog"
progList = Prog [ClassDecl ("List",ClassBody {superclass = "Object", fielddeclarations = [FDecl ("List","next"),FDecl ("int","val")], methods = [MDecl ("append",[ParMeth ("List","ys")],"void",MBody (2,1,[Load 0,Store 2,Push Unit,Pop,Load 2,Getfield "next" "List",Push Null,CmpNeq,IfFalse 7,Load 2,Getfield "next" "List",Store 2,Push Unit,Pop,Goto (-10),Push Unit,Pop,Load 2,Load 1,Putfield "next" "List",Push Unit,Return]))]}),ClassDecl ("Object",ClassBody {superclass = "", fielddeclarations = [], methods = []})]

-- Runs the programm with input:
-- List-Programm
-- Start-Class: List
-- Start-Method: Append
runList n = (runComputationGraph progList "List" "append" n)


runListDot n = printToFile "listout.dot" graph
 where progOutput = runList n 
       graph = compGraphToGraph progList progOutput 
  
