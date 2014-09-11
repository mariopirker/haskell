module Testing.TestProgram where

import qualified Data.Map as Map
import qualified Data.List as List
import ComputationGraph.HelperFunctions
import ComputationGraph.State
import ComputationGraph.Program
import ComputationGraph.Refinements
import ComputationGraph.StateGraph
import ComputationGraph.Morphism
import qualified Control.Monad.State as SMonad
import ComputationGraph.HeapGraph
import ComputationGraph.Unification
import ComputationGraph.Instructions
import ComputationGraph.PrettyState

-- ## Flatten Programm ## --

progFlatten = Prog [ClassDecl ("Flatten",ClassBody {superclass = "Object", fielddeclarations = [], methods = [MDecl ("flatten",[ParMeth ("TreeList","list")],"IntList",MBody (2,4,[Load 1,Store 2,Push Unit,Pop,Push Null,Store 3,Push Unit,Pop,Load 2,Push Null,CmpNeq,IfFalse 56,Load 2,Getfield "value" "TreeList",Store 4,Push Unit,Pop,Load 4,Push Null,CmpNeq,IfFalse 41,New "IntList",Store 3,Push Unit,Pop,Load 3,Load 4,Getfield "value" "Tree",Putfield "value" "IntList",Push Unit,Pop,Load 3,Load 5,Putfield "next" "IntList",Push Unit,Pop,Load 2,Store 5,Push Unit,Pop,New "TreeList",Store 2,Push Unit,Pop,Load 2,Load 4,Getfield "left" "Tree",Putfield "value" "TreeList",Push Unit,Pop,Load 2,Load 5,Putfield "next" "TreeList",Push Unit,Pop,Load 5,Load 4,Getfield "right" "Tree",Putfield "value" "TreeList",Push Unit,Goto 5,Load 2,Getfield "next" "TreeList",Store 2,Push Unit,Pop,Goto (-58),Push Unit,Pop,Load 3,Return]))]}),ClassDecl ("IntList",ClassBody {superclass = "Object", fielddeclarations = [FDecl ("IntList","next"),FDecl ("int","value")], methods = []}),ClassDecl ("Object",ClassBody {superclass = "Object", fielddeclarations = [], methods = []}),ClassDecl ("Tree",ClassBody {superclass = "Object", fielddeclarations = [FDecl ("Tree","left"),FDecl ("Tree","right"),FDecl ("int","value")], methods = []}),ClassDecl ("TreeList",ClassBody {superclass = "Object", fielddeclarations = [FDecl ("TreeList","next"),FDecl ("Tree","value")], methods = []})]

startstateFlatten = createStartState progFlatten "Flatten" "flatten"

-- ### List Programm ## --

progList = Prog [ClassDecl ("List",ClassBody {superclass = "Object", fielddeclarations = [FDecl ("List","next"),FDecl ("int","val")], methods = [MDecl ("append",[ParMeth ("List","ys")],"void",MBody (2,1,[Load 0,Store 2,Push Unit,Pop,Load 2,Getfield "next" "List",Push Null,CmpNeq,IfFalse 7,Load 2,Getfield "next" "List",Store 2,Push Unit,Pop,Goto (-10),Push Unit,Pop,Load 2,Load 1,Putfield "next" "List",Push Unit,Return]))]}),ClassDecl ("Object",ClassBody {superclass = "Object", fielddeclarations = [], methods = []})]

-- Two Subclasses IntList and TreeList that have both the same superclass List
progList' = Prog [ClassDecl ("List",ClassBody {superclass = "Object", fielddeclarations = [FDecl ("List","next"),FDecl ("int","val")], methods = [MDecl ("append",[ParMeth ("List","ys")],"void",MBody (2,1,[Load 0,Store 2,Push Unit,Pop,Load 2,Getfield "next" "List",Push Null,CmpNeq,IfFalse 7,Load 2,Getfield "next" "List",Store 2,Push Unit,Pop,Goto (-10),Push Unit,Pop,Load 2,Load 1,Putfield "next" "List",Push Unit,Return]))]}),ClassDecl ("Object",ClassBody {superclass = "Object", fielddeclarations = [], methods = []}),ClassDecl ("IntList",ClassBody {superclass = "List", fielddeclarations = [FDecl ("List","next"),FDecl ("int","val"), FDecl ("IntList","head")], methods = []}),ClassDecl ("TreeList",ClassBody {superclass = "List", fielddeclarations = [FDecl ("List","next"),FDecl ("int","val"), FDecl ("TreeList","root")], methods = []})]

-- One extra subclass BinaryTreeList that has father class TreeList
progList'' = Prog [ClassDecl ("List",ClassBody {superclass = "Object", fielddeclarations = [FDecl ("List","next"),FDecl ("int","val")], methods = [MDecl ("append",[ParMeth ("List","ys")],"void",MBody (2,1,[Load 0,Store 2,Push Unit,Pop,Load 2,Getfield "next" "List",Push Null,CmpNeq,IfFalse 7,Load 2,Getfield "next" "List",Store 2,Push Unit,Pop,Goto (-10),Push Unit,Pop,Load 2,Load 1,Putfield "next" "List",Push Unit,Return]))]}),ClassDecl ("Object",ClassBody {superclass = "Object", fielddeclarations = [], methods = []}),ClassDecl ("IntList",ClassBody {superclass = "List", fielddeclarations = [FDecl ("List","next"),FDecl ("int","val"), FDecl ("IntList","head")], methods = []}),ClassDecl ("TreeList",ClassBody {superclass = "List", fielddeclarations = [FDecl ("List","next"),FDecl ("int","val"), FDecl ("TreeList","root")], methods = []}),ClassDecl ("BinaryTreeList",ClassBody {superclass = "TreeList", fielddeclarations = [FDecl ("List","next"),FDecl ("int","val"), FDecl ("TreeList","root"),FDecl ("BinaryTreeList","left")], methods = []})]

{-
startstateList = createStartState progList "List" "append"


-- ### Programm Run Functions ###
runProg prog  | prog=="List" = exec progList startstateList "List" "append"
              | prog=="Flatten" = exec progFlatten startstateFlatten "Flatten" "flatten" 

-- afterwards make Map.fromList to enter it into 
runProgN prog n  | prog=="List" = execN progList startstateList "List" "append" n
                 | prog=="Flatten" = execN progFlatten startstateFlatten "Flatten" "flatten" n

runProgPrettyN prog n | prog=="List" = prettyProgram (execN progList startstateList "List" "append" n) progList "List" "append" 
                | prog=="Flatten" = prettyProgram (execN progFlatten startstateFlatten "Flatten" "flatten" n) progFlatten "Flatten" "flatten"
-}

