import qualified Data.Map as Map
import qualified Data.Maybe as Maybe (fromMaybe)
import qualified Data.List as List
import ComputationGraph.StateGraph
import ComputationGraph.Program
import ComputationGraph.HeapGraph
import ComputationGraph.State
import ComputationGraph.HelperFunctions
import ComputationGraph.Instructions
import ComputationGraph.Morphism

progList = Prog [ClassDecl ("List",ClassBody {superclass = "Object", fielddeclarations = [FDecl ("List","next"),FDecl ("int","val")], methods = [MDecl ("append",[ParMeth ("List","ys")],"Unit",MBody (2,1,[Load 0,Store 2,Push Unit,Pop,Load 2,Getfield "next" "List",Push Null,CmpEq,BNot,IfFalse 7,Load 2,Getfield "next" "List",Store 2,Push Unit,Pop,Goto (-11),Push Unit,Pop,Load 2,Load 1,Putfield "next" "List",Push Unit,Return]))]})]

startstateList = createStartState progList "List" "append"
stateTList=startstateList
stateSList=startstateList

tList=mkStateGraph startstateList
sList=mkStateGraph startstateList

mList=calcMorphism progList (stateTList,tList) (stateSList,sList)
