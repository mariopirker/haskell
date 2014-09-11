module Testing.UnificationTest where

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
import ComputationGraph.PrettyState
--import ComputationGraph.Summarize

progFlatten = Prog [ClassDecl ("Flatten",ClassBody {superclass = "Object", fielddeclarations = [], methods = [MDecl ("flatten",[ParMeth ("TreeList","list")],"IntList",MBody (2,5,[Load 1,Store 2,Push Unit,Pop,Push Null,Store 3,Push Unit,Pop,Load 2,Push Null,CmpEq,IfFalse 60,Load 2,Getfield "value" "TreeList",Store 4,Push Unit,Pop,Load 4,Push Null,CmpEq,IfFalse 45,Load 3,Store 5,Push Unit,Pop,New "IntList",Store 3,Push Unit,Pop,Load 3,Load 4,Getfield "value" "Tree",Putfield "value" "IntList",Push Unit,Pop,Load 3,Load 5,Putfield "next" "IntList",Push Unit,Pop,Load 2,Store 6,Push Unit,Pop,New "TreeList",Store 2,Push Unit,Pop,Load 2,Load 4,Getfield "left" "Tree",Putfield "value" "TreeList",Push Unit,Pop,Load 2,Load 6,Putfield "next" "TreeList",Push Unit,Pop,Load 6,Load 4,Getfield "right" "Tree",Putfield "value" "TreeList",Push Unit,Goto 5,Load 2,Getfield "next" "TreeList",Store 2,Push Unit,Pop,Goto (-62),Push Unit,Pop,Load 3,Return]))]}),ClassDecl ("IntList",ClassBody {superclass = "Object", fielddeclarations = [FDecl ("IntList","next"),FDecl ("int","value")], methods = []})]


{- ##### Test data for Unification Test #### -}
-- Test State Figure 4: Abstract State A
heapsa = Heaps heapa 1  
heapa = Map.singleton (Addr 1) (AbsVariable (ClassVar "TreeList"))
stka = []
loca = [(AddVal (Addr 1)), (AddVal (Addr 1)), Unit, Unit, Null, Unit]
cna = "Flatten"
mna = "flatten"
frmsa = [Frame stka loca cna mna 0]
iua = []
statea = State heapsa frmsa iua 0

-- Test State Figure 5: Abstract State B
heapsb = Heaps heapb 2  
heapb' = Map.singleton (Addr 1) (ObjPair ("TreeList", ftb))
heapb = Map.insert (Addr 3) (AbsVariable (ClassVar "TreeList")) heapb'
ftb' = Map.singleton ("TreeList","next") (AddVal (Addr 3)) 
ftb = Map.insert ("Tree","value") (Null) ftb'
stkb = []
locb = [(AddVal (Addr 1)),(AddVal (Addr 3)), Unit, Unit, Null, Unit]
cnb = "Flatten"
mnb = "flatten"
frmsb = [Frame stkb locb cnb mnb 0]
iub = []
stateb = State heapsb frmsb iub 0

-- Test State Figure 6: Abstract State C
heapsc = Heaps heapc 8  

heapsc' = Map.singleton (Addr 1) (ObjPair ("TreeList", ftc1))
ftc1' = Map.singleton ("TreeList","next") (AddVal (Addr 3))
ftc1 = Map.insert ("Tree","value") (AddVal (Addr 6)) ftc1'
--ftc1 = Map.insert ("Tree","value") (Null) ftc1'

heapsc'' = Map.insert (Addr 2) (ObjPair ("Tree",ftc2)) heapsc'
ftc2' = Map.singleton ("Tree","left") (AddVal (Addr 6))
ftc2'' = Map.insert ("int","value") (AbsIntVal "AbsInt") ftc2'
ftc2 = Map.insert ("Tree","right") (AddVal (Addr 7)) ftc2''

heapsc'''= Map.insert (Addr 3) (AbsVariable (ClassVar "TreeList")) heapsc'' 

heapsc'''' = Map.insert (Addr 4) (ObjPair ("IntList", ftc4)) heapsc'''
ftc4' = Map.singleton ("IntList","next") (Null)
ftc4 = Map.insert ("int","value") (AbsIntVal "AbsInt") ftc4'

heapsc''''' = Map.insert (Addr 6) (AbsVariable (ClassVar "Tree")) heapsc''''
heapsc'''''' = Map.insert (Addr 7) (AbsVariable (ClassVar "Tree")) heapsc'''''

heapc = Map.insert (Addr 8) (ObjPair ("TreeList", ftc8)) heapsc''''''
ftc8' = Map.singleton ("TreeList","next") (AddVal (Addr 1))
ftc8 = Map.insert ("Tree","value") (AddVal (Addr 6)) ftc8'

stkc = []
locc = [(AddVal (Addr 1)),(AddVal (Addr 8)), (AddVal (Addr 2)), Null , (AddVal (Addr 4)), (AddVal (Addr 1))]
cnc = "Flatten"
mnc = "flatten"
frmsc = [Frame stkc locc cnc mnc 0]
iuc = []
statec = State heapsc frmsc iuc 0


-- Test State with 80 Addresses for testing in a bigger way
stated = State heapsd frmsc iuc 0 

heapsd = Heaps heapd 80

heapd = Map.fromList addrList
 where addrList = zipWith (\x y -> (Addr x,y)) [1..] objListAll

       objListAll = List.concat (replicate 10 objList) 

       objList = [obj1,obj2,obj3,obj4,obj6,obj7,obj8]

       obj1 = ObjPair ("TreeList", ftc1)
       ftc1' = Map.singleton ("TreeList","next") (AddVal (Addr 3))
       ftc1 = Map.insert ("Tree","value") (AddVal (Addr 6)) ftc1'

       obj2 = ObjPair ("Tree",ftc2)
       ftc2' = Map.singleton ("Tree","left") (AddVal (Addr 6))
       ftc2'' = Map.insert ("int","value") (AbsIntVal "AbsInt") ftc2'
       ftc2 = Map.insert ("Tree","right") (AddVal (Addr 7)) ftc2'' 

       obj3= AbsVariable (ClassVar "TreeList")

       obj4 = ObjPair ("IntList", ftc4)
       ftc4' = Map.singleton ("IntList","next") (Null)
       ftc4 = Map.insert ("int","value") (AbsIntVal "AbsInt") ftc4'

       obj6 = AbsVariable (ClassVar "Tree")
       obj7 = AbsVariable (ClassVar "Tree")

       obj8 = ObjPair ("TreeList", ftc8)
       ftc8' = Map.singleton ("TreeList","next") (AddVal (Addr 1))
       ftc8 = Map.insert ("Tree","value") (AddVal (Addr 6)) ftc8'

-- PerformanceTest



putfieldTest prog state addr n = foldr (\x -> (++)(putfieldCheck prog state x)) [] addrList
 where addrList = replicate n addr


stateTest = State {heap = Heaps {heaps = Map.fromList [
(Addr 4,ObjPair ("TreeList",Map.fromList [(("Tree","value"),Null),(("TreeList","next"),AddVal (Addr 7))])),
(Addr 6,AbsVariable (ClassVar "Flatten")),
(Addr 7,ObjPair ("TreeList",Map.fromList [(("Tree","value"),AddVal (Addr 10)),(("TreeList","next"),AddVal (Addr 15))])),
(Addr 10,ObjPair ("Tree",Map.fromList [(("Tree","left"),AddVal (Addr 11)),(("Tree","right"),AddVal (Addr 12)),(("int","value"),AbsIntVal "AbsInt")])),
(Addr 11,AbsVariable (ClassVar "Tree")),(Addr 12,AbsVariable (ClassVar "Tree")),
(Addr 13,ObjPair ("IntList",Map.fromList [(("IntList","next"),Null),(("int","value"),AbsIntVal "AbsInt")])),
(Addr 14,AbsVariable (ClassVar "IntList")),
(Addr 15,ObjPair ("TreeList",Map.fromList [(("Tree","value"),AddVal (Addr 11)),(("TreeList","next"),AddVal (Addr 7))])),
(Addr 17,AbsVariable (ClassVar "Tree")),
(Addr 19,AbsVariable (ClassVar "Tree")),
(Addr 20,AbsVariable (ClassVar "TreeList")),
(Addr 21,AbsVariable (ClassVar "Tree"))], addrcounter = 21}, frames = [Frame {
stk = [AddVal (Addr 12),AddVal (Addr 7)],
loc = [AddVal (Addr 6),AddVal (Addr 15),AddVal (Addr 15),AddVal (Addr 13),AddVal (Addr 10),Null,AddVal (Addr 7)],
cn = "Flatten", mn = "flatten", pc = 62}], isUnshared = [], state = 128}
