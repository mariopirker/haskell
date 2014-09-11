module ComputationGraph.Morphism where

import qualified Data.Map as Map 
import qualified Data.Maybe as Maybe (fromMaybe)
import qualified Data.List as List 
import qualified Data.Char as Char
import ComputationGraph.StateGraph
import ComputationGraph.Program
import ComputationGraph.HeapGraph
import ComputationGraph.State
import ComputationGraph.HelperFunctions
import ComputationGraph.Summarize
import ComputationGraph.StateGraphOutput
import ComputationGraph.PrettyState

{- ############################################################################
 - ########################### Morphism #######################################
 - ############################################################################ -}

flattenProg = Prog [ClassDecl ("Flatten",ClassBody {superclass = "Object", fielddeclarations = [], methods = [MDecl ("flatten",[ParMeth ("TreeList","list")],"IntList",MBody (2,5,[Load 1,Store 2,Push Unit,Pop,Push Null,Store 3,Push Unit,Pop,Load 2,Push Null,CmpNeq,IfFalse 60,Load 2,Getfield "value" "TreeList",Store 4,Push Unit,Pop,Load 4,Push Null,CmpNeq,IfFalse 45,Load 3,Store 5,Push Unit,Pop,New "IntList",Store 3,Push Unit,Pop,Load 3,Load 4,Getfield "value" "Tree",Putfield "value" "IntList",Push Unit,Pop,Load 3,Load 5,Putfield "next" "IntList",Push Unit,Pop,Load 2,Store 6,Push Unit,Pop,New "TreeList",Store 2,Push Unit,Pop,Load 2,Load 4,Getfield "left" "Tree",Putfield "value" "TreeList",Push Unit,Pop,Load 2,Load 6,Putfield "next" "TreeList",Push Unit,Pop,Load 6,Load 4,Getfield "right" "Tree",Putfield "value" "TreeList",Push Unit,Goto 5,Load 2,Getfield "next" "TreeList",Store 2,Push Unit,Pop,Goto (-62),Push Unit,Pop,Load 3,Return])),MDecl ("main",[ParMeth ("String","args")],"void",MBody (2,4,[New "TreeList",Store 2,Push Unit,Pop,New "Flatten",Store 3,Push Unit,Pop,Load 2,Invoke "createCyclicList" 0,Store 4,Push Unit,Pop,Load 3,Load 4,Invoke "flatten" 1,Store 5,Push Unit,Pop,Return]))]}),ClassDecl ("IntList",ClassBody {superclass = "Object", fielddeclarations = [FDecl ("IntList","next"),FDecl ("int","value")], methods = []}),ClassDecl ("Object",ClassBody {superclass = "", fielddeclarations = [], methods = []}),ClassDecl ("String",ClassBody {superclass = "Object", fielddeclarations = [], methods = [MDecl ("length",[],"int",MBody (1,0,[Push (IntVal 0),Return]))]}),ClassDecl ("Tree",ClassBody {superclass = "Object", fielddeclarations = [FDecl ("Tree","left"),FDecl ("Tree","right"),FDecl ("int","value")], methods = []}),ClassDecl ("TreeList",ClassBody {superclass = "Object", fielddeclarations = [FDecl ("TreeList","next"),FDecl ("Tree","value")], methods = [MDecl ("createCyclicList",[],"TreeList",MBody (1,2,[New "TreeList",Store 1,Push Unit,Pop,New "TreeList",Store 2,Push Unit,Pop,Load 1,Load 2,Putfield "next" "TreeList",Push Unit,Pop,Load 2,Load 1,Putfield "next" "TreeList",Push Unit,Pop,Load 1,Return]))]})]


stateSum = State {heap = Heaps {heaps = Map.fromList [(Addr 20,AbsVariable (ClassVar "TreeList")),(Addr 21,AbsVariable (ClassVar "Tree")),(Addr 22,AbsVariable (ClassVar "IntList")),(Addr 23,AbsVariable (ClassVar "TreeList")),(Addr 24,AbsVariable (ClassVar "TreeList")),(Addr 25,AbsVariable (ClassVar "Flatten"))], addrcounter = 25}, frames = [Frame {stk = [], loc = [AddVal (Addr 25),AddVal (Addr 24),AddVal (Addr 23),AddVal (Addr 22),AddVal (Addr 21),Null,AddVal (Addr 20)], cn = "Flatten", mn = "flatten", pc = 8}], isUnshared = [], state = 113}

state1 = State {heap = Heaps {heaps = Map.fromList [(Addr 4,AbsVariable (ClassVar "TreeList")),(Addr 5,AbsVariable (ClassVar "TreeList")),(Addr 6,AbsVariable (ClassVar "Flatten"))], addrcounter = 6}, frames = [Frame {stk = [], loc = [AddVal (Addr 6),AddVal (Addr 5),AddVal (Addr 4),Null,Null,Unit,Unit], cn = "Flatten", mn = "flatten", pc = 8}], isUnshared = [], state = 32}

state2=State {heap = Heaps {heaps = Map.fromList [
(Addr 4,ObjPair ("TreeList",Map.fromList [(("Tree","value"),AddVal (Addr 10)),(("TreeList","next"),AddVal (Addr 13))])),
(Addr 6,AbsVariable (ClassVar "Flatten")),
(Addr 8,ObjPair ("Tree",Map.fromList [(("Tree","left"),AddVal (Addr 9)),(("Tree","right"),AddVal (Addr 10)),(("int","value"),AbsIntVal "AbsInt")])),
(Addr 9,AbsVariable (ClassVar "Tree")),
(Addr 10,AbsVariable (ClassVar "Tree")),
(Addr 11,ObjPair ("IntList",Map.fromList [(("IntList","next"),Null),(("int","value"),AbsIntVal "AbsInt")])),
(Addr 12,AbsVariable (ClassVar "IntList")),
(Addr 13,ObjPair ("TreeList",Map.fromList [(("Tree","value"),AddVal (Addr 9)),(("TreeList","next"),AddVal (Addr 4))])),
(Addr 15,AbsVariable (ClassVar "Tree")),
(Addr 17,AbsVariable (ClassVar "Tree")),
(Addr 18,AbsVariable (ClassVar "TreeList")),
(Addr 19,AbsVariable (ClassVar "Tree"))], addrcounter = 19}, frames = [
Frame {stk = [], loc = [AddVal (Addr 6),AddVal (Addr 13),AddVal (Addr 13),AddVal (Addr 11),AddVal (Addr 8),Null,AddVal (Addr 4)], cn = "Flatten", mn = "flatten", pc = 8}], isUnshared = [], state = 111}

-- | Representing a node number
type Nodes = [NodeSG]

-- | Morphism represents a Graph morphism between TermGraph A and
-- | TermGraph B, and hence indicates a mapping from some nodes of A to 
-- | some nodes of B
type Morphism = Map.Map NodeSG NodeSG

{- ############################################################################
 - ########################### Calculation Functions ##########################
 - ############################################################################ -}

-- | Returns the root nodes (dom(Stk) and dom(Loc) of a given state graph).
-- | Input: StateGraph
-- | Output: List of nodes that has data constructor StkNode or LocNode
getRootNodes :: StateGraph -> NodesSG
getRootNodes s = foldr(\x -> (if (condition x) then (:)x else (++)[])) [] (getNodesSG s)
 where condition x = (isLocNode x) || (isStkNode x)

-- | Adds Nodes to the morphism recursively for each list of node-tuples. going out from one stk or loc node
iterateMorph p t s [] [] m = m
iterateMorph p t s dfsT [] m = m
iterateMorph p t s [] dfsS m = m
iterateMorph p t s (dT:dfsT) (dS:dfsS) m = iterateMorph p t s dfsT' dfsS' m'
 where m' | (isStkNode (fst dT)) && (isStkNode (fst dS)) = Map.insert (fst dT) (fst dS) m
          | (isLocNode (fst dT)) && (isLocNode (fst dS)) = Map.insert (fst dT) (fst dS) m
          | (isNAddrNode (fst dT)) && (isNAddrNode (fst dS)) = Map.insert (fst dT) (fst dS) m
          | (isHeapNode (fst dT)) && (isHeapNode (fst dS)) = Map.insert (fst dT) (fst dS) m
          | otherwise = Map.insert (fst dT) (fst dS) m                    
      
       dfsT' = dfsT
       dfsS' | ((snd dT) == NullNode) && (not((snd dS) == NullNode)) = cutAllSuccEdges p s (fst dS) (dS:dfsS) 
             | otherwise = dfsS 


-- | Startpoints                               
calcMorphism :: Prog -> (State,StateGraph) -> (State,StateGraph) ->  Morphism
calcMorphism p (stateT,t) (stateS,s) = (foldr(\x y -> buildMorph x y) (Map.empty) (zip rootNodesT rootNodesS))
 where buildMorph x y = do 
                        let n1 = (fst x)
                        let n2 = (snd x)
                        let nlist1 = (depthFirstSearch' p n1 (stateT,t))
                        let nlist2 = (depthFirstSearch' p n2 (stateS,s))
                        let dfsT = getEdgeList nlist1 nlist1 t
                        let dfsS = getEdgeList nlist2 nlist2 s
                        let m' = iterateMorph p t s dfsT dfsS y
                        m'

       rootNodesT = getRootNodes t
       rootNodesS = getRootNodes s


{- ############################################################################
 - ########################### Definiton 4.6 checks ###########################
 - ############################################################################ -}

checkMorphism :: Prog -> (State,StateGraph) -> (State,StateGraph) -> (Bool,Morphism)
checkMorphism p (stateT,t) (stateS,s) | (m==Map.empty) = (False,Map.empty)
                                      | otherwise  = (finalResult,m)
  where m = calcMorphism p (stateT,t) (stateS,s)
        finalResult = rootResult && nRootResult && succsResult && edgesResult
        -- Point 1
        rootResult = checkRootNodes m t s
        -- Point 2
        nRootResult= if(rootResult == False) then False else checkNotRootNodes m p (stateT,t) (stateS,s)
        -- Point 3
        succsResult = if(nRootResult==False) then False else checkSuccessors m p (stateT,t) (stateS,s)
        -- Point 4
        edgesResult = if(succsResult==False) then False else checkEdgeLabels m p (stateT,t) (stateS,s)

-- | Point 1 in the Definiton 4.6
-- | Input: StateGraph T, StateGraph S
-- | Output: Morphism
checkRootNodes :: Morphism -> StateGraph -> StateGraph ->  Bool
checkRootNodes  m t s = List.and (foldr (\x -> (:)((x == (Map.findWithDefault (error "Morphismus > CheckRootNodes") x m)) && (sameLabel (t,x) (s,(Map.findWithDefault (error "Morphismus > CheckRootNodes") x m))))) [] rootNodesT)
  where rootNodesT = getRootNodes t

-- | Point 2 in the Definition 4.6
-- | Checks point 2 in the definition for all notes that are not part of dom(stk) and dom(loc). 
-- | That are the heapgraph nodes + nodes of the abstract locations
checkNotRootNodes' :: Morphism -> Prog -> (State,StateGraph) -> (State,StateGraph) -> [Bool]
checkNotRootNodes'  m p (stateT,t) (stateS,s) = (foldr (\x -> checkP2 x) [] notRootNodesT)
  where checkP2 tnode = (:)(absValBinRel p (snd tnode) (labelS' (fst tnode)))

        notRootNodesT = foldr(\x -> if((isStkNode x) || (isLocNode x)) then (++)[] else (:)(add x)) [] (getNodesSG t)
          where add x = (x,labelT' x)

        labelS' x | isNAddrNode (Map.findWithDefault (error "line 126") x m) = labelS x
                  | (isHeapNode x) && (Char.isLower (head(getAbsClassVar2 (labelS x) "line126 1"))) = AbsClassVar(Char.toUpper (head (getAbsClassVar2(labelS x) "line 126 2")):(tail (getAbsClassVar2(labelS x) "line 126 3")))
                  | otherwise = labelS x
        labelS x  = extractNodeLabel stateS s (Map.findWithDefault (error errorStr) x m) absLocTupleS
          where errorStr = ("\n"++"Morphismus: "++"\n"++
                           (showMorph m)++"\n"++
                            "Node:"++"\n"++(showNode x)++"\n"++
                            "stateT:"++"\n"++(showState stateT p)++"\n"++
                            "statS:" ++"\n"++(showState stateS p))
        labelT' x | isNAddrNode x = labelT x
                  | (isHeapNode x) && (Char.isLower (head(getAbsClassVar2(labelT x) "line 137 1"))) = AbsClassVar(Char.toUpper (head (getAbsClassVar2(labelT x) "line 137 2")):(tail (getAbsClassVar2(labelT x) "line 137 3")))
                  | otherwise = labelT x
        labelT x = extractNodeLabel stateT t x absLocTupleT

        absValofLoc x absloct = (foldr(\y -> if((fst y)==x) then (:)(snd y) else (++)[]) [] (Map.elems absloct))!!0
        getHeapLabel hg node = nlabelHG hg node

        absLocTupleT = buildAbsLocTupleSG stateT
        absLocTupleS = buildAbsLocTupleSG stateS


checkNotRootNodes :: Morphism -> Prog -> (State,StateGraph) -> (State,StateGraph) -> Bool
checkNotRootNodes  m p (stateT,t) (stateS,s) = List.and (foldr (\x -> checkP2 x) [] notRootNodesT)
  where checkP2 tnode = (:)(absValBinRel p (labelS' (fst tnode)) (snd tnode))

        notRootNodesT = foldr(\x -> if((isStkNode x) || (isLocNode x)) then (++)[] else (:)(add x)) [] (getNodesSG t)
          where add x = (x,labelT' x)

        labelS' x | isNAddrNode (Map.findWithDefault (error "line 126") x m) = labelS x
                  | (isHeapNode x) && (Char.isLower (head(getAbsClassVar2 (labelS x) "line126 1"))) = AbsClassVar(Char.toUpper (head (getAbsClassVar2(labelS x) "line 126 2")):(tail (getAbsClassVar2(labelS x) "line 126 3")))
                  | otherwise = labelS x
        labelS x  = extractNodeLabel stateS s (Map.findWithDefault (error "line 160") x m) absLocTupleS

        labelT' x | isNAddrNode x = labelT x
                  | (isHeapNode x) && (Char.isLower (head(getAbsClassVar2(labelT x) "line 137 1"))) = AbsClassVar(Char.toUpper (head (getAbsClassVar2(labelT x) "line 137 2")):(tail (getAbsClassVar2(labelT x) "line 137 3")))
                  | otherwise = labelT x
        labelT x = extractNodeLabel stateT t x absLocTupleT

        absValofLoc x absloct = (foldr(\y -> if((fst y)==x) then (:)(snd y) else (++)[]) [] (Map.elems absloct))!!0
        getHeapLabel hg node = nlabelHG hg node

        absLocTupleT = buildAbsLocTupleSG stateT
        absLocTupleS = buildAbsLocTupleSG stateS



-- | Point 3 in the Definition 4.6 
-- | Checks point 3 in the definition for all nodes. 
checkSuccessors :: Morphism -> Prog -> (State,StateGraph) -> (State,StateGraph) -> Bool 
checkSuccessors m p (stateT,t) (stateS,s) = List.and (succCheck) 
  where succCheck = foldr (\x -> succCheck' x) [] (getNodesSG t)
          where succCheck' x | (isNAddrNode x) = (:)True
                             | (isHeapNode x) && (isClassVar (Map.findWithDefault (error "line 156") (getAddrNode(getHeapNode x)) h)) = (:)True
                             | otherwise = (:)(((foldr (\y -> (:)(Map.findWithDefault (error "line 157 1") y m)))[] (nsucc t x)) == (nsucc s (Map.findWithDefault (error "line 157 2") x m)))
                h = curHeap (getHeaps stateT)

-- Same as before, just for testing.
checkSuccessorsT :: Morphism -> Prog -> (State,StateGraph) -> (State,StateGraph) -> [Bool]
checkSuccessorsT m p (stateT,t) (stateS,s) = succCheck
  where succCheck = foldr (\x -> succCheck' x) [] (getNodesSG t)
          where succCheck' x | (isNAddrNode x) = (:)True
                             | (isHeapNode x) && (isClassVar (Map.findWithDefault (error "line 164") (getAddrNode(getHeapNode x)) h)) = (:)True
                             | otherwise = (:)(((foldr (\y -> (:)(Map.findWithDefault (error "line 165 1") y m)))[] (nsucc t x)) == (nsucc s (Map.findWithDefault (error "line 165 2") x m)))
                h = curHeap (getHeaps stateT) 
        
-- | Point 4 in the Definition 4.6
-- | Checks point 4 in the definition for all edges
checkEdgeLabels :: Morphism -> Prog -> (State,StateGraph) -> (State,StateGraph) -> Bool
checkEdgeLabels m p (stateT,t) (stateS,s) = List.and (foldr (\x -> checkEdges x) [] (Map.keys elabelsT))
  where checkEdges x  = (:)((Map.findWithDefault (error "line 172 1") x elabelsT) == (Map.findWithDefault (error "line 172 2") ((Map.findWithDefault (error "line 172 3") (fst x) m),(Map.findWithDefault (error "line 172 4") (snd x) m)) elabelsS))
  
        elabelsT = getELabelsSG t
        elabelsS = getELabelsSG s

-- | Functions for troubleshooting

-- | Show the current Morphismus. Is nice if there is an exception raise and I need to troubleshoot.
showMorph m = output 
 where nodes1= Map.keys m
       nodes2 = Map.elems m

       output = foldr (\x -> (++)(showMorphEntry ((fst x),(snd x)) ++ "\n") ) "" zipped
       zipped = zip nodes1 nodes2

showMorphEntry (n1,n2) 
      | (isStkNode n1) && (isStkNode n2) = "stknode"++(show(getStkNode n1))++"->"++"stknode"++(show(getStkNode n2))
      | (isLocNode n1) && (isLocNode n2) = "locnode"++(show(getLocNode n1))++"->"++"locnode"++(show(getLocNode n2))
      | (isHeapNode n1) && (isHeapNode n2) = (show(getAddrNode(getHeapNode n1)))++"->"++(show(getAddrNode(getHeapNode n2)))
      | (isNAddrNode n1) && (isNAddrNode n2) = "absloc"++(show(getNAddrNodeSG n1))++"->"++"absloc"++(show(getNAddrNodeSG n2))
                   
-- | Displays a node of the StateGraph
showNode n 
  | isStkNode n = "stknode"++(show(getStkNode n))
  | isLocNode n = "locnode"++(show(getLocNode n))
  | isHeapNode n = (show(getAddrNode(getHeapNode n)))
  | isNAddrNode n = "absloc"++(show(getNAddrNodeSG n))


