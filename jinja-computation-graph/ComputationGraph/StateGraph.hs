module ComputationGraph.StateGraph
where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Char as Char
import ComputationGraph.State
import ComputationGraph.HelperFunctions
import ComputationGraph.HeapGraph
import ComputationGraph.Program


teststate = State {heap = Heaps {heaps = Map.fromList [(Addr 4,ObjPair ("TreeList",Map.fromList [(("Tree","value"),AddVal (Addr 8)),(("TreeList","next"),AddVal (Addr 13))])),(Addr 6,AbsVariable (ClassVar "Flatten")),(Addr 8,ObjPair ("Tree",Map.fromList [(("Tree","left"),AddVal (Addr 9)),(("Tree","right"),AddVal (Addr 10)),(("int","value"),AbsIntVal "AbsInt")])),(Addr 9,AbsVariable (ClassVar "Tree")),(Addr 10,AbsVariable (ClassVar "Tree")),(Addr 11,ObjPair ("IntList",Map.fromList [(("IntList","next"),Null),(("int","value"),AbsIntVal "AbsInt")])),(Addr 12,AbsVariable (ClassVar "IntList")),(Addr 13,ObjPair ("TreeList",Map.fromList [(("Tree","value"),AddVal (Addr 9)),(("TreeList","next"),AddVal (Addr 4))])),(Addr 15,AbsVariable (ClassVar "Tree")),(Addr 17,AbsVariable (ClassVar "Tree")),(Addr 18,AbsVariable (ClassVar "TreeList")),(Addr 19,AbsVariable (ClassVar "Tree"))], addrcounter = 19}, frames = [Frame {stk = [], loc = [AddVal (Addr 6),AddVal (Addr 13),AddVal (Addr 13),AddVal (Addr 11),AddVal (Addr 8),Null,AddVal (Addr 4)], cn = "Flatten", mn = "flatten", pc = 57}], isUnshared = [], state = 101}


flattenProg = Prog [ClassDecl ("Flatten",ClassBody {superclass = "Object", fielddeclarations = [], methods = [MDecl ("flatten",[ParMeth ("TreeList","list")],"IntList",MBody (2,5,[Load 1,Store 2,Push Unit,Pop,Push Null,Store 3,Push Unit,Pop,Load 2,Push Null,CmpNeq,IfFalse 60,Load 2,Getfield "value" "TreeList",Store 4,Push Unit,Pop,Load 4,Push Null,CmpNeq,IfFalse 45,Load 3,Store 5,Push Unit,Pop,New "IntList",Store 3,Push Unit,Pop,Load 3,Load 4,Getfield "value" "Tree",Putfield "value" "IntList",Push Unit,Pop,Load 3,Load 5,Putfield "next" "IntList",Push Unit,Pop,Load 2,Store 6,Push Unit,Pop,New "TreeList",Store 2,Push Unit,Pop,Load 2,Load 4,Getfield "left" "Tree",Putfield "value" "TreeList",Push Unit,Pop,Load 2,Load 6,Putfield "next" "TreeList",Push Unit,Pop,Load 6,Load 4,Getfield "right" "Tree",Putfield "value" "TreeList",Push Unit,Goto 5,Load 2,Getfield "next" "TreeList",Store 2,Push Unit,Pop,Goto (-62),Push Unit,Pop,Load 3,Return])),MDecl ("main",[ParMeth ("String","args")],"void",MBody (2,4,[New "TreeList",Store 2,Push Unit,Pop,New "Flatten",Store 3,Push Unit,Pop,Load 2,Invoke "createCyclicList" 0,Store 4,Push Unit,Pop,Load 3,Load 4,Invoke "flatten" 1,Store 5,Push Unit,Pop,Return]))]}),ClassDecl ("IntList",ClassBody {superclass = "Object", fielddeclarations = [FDecl ("IntList","next"),FDecl ("int","value")], methods = []}),ClassDecl ("Object",ClassBody {superclass = "", fielddeclarations = [], methods = []}),ClassDecl ("String",ClassBody {superclass = "Object", fielddeclarations = [], methods = [MDecl ("length",[],"int",MBody (1,0,[Push (IntVal 0),Return]))]}),ClassDecl ("Tree",ClassBody {superclass = "Object", fielddeclarations = [FDecl ("Tree","left"),FDecl ("Tree","right"),FDecl ("int","value")], methods = []}),ClassDecl ("TreeList",ClassBody {superclass = "Object", fielddeclarations = [FDecl ("TreeList","next"),FDecl ("Tree","value")], methods = [MDecl ("createCyclicList",[],"TreeList",MBody (1,2,[New "TreeList",Store 1,Push Unit,Pop,New "TreeList",Store 2,Push Unit,Pop,Load 1,Load 2,Putfield "next" "TreeList",Push Unit,Pop,Load 2,Load 1,Putfield "next" "TreeList",Push Unit,Pop,Load 1,Return]))]})]


{- ################### State Graph Definition ################### -} 

-- | This type is responsible for saving a map from NAddrNodes to AbsNodes during the summarization
-- | It is needed because each node needs to be unique
type NAddrNodeMap = Map.Map NodeSG NodeSG

-- | Nodes. Set of Nodes
type NodesSG = [NodeSG]

-- | Data-type of a node for the StateGraph
data NodeSG =  HeapNode NodeHG
            |  StkNode StkIndex
            |  LocNode LocIndex
            |  NAddrNode AbsLoc
            |  AbsNode AbsVal
            |  NullNode 
          deriving(Eq,Show,Ord)

-- | Case Distinction for a Stategraph Node.
caseDistNode :: NodeSG -> String
caseDistNode (StkNode s)        = "stknode"
caseDistNode (LocNode l)        = "locnode"
caseDistNode (NAddrNode a) = "abslocnode"
caseDistNode (HeapNode n)  = "heapnode"
caseDistNode (NullNode) = "nullnode"

-- | Returns True if the given Node is a StkNode
isStkNode (StkNode s) = True
isStkNode _           = False

-- | Returns true if the given Node is a LocNode
isLocNode (LocNode l) = True
isLocNode _           = False

-- | Returns true if the given Node is a HeapNode
isHeapNode (HeapNode n) = True
isHeapNode _            = False

-- | Returns true if the given node is a Heap Address Node.
isHeapAddrNode  (HeapNode (AddrNode a)) = True
isHeapAddrNode _ = False

-- | Returns true if the given node is an Abstract Class Var for a given heap + node
isClassVarNode h n | (isHeapAddrNode n) = (isClassVar (h Map.! (getAddrNode (getHeapNode n))))
                   | otherwise = False

-- | Returns true if the given node is a concrete Object. Input is the heap + node
isConcAddrNode h (HeapNode (AddrNode a)) = not(isClassVar (h Map.! a))

-- | Returns true if the given Node is a non address node
isNAddrNode (NAddrNode n) = True
isNAddrNode _             = False

-- | Returns the StkNode if the Node is of that type, otherwise it gives an error
getStkNode (StkNode s) = s
getStkNode _           = error "Node not an StkNode"

-- | Returns the LocNode if the Node is of that type, otherwise it gives an error
getLocNode (LocNode l) = l
getLocNode _           = error "Node not an LocNode"

-- | Returns the HeapNode if the Node is of that type, otherwise it gives an error
getHeapNode (HeapNode h) = h
getHeapNode _            = error "Node not a HeapNode"

-- | Returns the NAddrNode if the Node is of that type, otherwise it gives an error
getNAddrNodeSG (NAddrNode a) = a
getNAddrNodeSG _             = error "Node not an abslocnode"

-- | Returns the AbsNode if the Node is of that type, otherwise it gives an error
getAbsNode (AbsNode a) = a
getAbsNode _           = error "Node not an Abstract Value Node"

-- | Type for simulating the Successor Relation
-- | It is a mapping from one node to a list of nodes
type SuccessorsSG = Map.Map NodeSG NodesSG

-- | sameSuccessors (sg1,n1) (sg2,n2): true, if n1 in sg1 and n2 in sg2 has the same succ. relation
sameSuccessors :: (StateGraph,NodeSG) -> (StateGraph,NodeSG) -> Bool
sameSuccessors (sg1,n1) (sg2,n2) = nsucc sg1 n1 == nsucc sg2 n2

-- | Returns the successors for a given node in a state graph
nsucc :: StateGraph ->  NodeSG -> NodesSG 
nsucc sg n = (getSuccessorsSG sg) Map.! n

-- | Data type for represanting the Labels of Nodes and Edges.
data LabelSG = NLabelHG LabelHG
             | NLabelStk Os
             | NLabelLoc L 
             | NLabelAbsLoc AbsVal              
           deriving(Eq,Show,Ord)

-- | Returns the node lvel of type LabelHG if it is of that type, otherwise it gives an error
getNLabelHG (NLabelHG n) = n
getNLabelHG _ = error "Nodo Label is not of Type LabelHG"

-- | Returns true if the Label is of type NLabelStk
isNLabelStk :: LabelSG -> Bool
isNLabelStk (NLabelStk s) = True
isNLabelStk _             = False

-- | Returns true if the Label is of type NLabelLoc
isNLabelLoc :: LabelSG -> Bool
isNLabelLoc (NLabelLoc l) = True
isNLabelLoc _             = False

-- | Returns true if the Label is of type NLabelHG
isNLabelHG :: LabelSG -> Bool
isNLabelHG (NLabelHG h) = True
isNLabelHG _          = False

-- | Returns true if the Label is of type NLabelAbsLoc
isNLabelAbsLoc :: LabelSG -> Bool
isNLabelAbsLoc (NLabelAbsLoc l) = True
isNLabelAbsLoc _                = False

-- | Returns the NLabelAbsLoc if it is of that type, otherwise it gives an error
getNLabelAbsLoc (NLabelAbsLoc l) = l
getNLabelAbsLoc _ = error "Label not of type NLabelAbsLoc"

-- | sameLabel (sg,n) (sg',n'): true, if n in sg and n' in g' have the same label
sameLabel :: (StateGraph,NodeSG) -> (StateGraph,NodeSG) -> Bool
sameLabel (sg1,n1) (sg2,n2) = nlabel sg1 n1 == nlabel sg2 n2

-- | label sg n: label connected to node n in the graph sg
-- | this causes an error if n is not element of the graph.
nlabel :: StateGraph ->  NodeSG -> LabelSG
nlabel sg n = (getNLabelsSG sg) Map.! n

-- | Data Type for the Operand Stack
type Os = (String,Int,Int)
-- | Data Type for the Registers
type L  = (String,Int,Int)

-- | Edge and Node Label
-- | Edge Label Maps from an Edge (NodeSG,NodeSG) to the Label with the format (Cn,FieldId)
-- | Node Label Maps from a Node to a Node Label
type EdgeLabelSG = Map.Map EdgeSG LabelSG
type NodeLabelSG = Map.Map NodeSG LabelSG

-- | An Edge consists out of two Nodes of a Stategraph
type EdgeSG = (NodeSG, NodeSG)

-- | Data type for the StateGraph
-- | It consists of nodes, successor relation, nodelabels, edge labels and the annotations
data StateGraph = StateGraph {nodesSG::NodesSG, successorsSG::SuccessorsSG, nodelabelsSG::NodeLabelSG, edgelabelsSG::EdgeLabelSG, iu::Iu}
                 deriving(Eq,Show,Ord)

-- | Returns the nodes for a given StateGraph
-- | Input: StateGraph
-- | Output: Nodes of the StateGraph
getNodesSG :: StateGraph -> NodesSG
getNodesSG (StateGraph g _ _ _ _) = g

-- | Returns the successor relation for a given StateGraph
-- | Input: StateGraph
-- | Output: Successrelation of the whole graph
getSuccessorsSG :: StateGraph -> SuccessorsSG
getSuccessorsSG (StateGraph _ s _ _ _) = s

-- | Returns the nodelabels for a given StateGraph
-- | Input: StateGraph
-- | Output: Nodelabelling mapping
getNLabelsSG :: StateGraph -> NodeLabelSG
getNLabelsSG (StateGraph _ _ n _ _) = n

-- | Returns the Edge Labels for a given StateGraph
-- | Input: StateGraph
-- | Output: Edgelabelling mapping
getELabelsSG :: StateGraph -> EdgeLabelSG
getELabelsSG (StateGraph _ _ _ e _) = e

-- | Returns the Annotations("is Unshared") for a given StateGraph
getIuSG :: StateGraph -> Iu
getIuSG (StateGraph _ _ _ _ iu) = iu

{- ################# End of State Graph Definition ################ -}

-- | StkIndexSet and LocIndexSet. Both is mapping from stk(loc) indices to Abstract Jinja values
type StkIndexSet = Map.Map StkIndex AbsVal
type LocIndexSet = Map.Map LocIndex AbsVal

-- | This function builds up the StkIndexSet for a given State and returns this indexset 
buildStkIndexSet :: State -> StkIndexSet
buildStkIndexSet s = foldr (\x -> build x) Map.empty framedouble
  where build x = Map.union (foldr (\y -> Map.insert (StkIndex ("stk", (fst x), fst y)) (snd y)) Map.empty stk')
                               where stk' = zip [1..] (getStk (snd x))
        framedouble = zip [1..] (getFrames s)

-- | This function returns the Abstract Jinja Value stored ad a specific position from
-- | a given StkIndexSet
getStkValAtIndex :: StkIndexSet -> Int -> Int -> AbsVal
getStkValAtIndex stki i frmnumber= stki Map.! (StkIndex ("stk", frmnumber, i))

-- | This function builds up the LocIndexSet for a given State and returns this indexset
buildLocIndexSet :: State -> LocIndexSet
buildLocIndexSet s = foldr (\x -> build x) Map.empty framedouble
  where build x = Map.union (foldr (\y -> Map.insert (LocIndex ("loc", (fst x), fst y)) (snd y)) Map.empty loc')
                               where loc' = zip [1..] (getLoc (snd x))
        framedouble = zip [1..] (getFrames s)

-- | This function returns the Abstract Jinja Value stored ad a specific position from
-- | a given LocIndexSet
getLocValAtIndex :: LocIndexSet -> Int -> Int -> AbsVal
getLocValAtIndex loci i frmnumber= loci Map.! (LocIndex ("loc", frmnumber, i))

-- | This function returns the AbstractLocationTuple for a given state
-- | It builds up the map for all non-address values stored at registers, stack and the heap
buildAbsLocTupleSG :: State -> AbsLocationTuple
buildAbsLocTupleSG s = absLocTupleMap listfinal
   where  absLocTupleMap list = Map.fromList (zip poslist (zip [1..] list))

          listfinal = listHeap++listStkLoc
          listHeap = foldr (\x -> buildlist x) [] (Map.keys (curHeap (getHeaps s)))
           where buildlist x = if (isClassVar ((curHeap (getHeaps s)) Map.! x))
                               then (++)[]
                               else (++)listNonAddr
                                 where listNonAddr = foldr (\y -> if(isAddress y) then (++) [] else (:)y ) [] (listval x)
                 listval x = (rg(ft(getObjPair((curHeap (getHeaps s)) Map.! x))))

          listStkLoc      = listStkNonAddr++listLocNonAddr
          listStkNonAddr  = foldr(\x -> add x) [] (rg stkindexset)
                                where add x   = if (isAddress x == False) then (:)x else (++)[]
          listLocNonAddr  = foldr(\x -> add x) [] (rg locindexset)
                                where add x   = if (isAddress x == False) then (:)x else (++)[]

          stkindexset     = buildStkIndexSet s
          locindexset     = buildLocIndexSet s

          poslist= List.union (List.concat poslistHeap) poslistStkLoc
          poslistHeap= positionListHeap s
          poslistStkLoc=(List.union poslistStk poslistLoc)
          poslistStk = positionListStk stkindexset
          poslistLoc = positionListLoc locindexset

-- | Returns the positionlist from all non-address values stored in the heap for a given state
positionListHeap s = foldr (\x -> calc' x) [] (Map.keys (curHeap (getHeaps s)))
           where calc' x =  if (isClassVar ((curHeap (getHeaps s)) Map.! x))
                    then (++)[]
                    else (:)listPos
                     where listPos = foldr (\y -> findPos y) [] (Map.keys ftable)
                             where findPos y = if(isAddress (ftable Map.! y)==False)
                                               then (:)(HeapPos (x,fst y, snd y))
                                               else (++)[]
                           ftable = ft(getObjPair((curHeap (getHeaps s)) Map.! x))

-- | Returns the positionList for a given indexset from type Stk
positionListStk indexset = foldr(\x -> add x) [] (dom indexset)
                            where add x   = if (isAddress (indexset Map.! x))
                                            then (++)[]
                                            else (:)(StkPos x)

-- | Returns the positionList for a given indexset from type Loc
positionListLoc indexset = foldr(\x -> add x) [] (dom indexset)
                            where add x   = if (isAddress (indexset Map.! x)) 
                                            then (++)[] 
                                            else (:)(LocPos x)

-- | Returns the Abstract Jinja value at the given position
getAbsValOfAbsLocTupleSG :: AbsLocationTuple -> AbsLocPos -> AbsVal
getAbsValOfAbsLocTupleSG absloctuple pos = snd(absloctuple Map.! pos)

-- | Returns the Abstract location number for a given position
getAbsLocOfAbsLocTupleSG :: AbsLocationTuple -> AbsLocPos -> AbsLoc
getAbsLocOfAbsLocTupleSG absloctuple pos = fst(absloctuple Map.! pos)

-- | Creates the StateGraph from a given State
mkStateGraph :: State -> StateGraph
mkStateGraph s = StateGraph nodes' successors' nodelabel' edgelabel' annot
  where heapGraph       = mkHeapGraph (getHeaps s)
        heapGraphNodes  = getNodesHG heapGraph
        heapGraphSucc   = getSuccessorsHG heapGraph
        heapGraphNLabels= getNLabelsHG heapGraph
 
        h = getHeaps s
        annot = getAnnotations s
        
        curStk          = getStk (getFirstFrame(getFrames s))
        curLoc          = getLoc (getFirstFrame(getFrames s))

        stkindexset     = buildStkIndexSet s
        locindexset     = buildLocIndexSet s 
        absLocTuple     = buildAbsLocTupleSG s

        domStk          = foldr (\x -> (:)(StkNode x)) [] (dom stkindexset) 
        domLoc          = foldr (\x -> (:)(LocNode x)) [] (dom locindexset)
        domHeap         = foldr (\x -> (:)(HeapNode x)) [] heapGraphNodes
        domAbsLocations = foldr (\x -> (:)(NAddrNode x)) [] locations
        locations       = foldr (\x -> (:)(fst x)) [] (Map.elems absLocTuple)

        nodes'          = domLoc ++ domStk ++ domHeap ++ domAbsLocations 
        successors'     = foldr (\x -> succ x) Map.empty nodes'
         where succ x   = do
                           let curnode = x 
                           let absval = case (caseDistNode curnode) of
                                          "locnode" -> if(isAddress locval)
                                                       then [HeapNode (AddrNode (getAddVal locval))]
                                                       else [NAddrNode (getAbsLocOfAbsLocTuple absLocTuple (LocPos (getLocNode curnode)))]
                                                          where locval = (getLocValAtIndex locindexset (getLocIndex (getLocNode curnode)) (getLocFrmIx (getLocNode curnode)))  
                                                                
                                          "stknode" -> if(isAddress stkval)
                                                       then [HeapNode (AddrNode (getAddVal stkval))]
                                                       else [NAddrNode (getAbsLocOfAbsLocTuple absLocTuple (StkPos (getStkNode curnode)))]
                                                          where stkval = (getStkValAtIndex stkindexset (getStkIndex (getStkNode curnode)) (getStkFrmIx (getStkNode curnode)) )
                                          "heapnode"  -> if((heapGraphSucc Map.! (getHeapNode curnode)) == [])
--if (heapGraphSucc == Map.empty) 
                                                         then []
                                                         else foldr (\x -> (:)x) [] succnodes
                                                                where succnodes = foldr (\y -> insert y) [] (Map.keys f)
                                                                         where insert y = if (isAddress (f Map.! y))
                                                                                          then (:)(HeapNode(AddrNode(getAddVal (f Map.! y))))
                                                                                          else (:)(NAddrNode (getAbsLocOfAbsLocTuple absLocTuple (HeapPos (getAddrNode (getHeapNode curnode),fst y, snd y))))


                                                                      f = if (isClassVar ((curHeap h) Map.! (getAddrNode (getHeapNode curnode))))
                                                                          then Map.empty
                                                                          else ft(getObjPair((curHeap h) Map.! (getAddrNode (getHeapNode curnode))))

                                          "abslocnode" -> [] 
                           Map.insert curnode absval
        nodelabel'      = foldr (\x -> label x) Map.empty nodes'
         where label x  = do
                           let curnode = x
                           case (caseDistNode curnode) of
                                       "locnode"-> Map.insert curnode (NLabelLoc ("reg", getLocFrmIx (getLocNode curnode), getLocIndex (getLocNode curnode)))
                                       "stknode"-> Map.insert curnode (NLabelStk ("opstk", getStkFrmIx (getStkNode curnode), getStkIndex (getStkNode curnode)))
                                       "heapnode"-> Map.insert curnode (NLabelHG (heapGraphNLabels Map.! (getHeapNode curnode)))
                                       "abslocnode"-> Map.insert curnode (NLabelAbsLoc (pos!!0))
                          where pos = foldr(\y -> if((fst y)==getNAddrNodeSG x) then (:)(snd y) else (++)[]) [] (Map.elems absLocTuple)
        edgelabel'      = foldr (\x -> label x) Map.empty heapGraphNodes
           where label x = do
                            let curnode = x
                            let f    = if (isClassVar ((curHeap h) Map.! (getAddrNode x)))
                                       then Map.empty
                                       else ft(getObjPair((curHeap h) Map.! (getAddrNode x)))
                            let ret = foldr (\x -> insert x) Map.empty (Map.keys f)
                                 where insert x | ((isAddrNode curnode) && (isAddrNode (succnode x))) = Map.insert (HeapNode curnode, HeapNode (succnode x)) (NLabelHG (ELabel x))
                                                | ((isAddrNode curnode) && (not(isAddrNode (succnode x)))) = Map.insert (HeapNode curnode, NAddrNode(getNAddrNodeHG(succnode x))) (NLabelHG (ELabel x))
                                       succnode x = if (isAddress (f Map.! x))
                                                    then (AddrNode(getAddVal (f Map.! x)))
                                                    else (NAddrNodeHG(getAbsLocOfAbsLocTuple absLocTuple (HeapPos (getAddrNode curnode,fst x, snd x))))
                            Map.union ret

-- | Defines the case distinction for a two given Abstract Jinja Values. It is also defined on Abstract Class Variables.
caseDistAbsValBinRel :: AbsVal -> AbsVal -> String
caseDistAbsValBinRel Unit Null        = "one"
caseDistAbsValBinRel Unit (AbsIntVal a)  = "one"
caseDistAbsValBinRel Unit (AbsBoolVal a) = "one" 
caseDistAbsValBinRel Unit (AbsClassVar a) = "one"
caseDistAbsValBinRel Null (AbsIntVal a)  = "two"
caseDistAbsValBinRel Null (AbsBoolVal a)  = "two"
caseDistAbsValBinRel Null (AbsClassVar a) = "two"
caseDistAbsValBinRel (IntVal i) (AbsIntVal a) = "three"
caseDistAbsValBinRel (BoolVal b) (AbsBoolVal a)= "four"
caseDistAbsValBinRel (AbsClassVar a) (AbsClassVar b) = "five"
caseDistAbsValBinRel _ _ = "false"

-- | Checks wether the Binary Relation between the two Jinja Values holds or not
-- | Input: Programm, two Abstract Values
-- | Output: True or False
absValBinRel :: Prog -> AbsVal -> AbsVal -> Bool
absValBinRel p a1 a2 = if (a1 == a2) then True
                     else case (caseDistAbsValBinRel a1 a2) of 
                                "one"      -> True
                                "two"      -> True
                                "three"    -> True
                                "four"     -> True
                                "five"     -> if (isSubClass (buildSubClassRelation p) (getAbsClassVar2 a2 "line 378 1") (getAbsClassVar2 a1 "line 378 2")) then True else False
                                _          -> False

               
-- Returns the Edge Labels going out of this address.
-- Input: State and the Address Node
-- Output: List of Labels
getEdgeLabelsFromAddr :: State -> NodeSG -> [LabelSG]
getEdgeLabelsFromAddr s n1 = eLabels n1
  where sg = mkStateGraph s
        el = getELabelsSG sg
        eLabels n1 = foldr(\x -> if((fst x) == n1)then(:)(el Map.! x) else (++) [])[] (Map.keys el)

-- | Checks if a given Abstract Jinja Value is reachable from a given Address.
isReachable ::  State -> Address -> AbsVal -> Bool
isReachable state a val = (List.or (checkReachable state a val))

-- | Recursively traverses the FieldTable Jinja Values for a given Edge and stops recursion if it has reached the Jinja value
-- | or calls itself if there is another addresse (another sub tree)
checkReachable :: State -> Address -> AbsVal -> [Bool]
checkReachable state a val  
                        | (isClassVar obj) && (a /= (getAddVal val)) = [False]
                        | (isClassVar obj) && (a == (getAddVal val)) = [True]
                        | otherwise =  (foldr (\x -> checkvals x) [] ftablevals)
 where checkvals x | (x==val) = (:)True
                   | ((isAddVal x) && (isObjPair (heap Map.! a)))  = (++)(checkReachable state (getAddVal x) val)
                   | otherwise = (:)False

       heap = getHeap (getHeaps state)
       obj = (heap Map.! a)
       obj' = getObjPair (obj)
       ftable = snd obj'
       ftablevals = Map.elems ftable

-- | Checks wether the given address reaches itself again (cycle!)
isCycle state a = List.or (checkCycle state a [])

-- | Checks if there is a cycle. 
checkCycle state a addrs 
                    | List.elem a addrs = [True]
                    | isClassVar obj = [False]
                    | otherwise = (foldr (\x -> checkvals x) [] ftablevals)
 where checkvals x | x==(AddVal a) = (:)True
                   | ((isAddVal x) && (isObjPair (heap Map.! a)))  = (++)(checkCycle state (getAddVal x) (a:addrs))
                   | otherwise = (:)False


       heap = getHeap (getHeaps state)
       obj = (heap Map.! a)
       obj' = getObjPair (obj)
       ftable = snd obj'
       ftablevals = Map.elems ftable

-- | CycleCheck
cycleCheck p s = (gencheck,addrs)
 where gencheck = List.or(foldr(\x -> (:)(isCycle s x)) [] (Map.keys (getHeap(getHeaps s))))
       addrs = reverse (foldr(\x -> if((isCycle s x)) then (:)x else (++)[]) [] (Map.keys (getHeap(getHeaps s))))

-- | Removes the cycle out of the heap while it replaces the address that causes the with an abstract class variable.
deleteCycle state addresses = State heaps' (getFrames state) (getAnnotations state) (getStateId state)
                   where heaps' = Heaps heap' (curAddrCounter (getHeaps state))
                         heap' = foldr(\x y -> add x y) heap'' addresses
                            where obj x = heap'' Map.! x
                                  add x y = do
                                             let addr = x
                                             let obj = heap'' Map.! x
                                             let cn = (cl obj)
                                             let cn' = (Char.toLower (head (cn)):(tail cn))
                                             let h' = (Map.delete x y)
                                             let h'' = Map.insert addr (AbsVariable (ClassVar cn)) h'
                                             h''
                         heap'' =  (getHeap (getHeaps state))
  

-- | Checks wether an address on the Heap is not referenced by any field or is stored by any stk or loc
isReferenced state addr = (heapcheck || frmscheck,addr)
 where heapcheck = List.or(foldr(\x -> (:)(addrcheck x)) [] (List.delete addr (Map.keys(getHeap(getHeaps state)))))
       addrcheck x = isReachable state x (AddVal addr)

       frmscheck = List.or (foldr(\x -> (:)(refCheck x)) [] frames)
       refCheck x = (stkcheck x) || (loccheck x)
       stkcheck f = List.or(foldr(\x -> (:)((AddVal (addr)) == x)) [] (getStk f))
       loccheck f = List.or(foldr(\x -> (:)((AddVal (addr)) == x)) [] (getLoc f))
       frames = getFrames state


-- | Simply merges or builds the union of two stategraphs
mergeStateGraphs :: StateGraph -> StateGraph -> StateGraph
mergeStateGraphs sg1 sg2 = sg3 
  where sg3 = StateGraph sg3nodes sg3succs sg3nlabels sg3elabels sg3ius
        sg3nodes = List.union (getNodesSG sg1) (getNodesSG sg2)
        sg3succs = Map.union (getSuccessorsSG sg1) (getSuccessorsSG sg2)
        sg3nlabels = Map.union (getNLabelsSG sg1) (getNLabelsSG sg2)
        sg3elabels = Map.union (getELabelsSG sg1) (getELabelsSG sg2)
        sg3ius = List.union (getIuSG sg1) (getIuSG sg2)
        
-- | Sorts and preprare an unsorted stategraph into a sorted one. This is needed after Summarization.
-- | Additionally, this dummy sucessornode (NullNode) is replaced by the empty successor []
sortAndPrepareSG :: StateGraph -> StateGraph 
sortAndPrepareSG sg = sg' 
 where sg' = StateGraph nodes' succs' nlabels' elabels' iu'
       nodes' = sgnodesStk++sgnodesLoc++sgnodesHeap
       sgnodesStk = sortStk(foldr(\x -> if(isStkNode x) then (:)x else (++)[]) [] (getNodesSG sg))
       sgnodesLoc = sortLoc(foldr(\x -> if(isLocNode x) then (:)x else (++)[]) [] (getNodesSG sg))
       sgnodesHeap = sortHeap(foldr(\x -> if(isHeapNode x) then (:)x else (++) []) [] (getNodesSG sg))
       sgnaddr = foldr(\x -> if(isNAddrNode x) then (:)x else (++)[]) [] (getNodesSG sg)
       
       succs' = Map.union stksuccs (Map.union locsuccs heapsuccs)
       stksuccs = foldr(\x -> Map.insert x ((getSuccessorsSG sg) Map.! x)) Map.empty sgnodesStk
       locsuccs = foldr(\x -> Map.insert x ((getSuccessorsSG sg) Map.! x)) Map.empty sgnodesLoc 
       heapsuccs = foldr(\x -> prep x) Map.empty sgnodesHeap
            where prep x = if(((getSuccessorsSG sg) Map.! x)==[NullNode])
                           then Map.insert x []
                           else Map.insert x ((getSuccessorsSG sg) Map.! x) 
       nlabels' = Map.union stknlabels(Map.union locnlabels heapnlabels) 
       stknlabels= foldr(\x -> if(isStkNode x) then Map.insert x ((getNLabelsSG sg) Map.! x) else Map.union Map.empty) Map.empty (Map.keys (getNLabelsSG sg))
       locnlabels= foldr(\x -> if(isLocNode x) then Map.insert x ((getNLabelsSG sg) Map.! x) else Map.union Map.empty) Map.empty (Map.keys (getNLabelsSG sg))
       heapnlabels=foldr(\x -> if(isHeapNode x) then Map.insert x ((getNLabelsSG sg) Map.! x) else Map.union Map.empty) Map.empty (Map.keys (getNLabelsSG sg))

       elabels'=(getELabelsSG sg)
       iu' = (getIuSG sg)

-- | Sorts the Loc elements
sortLoc list = List.sortBy locOrder list
locOrder (LocNode(LocIndex (_,x1,y1))) (LocNode(LocIndex(_,x2,y2))) = case compare x1 x2 of
        EQ -> compare y1 y2
        r  -> r

-- | Sorts the Stk elements
sortStk list = List.sortBy stkOrder list
stkOrder (StkNode(StkIndex (_,x1,y1))) (StkNode(StkIndex(_,x2,y2))) = case compare x1 x2 of
        EQ -> compare y1 y2
        r  -> r

-- | Sorts the Heap elements
sortHeap list = List.sortBy heapOrder list
heapOrder (HeapNode (AddrNode (Addr x))) (HeapNode(AddrNode (Addr y))) = compare x y


-- | Converts a state graph to a state
-- | Function for converting the given Stategraph into a state of the computationgraph
stateGraphToState :: StateGraph -> String -> String -> Int -> Int -> NAddrNodeMap -> State 
stateGraphToState sg cn mn pc stateid map = State heaps frames isUnshared stateid
 where frames = foldr(\x -> (buildFrame x)) [] (res zipped (maxFrame 0 zipped))
                  where buildFrame x = (:)(Frame (if((stk x)==[]) then [] else (stk x)) (if((loc x)==[]) then [] else (loc x)) cn mn pc)  
                                          where stk list = foldr(\x -> if(isHeapNode x) then (:)(AddVal (getAddrNode(getHeapNode x))) else (:)(absVal x)) [] (stk' list)
                                                loc list = foldr(\x -> if(isHeapNode x) then (:)(AddVal (getAddrNode(getHeapNode x))) else (:)(absVal x)) [] (loc' list)

                                                stk' list = (foldr(\x -> if(isStkNode x) then (:)((successors Map.! x)!!0) else (++)[]) [] list)
                                                loc' list = (foldr(\x -> if(isLocNode x) then (:)((successors Map.! x)!!0) else (++)[]) [] list)

       absVal x | (Map.null map) = (getNLabelAbsLoc (nlabels Map.! x))
                | otherwise = (getAbsNode (map Map.! x))
       
       -- Determines the maximum frame number out of the stkindexes and locindexes
       maxFrame max [] = max
       maxFrame max (x:xs) | (isStkNode x) = if ((centerTripel (getStkIndexSet (getStkNode x))) > max) 
                                             then maxFrame (centerTripel (getStkIndexSet (getStkNode x))) xs
                                             else maxFrame max xs  
                           | (isLocNode x) = if ((centerTripel (getLocIndexSet (getLocNode x))) > max)
                                             then maxFrame (centerTripel (getLocIndexSet (getLocNode x))) xs
                                             else maxFrame max xs
       zipped = stknodes++locnodes
       stknodes = foldr(\x -> if(isStkNode x) then (:)x else (++)[]) [] nodesSG
       locnodes =  foldr(\x -> if(isLocNode x) then (:)x else (++)[]) [] nodesSG
       nlabels = getNLabelsSG sg
       isUnshared = getIuSG sg 
       
       successors = getSuccessorsSG sg 
       nodesSG=getNodesSG sg
       elabels=getELabelsSG sg
       elabelsnodes = Map.keys elabels

       -- splits up the list into sublists with same frame indices
       -- limit is the maximum frame number -1
       f x (StkNode (StkIndex (_,y,_))) = x == y
       f x (LocNode (LocIndex (_,y,_))) = x == y
       f x _ = False
       res list limit = foldl (\x y -> filter (f y) list : x) [] (take limit [1..])
       
       -- Builds up the heap out of the stategraph
       heaps = Heaps (foldr (\x -> buildHeap x) Map.empty nodes) heapaddrcounter
         where buildHeap x = if ((successors Map.! x == []))
                      then Map.insert (getAddrNode (getHeapNode x)) (AbsVariable (ClassVar((getNLabel(getNLabelHG(nlabels Map.! x))))))
                      else Map.insert (getAddrNode (getHeapNode x)) (ObjPair(getNLabel(getNLabelHG(nlabels Map.! x)),(ftable x)))

               ftable x = foldr (\y -> addToFTable y) Map.empty elabelsnodes
                              where addToFTable y =  if ((fst y) == x)
                                                     then Map.insert (getELabel(getNLabelHG(elabels Map.! y))) (sndNode (snd y))
                                                     else Map.union Map.empty

               sndNode y | (isHeapAddrNode y) = (AddVal(getAddrNode (getHeapNode y)))
                         | otherwise = (absVal y)
               nodesHeap = foldr(\x -> if(isHeapNode x) then (:)x else (++)[]) [] nodesSG
               nodes = foldr(\x -> if(isAddrNode (getHeapNode x)) then (:)x else (++)[]) [] nodesHeap 

               heapaddrcounter 
                    | (nlist == []) = 0
                    | otherwise = last(nlist)
                       where nlist = List.sort(foldr(\x -> if(isHeapAddrNode x) then (:)(getAddress(getAddrNode (getHeapNode x))) else (++)[]) [] (getNodesSG sg)) 

-- | Returns the predecessor of an edge. 
getPredecessor s succs nlist 
         | (isLocNode s) = s
         | (isStkNode s) = s
         | otherwise = (List.intersect nlist (foldr(\x -> if(List.elem s (succs Map.! x)) then (:)(x) else (++)[]) [] (Map.keys succs)))!!0



