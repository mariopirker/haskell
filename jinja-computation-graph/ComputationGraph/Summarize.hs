-- | file: ComputationGraph/Summarize.hs
-- | author: Mario Pirker
-- | This module is for summarizing two states to one new state 
module ComputationGraph.Summarize where

import ComputationGraph.StateGraph
import ComputationGraph.HeapGraph
import ComputationGraph.State
import ComputationGraph.Program
import ComputationGraph.HelperFunctions
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Char as Char

{- #####################################################################################
 - #####################################################################################
 - ##################################################################################### -}

-- | The correspondence Map is a Mapping from a tuple of nodes to one node
type CorrMap = Map.Map (NodeSG,NodeSG) NodeSG

-- | Adds a new entry to the Correspondence Map
addToCorrMap :: (NodeSG, NodeSG) -> NodeSG -> CorrMap -> CorrMap 
addToCorrMap (n1,n2) (n3) cm | (Map.member (n1,n2) cm) = cm 
                             | otherwise = Map.insert (n1,n2) n3 cm

-- | Adds a new entry into the nAddrNodeMap (later on needed for return transformation from stategraph to state). 
-- | It is also needed because if there are e.g edges with the (a1,Null) -> ("List","next") and (a1,null) -> ("int","val"), both
-- | would have the same key and so only the last entry would stay.
addTonAddrNodeMap :: NodeSG -> NodeSG -> NAddrNodeMap -> NAddrNodeMap
addTonAddrNodeMap naddrnode absnode map = Map.insert naddrnode absnode map 

-- | Returns a new fresh NAddrNode. Determines the highest number inisde the Map already and then returns the 
-- | new fresh NAddrNode
retNewNAddrNode :: NAddrNodeMap -> NodeSG
retNewNAddrNode map = number 
 where number | (Map.null map) = NAddrNode 1
              | otherwise = NAddrNode ((last (nlist))+1)
       nlist = List.sort(foldr(\x -> (:)(getNAddrNodeSG x)) [] (Map.keys map))

-- | extracts the node label out of a certain node. 
extractNodeLabel state sg x absloct = 
      case (caseDistNode x) of
        "abslocnode"  -> (absValofLocation (getNAddrNodeSG x) absloct)
        "heapnode"    -> if((isNLabel (nlabelHG (mkHeapGraph (getHeaps state)) (getHeapNode x)))==True)
                         then (AbsClassVar (getNLabel (nlabelHG (mkHeapGraph (getHeaps state)) (getHeapNode x))))
                         else (getNLabelAbs(nlabelHG (mkHeapGraph (getHeaps state)) (getHeapNode x)))
        _            -> Null

nLabelStr state sg x absloct  = getAbsClassVar2 (extractNodeLabel state sg x absloct) "line 51"
 where label' = (extractNodeLabel state sg x absloct)

-- | Returns the abstract value stored within a given abstract location
absValofLocation x absloct = (foldr(\y -> if((fst y)==x) then (:)(snd y) else (++)[]) [] (Map.elems absloct))!!0

-- | New Data type for always getting with the Correspondence Table during iteration
-- | Was needed so the Correspondence Table is getting accumulated during building up the new SG.
data StateGraphCorr = StateGraphCorr {nodesSG::NodesSG, successorsSG::SuccessorsSG, nodelabelsSG::NodeLabelSG, edgelabelsSG::EdgeLabelSG, iu::Iu, corr::CorrMap,naddr::NAddrNodeMap}
                 deriving(Eq,Show,Ord)

-- | Get functions to get all the nodes inside this new transition datatype.
getNodesSGCorr (StateGraphCorr x _ _ _ _ _ _) = x
getSuccsSGCorr (StateGraphCorr _ x _ _ _ _ _) = x
getNLabelsSGCorr (StateGraphCorr _ _ x _ _ _ _) = x
getELabelsSGCorr (StateGraphCorr _ _ _ x _ _ _) = x
getIuSGCorr (StateGraphCorr _ _ _ _ x _ _) = x
getCorrSG (StateGraphCorr _ _ _ _ _ x _) = x
getNAddrSGCorr (StateGraphCorr _ _ _ _ _ _ x) = x

-- | Merges two StateGraphs with the CorrMap as it last entry. Same as with usual stategraphs.
mergeStateGraphsCorr :: StateGraphCorr -> StateGraphCorr -> StateGraphCorr
mergeStateGraphsCorr sg1 sg2 = sg3
  where sg3 = StateGraphCorr sg3nodes sg3succs sg3nlabels sg3elabels sg3ius sg3cms sg3nadd 
        sg3nodes = List.nub(List.union (getNodesSGCorr sg1) (getNodesSGCorr sg2))
        sg3succs = Map.union (getSuccsSGCorr sg1) (getSuccsSGCorr sg2)
        sg3nlabels = Map.union (getNLabelsSGCorr sg1) (getNLabelsSGCorr sg2)
        sg3elabels = Map.union (getELabelsSGCorr sg1) (getELabelsSGCorr sg2)
        sg3ius = List.union (getIuSGCorr sg1) (getIuSGCorr sg2)
        sg3cms = Map.union (getCorrSG sg1) (getCorrSG sg2)
        sg3nadd = Map.union (getNAddrSGCorr sg1) (getNAddrSGCorr sg2)

-- | Extracts the StateGraph out of this transition data type StateGraphCorr (CorrMap is dropped)
extractSGofSGCorr sgcorr = sg
 where sg = StateGraph (getNodesSGCorr sgcorr) (getSuccsSGCorr sgcorr) (getNLabelsSGCorr sgcorr) (getELabelsSGCorr sgcorr) (getIuSGCorr sgcorr) 

-- | Takes two states + programm as the input, and then returns the summarized state in the usual "state" format.
-- | Is a wrapper function returning the final state
summarizeStates p s1 s2 = stateGraphToState sg cn mn pc (stid+1) map  
 where sg = sortAndPrepareSG (extractSGofSGCorr sgCorr)
       map = getNAddrSGCorr sgCorr 
       sgCorr = summarize p s1 s2 
       frame = getFirstFrame (getFrames s1)
       cn = getCn frame
       mn = getMn frame
       pc = getPc frame
       stid | (getStateId s1) > (getStateId s2) = ((getStateId s1)+1)
            | (getStateId s1) == (getStateId s2) = ((getStateId s1)+1)
            | (getStateId s2) > (getStateId s1) = ((getStateId s2)+1)

-- | Takes two states + programm as the input, and returns the summarized stategraph.
-- | Is a wrapper function returning the stategraph.
summarizeToSG p s1 s2 = sortAndPrepareSG (extractSGofSGCorr sgCorr)
  where sgCorr = summarize p s1 s2

-- | Summarizes two states to one new abstract state
summarize ::  Prog -> State -> State -> StateGraphCorr
summarize p s1 s2 = (foldr(\x y -> buildSummarization x y) (StateGraphCorr [] Map.empty Map.empty Map.empty [] Map.empty Map.empty) (zip sg1nodesStkReg sg2nodesStkReg))
 where buildSummarization x sg3cm = 
               do 
                  let n1 = (fst x)
                  let n2 = (snd x)
                  let nlist1 = (depthFirstSearch' p n1 (s1,sg1)) 
                  let nlist2 = (depthFirstSearch' p n2 (s2,sg2)) 
                  let dfs1 = getEdgeList nlist1 nlist1 sg1
                  let dfs2 = getEdgeList nlist2 nlist2 sg2 
                  let cm = (getCorrSG sg3cm)
                  let naddr = (getNAddrSGCorr sg3cm)
                  let sg3 = StateGraph (getNodesSGCorr sg3cm) (getSuccsSGCorr sg3cm) (getNLabelsSGCorr sg3cm) (getELabelsSGCorr sg3cm) (getIuSGCorr sg3cm)
                  let sg3' = iterateSum p (s1,sg1) (s2,sg2) sg3 dfs1 dfs2 cm naddr
                  let sg3ret = fst (fst sg3')
                  let cmret = snd (fst sg3')
                  let naddrret = snd sg3'
                  let sg3rek = StateGraphCorr (getNodesSG sg3ret) (getSuccessorsSG sg3ret) (getNLabelsSG sg3ret) (getELabelsSG sg3ret) (getIuSG sg3ret) cmret naddrret
                  (mergeStateGraphsCorr sg3cm sg3rek)

       sg1 = mkStateGraph s1
       sg2 = mkStateGraph s2

       sg1nodes = getNodesSG sg1
       sg1nodesStkReg = foldr(\x -> if((isStkNode x) || (isLocNode x)) then (:)x else (++)[]) [] sg1nodes

       sg2nodes = getNodesSG sg2
       sg2nodesStkReg = foldr(\x -> if((isStkNode x) || (isLocNode x)) then (:)x else (++)[]) [] sg2nodes


-- | We need to keep track of two lists for each of the graphs that we travers, one are the candidate nodes and the other the visited nodes. 
iterateSum :: Prog -> (State,StateGraph) -> (State,StateGraph) -> StateGraph -> [(NodeSG,NodeSG)]-> [(NodeSG,NodeSG)] -> CorrMap -> NAddrNodeMap -> ((StateGraph,CorrMap),NAddrNodeMap)
iterateSum p (s1,sg1) (s2,sg2) sgf [] [] cm nadd= ((sgf,cm),nadd)
iterateSum p (s1,sg1) (s2,sg2) sgf [] ns2 cm nadd = ((sgf,cm),nadd)
iterateSum p (s1,sg1) (s2,sg2) sgf ns1 [] cm nadd = ((sgf,cm),nadd)
iterateSum p (s1,sg1) (s2,sg2) sgf (n1:ns1) (n2:ns2) cm nadd = iterateSum p (s1,sg1) (s2,sg2) sgf' (snd ns1') (snd ns2') corrMap nadd'
  where sgf' = StateGraph node' succs' nlabels' elabels' ius'
        corrMap = cm' fstN1 fstN2
  
        -- List of Edges (outgoing from a stk or loc node in state1)
        ns1' | (isStkNode fstN1) = (False,ns1)
             | (isLocNode fstN1) = (False,ns1)
             | ((not(sndN1 == NullNode)) && (sndN2 == NullNode)) = (True,cutAllSuccEdges p sg1 fstN1 (n1:ns1)) 
             | (isNAddrNode fstN1) && (isNAddrNode fstN2)  = (False,ns1)
             | (((extractNodeLabel s2 sg2 fstN2 absLocTuple2) == Unit) || ((extractNodeLabel s2 sg2 fstN2 absLocTuple2) == Null)) && (isHeapAddrNode fstN1) = (True, cutAllSuccEdges p sg1 fstN1 (n1:ns1))
             | ((isHeapNode fstN1) && (not(isNAddrNode fstN1))) = (False,ns1)
             | (isHeapAddrNode fstN1) && (Char.isLower (head (str1))) = (False,ns1)
             | (((tail (str1)) == (tail (str2))) && (Char.isLower (head (str2)))) = (True,cutAllSuccEdges p sg1 fstN1 (n1:ns1))
             | ((isHeapAddrNode fstN1) && (not(l1'==str1))) = (True,(cutSuccessorList p sg1 l1 fstN1 ns1))
             | otherwise = (False,ns1)
                 where l1 | (Char.isLower (head l1')) = (Char.toUpper (head (l1')):(tail l1'))
                          | otherwise = l1'
                       l1' = getNLabel (getNLabelHG (corrnLabel fstN1 fstN2))
                       l2' = getNLabel (getNLabelHG (corrnLabel sndN1 sndN2))
                       str1 = (nLabelStr s1 sg1 fstN1 absLocTuple1)
                       str2 = (nLabelStr s2 sg2 fstN2 absLocTuple2)

        -- List of Edges (outgoing from a stk or loc node in state2)
        ns2' | (isStkNode fstN2) = (False,ns2)
             | (isLocNode fstN2) = (False,ns2)
             | ((not(sndN2 == NullNode)) && (sndN1 == NullNode)) = (True,cutAllSuccEdges p sg2 fstN2 (n2:ns2)) 
             | (isNAddrNode fstN1) || (isNAddrNode fstN2) = (False,ns2)
             | (isNAddrNode fstN1) && (isNAddrNode fstN2) = (False,ns2)
             | (((extractNodeLabel s1 sg1 fstN1 absLocTuple1) == Unit) || ((extractNodeLabel s1 sg1 fstN1 absLocTuple1) == Null)) && (isHeapAddrNode fstN2) = (True, cutAllSuccEdges p sg2 fstN2 (n2:ns2))
             | ((isHeapNode fstN2) && (not(isHeapAddrNode fstN2))) = (False,ns2)
             | (isHeapAddrNode fstN2) && (Char.isLower (head (str2))) = (False,ns2)
             | (((tail (str1)) == (tail (str2))) && (Char.isLower (head (str1)))) = (True,cutAllSuccEdges p sg2 fstN2 (n2:ns2))
             | ((isHeapAddrNode fstN2) && (not (l1'==str2))) = (True,(cutSuccessorList p sg2 l1 fstN2 ns2))
             | otherwise = (False,ns2)
                 where l1 | (Char.isLower (head l1')) = (Char.toUpper (head (l1')):(tail l1'))
                          | otherwise = l1'
                       l1' = getNLabel (getNLabelHG (corrnLabel fstN1 fstN2))
                       l2' = getNLabel (getNLabelHG (corrnLabel sndN1 sndN2))
                       str1 = (nLabelStr s1 sg1 fstN1 absLocTuple1)
                       str2 = (nLabelStr s2 sg2 fstN2 absLocTuple2)

        -- Decides wether it shall put a duple into the non-address node map
        nadd' | (isNAddrNode node) =  
                  if (absValBinRel p (extractNodeLabel s1 sg1 fstN1 absLocTuple1) (extractNodeLabel s2 sg2 fstN2 absLocTuple2))
                  then addTonAddrNodeMap node (AbsNode (extractNodeLabel s2 sg2 fstN2 absLocTuple2)) nadd
                  else addTonAddrNodeMap node (AbsNode (extractNodeLabel s1 sg1 fstN1 absLocTuple1)) nadd
              | otherwise = nadd
            where node = (corrNode fstN1 fstN2 (fst possAddresses))

        -- Builds up the correspondence table
        cm' x y = addToCorrMap (x,y) (corrNode x y (fst possAddresses)) cm

        possAddresses = retNewAddr s1 s2 sgf -- returns tuple of two possible new addresses 
        possNAddr = retNewNAddrNode nadd 

        -- Adds a new node for the new stategraph
        node' 
          | (List.elem n1' nodesBef) = nodesBef
          | otherwise = nodesBef++[n1'] 
                  where nodesBef = (getNodesSG sgf)
                        n1' = (corrNode fstN1 fstN2 (fst possAddresses))

        -- Adds the new node into the successor relation
        succs' 
               | (Map.member n1' succsBef) = Map.insert n1' (n2':(succsBef Map.! n1')) succsBef
               | otherwise = Map.insert n1' [n2'] succsBef
                   where succsBef = (getSuccessorsSG sgf)
                         n1' = (corrNode fstN1 fstN2 (fst possAddresses))
                         n2' |(sndN1 == NullNode) = NullNode
                             |(sndN2 == NullNode) = NullNode
                             | (isLocNode n1') = (corrNode sndN1 sndN2 (fst possAddresses))
                             | (isStkNode n1') = (corrNode sndN1 sndN2 (fst possAddresses))
                             | otherwise = if ((getAddress(getAddrNode (getHeapNode n1')))==(fst possAddresses))
                                           then (corrNode sndN1 sndN2 (snd possAddresses))
                                           else (corrNode sndN1 sndN2 (fst possAddresses)) 
                         

        -- Adds the node label
        nlabels' | (List.elem n1' nodesBef) && (not (Map.member n1' nlabelsBef)) =  Map.insert n1' l1 nlabelsBef
                 | otherwise = Map.insert n1' l1 nlabelsBef
                    where nlabelsBef = (getNLabelsSG sgf)
                          nodesBef = (getNodesSG sgf)
                          n1' = (corrNode fstN1 fstN2 (fst possAddresses))
                          l1 = (corrnLabel fstN1 fstN2)

        -- Adds the edge label 
        elabels' | (isLocNode n1') = elabelsBef
                 | (isStkNode n1') = elabelsBef
                 | (n2' == NullNode) = elabelsBef
                 | ((getELabelsSG sg1) Map.! (fstN1,sndN1)) == ((getELabelsSG sg2) Map.! (fstN2,sndN2)) = Map.insert (n1',n2') elabel elabelsBef
                 | otherwise = elabelsBef
                      where nodesBef = (getNodesSG sgf)
                            elabelsBef = (getELabelsSG sgf)
                            n1' = (corrNode fstN1 fstN2 (fst possAddresses))
                            n2' 
                             |(sndN1 == NullNode) = NullNode
                             |(sndN2 == NullNode) = NullNode
                             | (isLocNode n1') = (corrNode sndN1 sndN2 (fst possAddresses))
                             | (isStkNode n1') = (corrNode sndN1 sndN2 (fst possAddresses))
                             | otherwise=  if ((getAddress(getAddrNode (getHeapNode n1')))==(fst possAddresses))
                                           then (corrNode sndN1 sndN2 (snd possAddresses))
                                           else (corrNode sndN1 sndN2 (fst possAddresses))
                            elabel = (correLabel fstN2 sndN2)

        -- Adds an "is unshared" entry if needed
        ius' | (isLocNode n1') = iusBef
             | (isStkNode n1') = iusBef
             | (isNAddrNode n1') || (isNAddrNode n2') = iusBef
             | (isHeapAddrNode n1') && (isHeapAddrNode n2') = 
                           if((List.elem (v,v') iusS1) && (List.elem (w,w') iusS2))
                           then (u,u'):iusBef
                           else iusBef 
             | otherwise = iusBef
                  where iusS1 = getIuSG sg1
                        iusS2 = getIuSG sg2
                        iusBef = getIuSG sgf
                        u = getAddrNode (getHeapNode n1')
                        u' = getAddrNode (getHeapNode n2')
                        v = getAddrNode (getHeapNode fstN1)
                        v' = getAddrNode (getHeapNode sndN1)
                        w = getAddrNode (getHeapNode fstN2)
                        w' = getAddrNode (getHeapNode sndN2)
                        n1' = (corrNode fstN1 fstN2 (fst possAddresses))
                        n2'
                         |(sndN1 == NullNode) = NullNode
                         |(sndN2 == NullNode) = NullNode
                         | (isLocNode n1') = (corrNode sndN1 sndN2 (fst possAddresses))
                         | (isStkNode n1') = (corrNode sndN1 sndN2 (fst possAddresses))
                         | otherwise=  if ((getAddress(getAddrNode (getHeapNode n1')))==(fst possAddresses))
                                       then (corrNode sndN1 sndN2 (snd possAddresses))
                                       else (corrNode sndN1 sndN2 (fst possAddresses))

        fstN1 = (fst n1)
        sndN1 = (snd n1)
        fstN2 = (fst n2)
        sndN2 = (snd n2)


        --absValBinRel p a1 a2. a2 is subclass of a1. (both abstract). 

        corrNode x y n
           | (List.elem (x,y) (Map.keys cm)) = (cm Map.! (x,y))
           | ((isStkNode x) && (isStkNode y)) = x
           | ((isLocNode x) && (isLocNode y)) = x
           | (((extractNodeLabel s1 sg1 x absLocTuple1) == Unit) || ((extractNodeLabel s1 sg1 x absLocTuple1) == Null)) && (isHeapAddrNode y) = HeapNode (AddrNode (Addr n))
           | (((extractNodeLabel s2 sg2 y absLocTuple2) == Unit) || ((extractNodeLabel s2 sg2 y absLocTuple2) == Null)) && (isHeapAddrNode x) = HeapNode (AddrNode (Addr n))
--           | ((isNAddrNode x) && (isHeapNode y)) = heapNode (s1,sg1) (s2,sg2) x y n sgf
--           | ((isHeapNode x) && (isNAddrNode y)) = heapNode (s1,sg1) (s2,sg2) x y n sgf
           | ((isNAddrNode x) || (isNAddrNode y)) =
              if (absValBinRel p (extractNodeLabel s1 sg1 x absLocTuple1) (extractNodeLabel s2 sg2 y absLocTuple2))
              then possNAddr 
              else possNAddr
           | (isHeapAddrNode x) && (isHeapAddrNode y) = heapNode (s1,sg1) (s2,sg2) x y n sgf

        corrnLabel x y
          | (List.elem (x,y) (Map.keys cm)) = ((getNLabelsSG sgf) Map.! (cm Map.! (x,y))) 
          | ((isStkNode x) && (isStkNode y)) = ((getNLabelsSG sg1) Map.! x)
          | ((isLocNode x) && (isLocNode y)) = ((getNLabelsSG sg1) Map.! x)
          | (((extractNodeLabel s1 sg1 x absLocTuple1) == Unit) || ((extractNodeLabel s1 sg1 x absLocTuple1) == Null)) && (isHeapAddrNode y) = ((getNLabelsSG sg2) Map.! y)
          | (((extractNodeLabel s2 sg2 y absLocTuple2) == Unit) || ((extractNodeLabel s2 sg2 y absLocTuple2) == Null)) && (isHeapAddrNode x) = ((getNLabelsSG sg1) Map.! x)
--          | ((isNAddrNode x) && (isHeapNode y)) = heapLabel p (s1,sg1) (s2,sg2) x y sgf
--          | ((isHeapNode x) && (isNAddrNode y)) = heapLabel p (s1,sg1) (s2,sg2) x y sgf
          | (isNAddrNode x) || (isNAddrNode y) =
              if(absValBinRel p (extractNodeLabel s1 sg1 x absLocTuple1) (extractNodeLabel s2 sg2 y absLocTuple2))
              then ((getNLabelsSG sg2) Map.! y)
              else ((getNLabelsSG sg1) Map.! x)
          | ((isHeapNode x) && (isHeapNode y)) = heapLabel p (s1,sg1) (s2,sg2) x y sgf



        correLabel x y = (getELabelsSG sg2) Map.! (x,y)

        absLocTuple1 = buildAbsLocTupleSG s1
        absLocTuple2 = buildAbsLocTupleSG s2       
 

-- | Checks wether Nodex or Nodey are already corresponded two.
caseDistCorrMap :: StateGraph -> NodeSG -> NodeSG -> String
caseDistCorrMap sgf x y
     | ((not(List.elem x (getNodesSG sgf)) && (not(List.elem y (getNodesSG sgf))))) = "one" 
     | ((List.elem x (getNodesSG sgf)) && (not(List.elem y (getNodesSG sgf)))) = "two"
     | ((not(List.elem x (getNodesSG sgf)) && (List.elem y (getNodesSG sgf)))) = "three"
     | ((List.elem x (getNodesSG sgf)) && (List.elem y (getNodesSG sgf))) = "four"


-- | Returns the summarized heap node
heapNode (s1,sg1) (s2,sg2) x y n sgf
    | ((extractNodeLabel s1 sg1 x absLocTuple1) == (extractNodeLabel s2 sg2 y absLocTuple2)) = HeapNode (AddrNode (Addr n))
    | (((tail (str1 x)) == (tail (str2 y))) && (Char.isLower (head (str1 x)))) =
                  if((not(List.elem x (getNodesSG sgf))))
                  then x
                  else HeapNode (AddrNode (Addr n))
    | (((tail (str1 x)) == (tail (str2 y))) && (Char.isLower (head (str2 y)))) =
                  if((not(List.elem y (getNodesSG sgf))))
                  then y
                  else  HeapNode (AddrNode (Addr n)) 
    | otherwise = HeapNode (AddrNode (Addr n))
 where absLocTuple1 = buildAbsLocTupleSG s1
       absLocTuple2 = buildAbsLocTupleSG s2

       str1 x= (nLabelStr s1 sg1 x absLocTuple1)
       str2 y= (nLabelStr s2 sg2 y absLocTuple2)

-- | Returns the summarized heapLabel
heapLabel p (s1,sg1) (s2,sg2) x y sgf
    | ((extractNodeLabel s1 sg1 x absLocTuple1) == (extractNodeLabel s2 sg2 y absLocTuple2)) = ((getNLabelsSG sg1) Map.!x)
    | (((tail (str1 x)) == (tail (str2 y))) && (Char.isLower (head (str1 x))) && (Char.isLower (head (str2 y)))) = (getNLabelsSG sg1) Map.! x 
    | (((tail (str1 x)) == (tail (str2 y))) && (Char.isLower (head (str1 x)))) = (getNLabelsSG sg1) Map.! x
    | (((tail (str1 x)) == (tail (str2 y))) && (Char.isLower (head (str2 y)))) = (getNLabelsSG sg2) Map.! y
    | ((not(((tail (str1 x)) == (tail (str2 y))))) && (Char.isLower (head (str1 x))) && (Char.isLower (head (str2 y)))) = NLabelHG (NLabel cn)
    | ((not(((tail (str1 x)) == (tail (str2 y))))) && (Char.isLower (head (str1 x)))) = NLabelHG (NLabel cn)
    | ((not(((tail (str1 x)) == (tail (str2 y))))) && (Char.isLower (head (str2 y)))) = NLabelHG (NLabel cn)
    | otherwise = NLabelHG (NLabel (searchUpwards p cn1 cn2))   
 where cn = (Char.toLower (head (cn')):(tail cn'))
       cn' = searchUpwards p cn1 cn2
                 
       absLocTuple1 = buildAbsLocTupleSG s1
       absLocTuple2 = buildAbsLocTupleSG s2

       cn1 = getAbsClassVar2 var1 "line 361"
              where var1 =(extractNodeLabel s1 sg1 x absLocTuple1)
       cn2 = getAbsClassVar2 var2 "line 363"
              where var2=(extractNodeLabel s2 sg2 y absLocTuple2)

       str1 x= (nLabelStr s1 sg1 x absLocTuple1)
       str2 y= (nLabelStr s2 sg2 y absLocTuple2)


-- | Cuts the successorList according to the new father class. 
-- | The function purges all edges that have a label (fieldname) not in cn. 
cutSuccessorList p sg cn node list = foldr(\x y -> removeSuccEdges x y) list edges 
 where  edges = filterEdges p node sg cn list

-- | Cuts all successing Edges from a given node.
-- | This is e.g needed when you want to summarize one abstract node with one concrete one. 
-- | All successing nodes of the concrete one needs to be deleted in order to keep the right format
cutAllSuccEdges p sg node list = foldr(\x y -> removeSuccEdges x y) list edges
 where  edges = foldr(\x -> if((fst x) == node) then (:)x else (++)[]) [] list
         
-- | Filters all edges from the list of edges, that are not element of the field declarations within the ClassDecl of Class "cn".
filterEdges p node sg cn elist = foldr(\x -> filterEdges x) [] elist
                 where filterEdges x  | ((fst x) == node) && ((snd x) == NullNode) = (++)[]
                                      | (not (Map.member x (getELabelsSG sg) )) = (++)[]
                                      | ((not(List.elem (FDecl (getELabel (getNLabelHG ((getELabelsSG sg) Map.! x)))) fieldsNew)) && ((fst x) ==node)) = (:)x
                                      | otherwise = (++)[]
                       fieldsNew = getFDeclarations p cn     -- is a list of (Cn,Fn)

-- | Removes any successive edges from a given Edge.  
removeSuccEdges (node,succ) ls = (remoSuccessingEdges' succ ls')
    where
        ls' = filter f ls
        f (a,b) 
            | a == node && b == succ = False
            | otherwise = True

remoSuccessingEdges' y ls 
    | ys == ls = ls
    | otherwise = foldr (\a b -> remoSuccessingEdges' a b) ys xs  
    where
        (xs,ys) = loop y ls [] []

loop _ [] xs ys = (xs,ys)
loop k (i@(x,y):is) xs ys 
    | k == x = loop k is (y:xs) ys
    | otherwise = loop k is xs (i:ys)


-- | Depth first traversal of a graph. Returns the list of visited nodes in dfs manner
depthFirstSearch' :: Prog -> NodeSG -> (State,StateGraph) -> NodesSG
depthFirstSearch' p start (s,sg) = reverse (dfs [start] [])
 where 
    dfs [] vis = vis 
    dfs (c:cs) vis 
       | (List.elem c vis) = dfs cs vis
       | otherwise = dfs ((succs' c)++cs) (c:vis)
                      where heap = getHeap (getHeaps s)
                            succs' x | (isLocNode x) = ((getSuccessorsSG sg) Map.! c)
                                     | (isStkNode x) = ((getSuccessorsSG sg) Map.! c)
                                     | (isNAddrNode x) = ((getSuccessorsSG sg) Map.! c)
                                     | ((getSuccessorsSG sg) Map.! c) == [] = ((getSuccessorsSG sg) Map.! c)
                                     | (isHeapNode x) = 
                                             do 
                                               let curnode = x
                                               let successors = ((getSuccessorsSG sg) Map.! curnode)
                                               let elabels = (getELabelsSG sg)
                                               -- abstract to the elabels reachable from curnode
                                               let elabels' = foldr(\y -> if((fst y) == curnode) then Map.insert y (elabels Map.! y) else Map.union Map.empty) Map.empty (Map.keys elabels) 
                                               -- inverse them to can lookup
                                               let invelabels = foldr(\y -> Map.insert (elabels Map.! y) y) Map.empty (Map.keys elabels')
                                               -- get the class name to make a lookup of the fields
                                               let cn = fst(getObjPair (heap Map.! (getAddrNode (getHeapNode curnode))))
                                               -- get the field declarations for that object
                                               let fdecls = foldr(\y -> (:)y) [] (getFDecl(getFDeclarations p cn))
                                               -- insert them into the correct order
                                               foldr(\y -> if(not(Map.member (NLabelHG (ELabel y)) invelabels)) then (++)[] else (:)(snd(invelabels Map.! (NLabelHG (ELabel y))))) [] fdecls
           

-- | Returns the List of edges coming out of the list of nodes (output from deptfirstsearch.
getEdgeList :: NodesSG -> NodesSG -> StateGraph -> [(NodeSG,NodeSG)]
getEdgeList nlist ns sg = reverse (findEdges ns [])
  where 
    findEdges [] eLst = eLst
    findEdges (n:ns) eLst | ((not(eLst == [])) && (snd (head eLst))== NullNode) = findEdges (n:ns) (((getPredecessor n (getSuccessorsSG sg) nlist),n):eLst)
                          | (List.elem (head ns) ((getSuccessorsSG sg) Map.! n)) = findEdges ns ((n,(head ns)):eLst)
                          | (((getSuccessorsSG sg) Map.! n) == []) = findEdges ns ((n,NullNode):eLst)
                          | (snd (head eLst)) == NullNode = findEdges ns (((getPredecessor n (getSuccessorsSG sg) nlist),n):eLst)

-- | Returns a duple of two possible new addresses. (one for curNode and one for curSucc).
-- | Determines the highest number of address from the two given states plus intermediate stategraph getting build during summarization
retNewAddr :: State -> State -> StateGraph -> (Int,Int)
retNewAddr s1 s2 sg3 = maxRet
 where maxRet | (maxsg3==max) = ((max+1),(max+2))
              | (maxsg3 > max) = ((maxsg3+1),(maxsg3+2))
              | otherwise = ((max+1),(max+2))
       maxsg3 
         | (nlist == []) = 0
         | otherwise = last(nlist)
            where nlist = List.sort(foldr(\x -> if(isHeapAddrNode x) then (:)(getAddress(getAddrNode (getHeapNode x))) else (++)[]) [] (getNodesSG sg3))
       maxh1 
         | (nlist== []) = 0
         | otherwise = last(nlist)
            where nlist = List.sort(foldr(\x -> (:)(getAddress x)) [] (Map.keys (getHeap (getHeaps s1))))
       maxh2 
         | (nlist== []) =0
         | otherwise = last(nlist)
            where nlist = List.sort(foldr(\x -> (:)(getAddress x)) [] (Map.keys (getHeap (getHeaps s2))))
       max | (maxh1 == maxh2) = maxh1
           | (maxh1 > maxh2) =  maxh1
           | otherwise = maxh2
