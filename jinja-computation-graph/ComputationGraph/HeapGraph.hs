-- file: HeapGraph.hs
-- represents the heap as a graph

module ComputationGraph.HeapGraph where

import qualified Data.Map as Map
import qualified Data.List as List
import ComputationGraph.State
import ComputationGraph.HelperFunctions

{- ################ Definition of the heap graph ################ -}

-- | Nodes. Set of Nodes
type NodesHG = [NodeHG]

-- | A Node can either be of type Address Node, NonAddress Node (for non address values - aka Abstract Jinja Values in the State)
data NodeHG = AddrNode Address
          | NAddrNodeHG AbsLoc
          | SuccNodeAbsVal AbsVal
          | SuccNodeAbsLoc AbsLoc
          deriving(Eq,Show,Ord)

-- | Returns the Value stored at the SuccNodeAbsVal. If it is not of that type it returns an error
getSuccNodeAbsVal (SuccNodeAbsVal a) = a
getSuccNodeAbsVal _                     = error "Node not an SuccNode AbsVal"

-- | Returns the Value stored at the SuccNodeAbsLoc. If it is not of that type it returns an error
getSuccNodeAbsLoc (SuccNodeAbsLoc a) = a
getSuccNodeAbsLoc _                     = error "Node not an SuccNode AbsLoc"


-- | Label for Edge and the Note
data LabelHG = ELabel (Dn, Fieldid)
             | NLabel String
             | NLabelAbs AbsVal
           deriving(Eq,Show,Ord)

-- | Returns the Edge Label. If it is not of type Edge Label (ClassName,Fieldid) it gives an error
getELabel (ELabel (a,b)) = (a,b)
getELabel _ = error "Value not an Edge Label of type (x,y)"

-- | Returns the Node Label. If it is not of type Node Label (a usual String) it gives an error
getNLabel (NLabel n)    = n
getNLabel _ = error "Value not an Node Label of Type String"

-- | Returns True if the given label is of type NLabel (Node Label)
isNLabel(NLabel n) = True

-- | label sg n: label connected to node n in the graph sg
-- | this causes an error if n is not element of the graph.
nlabelHG :: HeapGraph ->  NodeHG -> LabelHG
nlabelHG hg n = (getNLabelsHG hg) Map.! n

-- | Returns the Node Labels for the Abstract Location. If it is not of that type it returns an error
getNLabelAbs (NLabelAbs a) = a
getNLabelAbs _ = error "Node Label nof type NLabelAbs"

-- | Edge Label in the HeapGraph is a Mapping from an Edge (NodeHG,NodeHG) to a Label
type EdgeLabelHG = Map.Map EdgeHG LabelHG
-- | Node Label in the HeapGraph is a Mapping from a Node to a Label
type NodeLabelHG = Map.Map NodeHG LabelHG
-- | Edge. Tuple of Node and a SuccessorNode
type EdgeHG = (NodeHG, NodeHG)
-- | Successor Relation is a Mapping from a Node to 0 or a list of Successor Nodes of Type NodeHG
type SuccessorsHG = Map.Map NodeHG NodesHG

-- | HeapGraph representation. It consists of nodes, successor relation, nodelabels, edgelabels
data HeapGraph = HeapGraph {nodesHG::NodesHG, successorsHG::SuccessorsHG, nodelabelsHG::NodeLabelHG, edgelabelsHG::EdgeLabelHG} 
                 deriving(Eq,Show,Ord)

-- | Returns the nodes list of a given HeapGraph
getNodesHG :: HeapGraph -> NodesHG
getNodesHG (HeapGraph g _ _ _) = g

-- | Returns the successor relation mapping for a given HeapGraph
getSuccessorsHG :: HeapGraph -> SuccessorsHG
getSuccessorsHG (HeapGraph _ s _ _) = s

-- | Returns the Node Label mapping for a given HeapGraph
getNLabelsHG :: HeapGraph -> NodeLabelHG
getNLabelsHG (HeapGraph _ _ n _) = n

-- | Returns the Edge Label mapping for a given HeapGraph
getELabelsHG :: HeapGraph -> EdgeLabelHG
getELabelsHG (HeapGraph _ _ _ e) = e

-- | Returns the list of successors for a given node in a heapgraph
-- | Input: Node + Successors. 
-- | Output: List of SuccessorNodes.
getSuccessors :: NodeHG -> SuccessorsHG -> NodesHG
getSuccessors n s = s Map.! n

-- | Creates a list of edges for a given node to each successor
-- | Input: Node + Successorrelation
-- | Output: Creates a list of edges for the certain node
createEdges :: NodeHG -> SuccessorsHG -> [EdgeHG]
createEdges n s = if(s == Map.empty)
                  then []
                  else foldr (\x -> (:) (n,x)) [] (s Map.! n) 

-- | Returns True if the node is an Address Node
isAddrNode (AddrNode a) = True
isAddrNode _ = False

-- | Returns the value stored at the AddrNode. If it is not of that type it returns an error.
getAddrNode (AddrNode a) = a
getAddrNode _          = error "Value not an Address Node"

-- | Returns the value stored at the NonAddrNode. If it is not of that type it returns an error.
getNAddrNodeHG (NAddrNodeHG n) = n
getNAddrNodeHG _               = error "Value not an Abstract Location Node"

{- ################ end definition of the heap graph ################ -}


{- ############### Begin functions for the heap graph ############### -}

-- | Function that associates every non-address value in the heap with an abstract location
-- | filter ((isAddress).snd) is filtering on the second elem of the list of tuples
buildAbsLocTupleHG :: Heaps -> AbsLocationTuple
buildAbsLocTupleHG h = final (List.concat list) 
   where final l= Map.fromList (zip (List.concat poslist) (zip [1..] l))
         poslist= foldr (\x -> calc' x) [] (Map.keys (curHeap h))
         calc' x =  if (isClassVar ((curHeap h) Map.! x))
                   then (++)[]
                   else (:)listPos
                    where listPos = foldr (\y -> findPos y) [] (Map.keys ftable)
                             where findPos y = if(isAddress (ftable Map.! y)==False)
                                               then (:)(HeapPos (x,fst y, snd y))
                                               else (++)[]
                          ftable = ft(getObjPair((curHeap h) Map.! x))
                          
         list   = foldr (\x -> calc x) [] (Map.keys (curHeap h)) 
         calc x =  if (isClassVar ((curHeap h) Map.! x))
                   then (++)[]
                   else (:)listNonAddr
                    where listNonAddr = foldr (\y -> calc' y) [] listval
                             where calc' y = if(isAddress y==False)
                                             then (:)y
                                             else (++)[]
                          listval     = (rg(ft(getObjPair((curHeap h) Map.! x))))                  

-- | Returns the value at a given position of the AbsLocationTuple mapping
getAbsValOfAbsLocTuple :: AbsLocationTuple -> AbsLocPos -> AbsVal
getAbsValOfAbsLocTuple absloctuple pos = snd(absloctuple Map.! pos) 

                                
-- | Looks up the abslocation number for a givenn Position.
getAbsLocOfAbsLocTuple :: AbsLocationTuple -> AbsLocPos -> AbsLoc
getAbsLocOfAbsLocTuple absloctuple pos = fst(absloctuple Map.! pos)


-- | Creates the Heap Graph out of a Heap
mkHeapGraph :: Heaps -> HeapGraph
mkHeapGraph h = HeapGraph nodes' successors' nodelabel' edgelabel' 
  where domHeap         = foldr (\x -> (:)(AddrNode x)) [] (dom heap)
        locations       = foldr (\x -> (:)(fst x)) [] (Map.elems absLocTuple)
        absLocTuple     = buildAbsLocTupleHG h
        
        heap            = curHeap h

        nodes'          = domHeap
        
        successors'     = foldr (\x -> succ x) Map.empty nodes'       
          where succ x  = if (isAddrNode x==True)
                          then Map.insert x succnodes 
                          else Map.insert x []
                             where succnodes = foldr (\y -> insert y) [] (Map.keys f)
                                     where insert y = if (isAddress (f Map.! y))
                                                      then (:)(AddrNode(getAddVal (f Map.! y)))
                                                      else (:)(NAddrNodeHG (getAbsLocOfAbsLocTuple absLocTuple (HeapPos (getAddrNode x,fst y, snd y))))


                                   f         = if (isClassVar ((curHeap h) Map.! (getAddrNode x)))
                                               then Map.empty
                                               else ft(getObjPair(heap Map.! (getAddrNode x)))

        nodelabel'     = foldr (\x -> label x) Map.empty nodes'
          where label x= if(isAddrNode x == True)
                         -- if curr node is an address ad the class name as label for node
                         then Map.insert x (NLabel (cl(heap Map.! (getAddrNode x))))
                         -- if its not added then enter absval name as node label
                         else Map.insert x (NLabelAbs (pos!!0))
                          where pos = foldr(\y -> if((fst y)==getNAddrNodeHG x) then (:)(snd y) else (++)[]) [] (Map.elems absLocTuple)
        -- if node is not an address there are no successors according to the definition. 
        edgelabel'     = foldr (\x -> label x) Map.empty domHeap
           where label x = do 
                            let curnode = x 
                            let f    = if (isClassVar ((curHeap h) Map.! (getAddrNode x)))
                                       then Map.empty
                                       else ft(getObjPair(heap Map.! (getAddrNode x)))
                            let ret = foldr (\x -> insert x) Map.empty (Map.keys f)
                                 where insert x = if (isClassVar ((curHeap h) Map.! (getAddrNode curnode))) 
                                                 then Map.union Map.empty
                                                 else Map.insert (curnode, succnode x) (ELabel x)
                                       succnode x = if (isAddress (f Map.! x))
                                                    then (AddrNode(getAddVal (f Map.! x)))
                                                    else (NAddrNodeHG (getAbsLocOfAbsLocTuple absLocTuple (HeapPos (getAddrNode curnode,fst x, snd x))))                             
                            Map.union ret 

