-- file: ComputationGraph/Unification.hs
-- author: Mario Pirker
-- This module provides the function for testing wether two StateGraphs unify
module ComputationGraph.Unification where

import qualified Data.Map as Map
import qualified Data.List as List
import ComputationGraph.HeapGraph
import ComputationGraph.StateGraph
import ComputationGraph.Program
import ComputationGraph.State
import ComputationGraph.HelperFunctions

-- | Determines wether two given addresses are unifiable or not
-- | Input: (Prog,State) and the two Addresses
-- | Output: True or False
unifiable :: (Prog,State) -> NodeSG -> NodeSG -> Bool
unifiable (prog,state) node1 node2 | not ((isHeapAddrNode node1) && (isHeapAddrNode node2)) = False
                                   | (List.elem ((getAddrNode (getHeapNode node1)), (getAddrNode (getHeapNode node2))) (getAnnotations state)) = False 
                                   | (List.elem ((getAddrNode (getHeapNode node2)), (getAddrNode (getHeapNode node1))) (getAnnotations state)) = False
                                   | not (occursCheck state (getAddrNode (getHeapNode node2)) (getAddrNode (getHeapNode node1))) = False
                                   | not (occursCheck state (getAddrNode (getHeapNode node1)) (getAddrNode (getHeapNode node2))) = False
                                   | otherwise = unifiable' node1 node2 
  where unifiable' node1 node2 = addresses && edges && succs
        addresses = compareRoots (prog,state) node1 node2
        edges = if(addresses==False)then False else (compareEdges (prog,state) node1 node2) 
        succs = if(edges==False) then False else (compareSuccessors (prog,state) node1 node2)

-- | Checks the binary relation on two given address for all cases and returns true if it holds.
-- | Input: (Prog,State) and the two Address Nodes
-- | Output: True or False 
compareRoots :: (Prog,State) -> NodeSG -> NodeSG -> Bool
compareRoots (p,state) node1 node2 | (cond1 node1 node2) = checkAbstractVars
                                   | (cond2 node1 node2) = check node1 node2
                                   | (cond3 node1 node2) = check node2 node1
                                   | (cond4 node1 node2) = checkConcreteObjects
                                   | otherwise = False
  where checkAbstractVars = (absValBinRel p (absval1 node1) (absval1 node2))||(absValBinRel p (absval1 node2) (absval1 node1))  
        checkConcreteObjects = ((fst (getObjPair (heap Map.! (getAddrNode (getHeapNode node1))))) == (fst (getObjPair (heap Map.! (getAddrNode (getHeapNode node2))))))
        check n1 n2 = absValBinRel p (absval1 n1) (absval2 n2)
        heap = curHeap (getHeaps state)
        
        -- Two classvars
        cond1 x y = (isClassVar (heap Map.! (getAddrNode (getHeapNode x)))) && (isClassVar (heap Map.! (getAddrNode (getHeapNode y)))) 
        -- Address one is abstract, Address two is concrete
        cond2 x y = (isClassVar (heap Map.! (getAddrNode (getHeapNode x)))) && (not (isClassVar (heap Map.! (getAddrNode (getHeapNode y)))))
        -- Address one is conrete, Address two abstract
        cond3 x y = ((not (isClassVar (heap Map.! (getAddrNode (getHeapNode x))))) && (isClassVar (heap Map.! (getAddrNode (getHeapNode y)))))
        -- Address one is abstract, Address to is abstract
        cond4 x y = ((not (isClassVar (heap Map.! (getAddrNode (getHeapNode x))))) && (not (isClassVar (heap Map.! (getAddrNode (getHeapNode y))))))

        absval1 x = AbsClassVar (getClassVar (heap Map.! (getAddrNode (getHeapNode x))))
        absval2 x = AbsClassVar (fst (getObjPair (heap Map.! (getAddrNode (getHeapNode x)))))

        scr = buildSubClassRelation p

-- | Compares the Edge Labels of two addresses. 
compareEdges :: (Prog,State) -> NodeSG -> NodeSG -> Bool
compareEdges (p,state) node1 node2 | (isClassVar (heap Map.! (getAddrNode (getHeapNode node1)))) && (isClassVar (heap Map.! (getAddrNode (getHeapNode node2)))) = True
				   | ((not (isClassVar (heap Map.! (getAddrNode (getHeapNode node1))))) && (isClassVar (heap Map.! (getAddrNode (getHeapNode node2))))) = False
--                                   | ((not (isClassVar (heap Map.! (getAddrNode (getHeapNode node1))))) && (isClassVar (heap Map.! (getAddrNode (getHeapNode node2))))) = True
--                                   | (isClassVar (heap Map.! (getAddrNode (getHeapNode node1)))) && (not (isClassVar (heap Map.! (getAddrNode (getHeapNode node2))))) = True
                                   | (isClassVar (heap Map.! (getAddrNode (getHeapNode node1)))) && (not (isClassVar (heap Map.! (getAddrNode (getHeapNode node2))))) = False
                                   | ((not (isClassVar (heap Map.! (getAddrNode (getHeapNode node1))))) && (not (isClassVar (heap Map.! (getAddrNode (getHeapNode node2)))))) = checkELabels node1 node2
                                   | otherwise = False
  where checkELabels n1 n2 = if((not ((getEdgeLabelsFromAddr state n1)==(getEdgeLabelsFromAddr state n2))))then False else True 
        heap = curHeap (getHeaps state)

-- | Compares the successos 
compareSuccessors :: (Prog,State) -> NodeSG -> NodeSG -> Bool
compareSuccessors (p,state) node1 node2 | (isClassVar (heap Map.! (getAddrNode (getHeapNode node1)))) && (isClassVar (heap Map.! (getAddrNode (getHeapNode node2)))) = True
                                        | ((not (isClassVar (heap Map.! (getAddrNode (getHeapNode node1))))) && (isClassVar (heap Map.! (getAddrNode (getHeapNode node2))))) = True
                                        | (isClassVar (heap Map.! (getAddrNode (getHeapNode node1)))) && (not (isClassVar (heap Map.! (getAddrNode (getHeapNode node2))))) = True
                                        | ((not (isClassVar (heap Map.! (getAddrNode (getHeapNode node1))))) && (not (isClassVar (heap Map.! (getAddrNode (getHeapNode node2)))))) = List.and (foldr(\x -> checkInstanceRelation x) [] zipped)
                                        | otherwise = False 
  where checkInstanceRelation x = case (f x) of 
                                   (0,False) -> (:)((absValBinRel p (val (fst x)) (val (snd x))) || (absValBinRel p (val (snd x)) (val (fst x))))
                                   (1,False) -> (:)((absValBinRel p (val (fst x)) (val (snd x))) || (absValBinRel p (val (snd x)) (val (fst x))))
                                   (2,False) -> (:)((absValBinRel p (val (fst x)) (val (snd x))) || (absValBinRel p (val (snd x)) (val (fst x))))
                                   (1,True) ->  (:)(unifiable (p,state) (fst x) (snd x))

        val node | isNAddrNode node = (Map.fromList (Map.elems abs)) Map.! (getNAddrNodeSG node)
                 | otherwise = if(isClassVar (heap Map.! (getAddrNode(getHeapNode node)))) then AbsClassVar (getClassVar (heap Map.! (getAddrNode (getHeapNode node)))) else AbsClassVar (fst (getObjPair (heap Map.! (getAddrNode (getHeapNode node)))))
        successorsn1  = getAddrNodeSuccs' state node1
        successorsn2  = getAddrNodeSuccs' state node2

        zipped = zip successorsn1 successorsn2
        heap = curHeap (getHeaps state)
        abs = (buildAbsLocTupleSG state)


getAddrNodeSuccs' :: State -> NodeSG -> NodesSG
getAddrNodeSuccs'  state node = foldr (\y -> insert y) [] (elabels)
  where insert y = do 
                     let curlabel = getELabel (getNLabelHG y)
                     let ret = if (isAddress (ftable Map.! curlabel))
                               then (HeapNode (AddrNode(getAddVal (ftable Map.! curlabel))))
                               else (NAddrNode (getAbsLocOfAbsLocTuple absLocTuple (HeapPos (getAddrNode (getHeapNode node),fst curlabel, snd curlabel))))
                     (:)ret
        ftable = ft(getObjPair(heap Map.! (getAddrNode (getHeapNode node))))
        elabels = getEdgeLabelsFromAddr state node
        absLocTuple = buildAbsLocTupleSG state 
        heap = curHeap (getHeaps state) 

-- | Checks wether a duple of nodes consit of two address nodes
-- | If one of the nodes is an address and the other an abstract value then false
-- | If both are abstract values then check the instance relation in bot
f :: (NodeSG, NodeSG) -> (Int, Bool)
f x | (isNAddrNode (fst x)) || (isNAddrNode (snd x)) = (1,False)
    |  ((isAddrNode (getHeapNode (fst x))) && (not (isAddrNode (getHeapNode(snd x))))) = (1,False) 
    |  ((not (isAddrNode (getHeapNode(fst x)))) && (isAddrNode (getHeapNode (snd x)))) = (2,False)
    |  ((isAddrNode (getHeapNode(fst x))) && (isAddrNode (getHeapNode(snd x)))) = (1, True)
    |  otherwise = (0,False)

-- | Returns the Edges itself going out of the given address.
getEdgesFromAddr :: State -> NodeSG -> [EdgeSG]
getEdgesFromAddr s n1 = eLabels n1
  where sg = mkStateGraph s
        el = getELabelsSG sg
        eLabels n1 = foldr(\x -> if((fst x) == n1)then(:)x else (++) [])[] (Map.keys el)


-- | Tries to unify address a with all addresses from the heap and returns result
putfieldCheck :: Prog -> State -> Address -> [(Bool,(Address,Address))]
putfieldCheck prog state a = uncheck
 where uncheck =  foldr (\x -> unicheck x) [] addrs
         where unicheck x = (:)((unifiable (prog,state) (HeapNode (AddrNode a)) (HeapNode (AddrNode x))),(a,x))
       addrs = removeItem a (Map.keys h)
       h = curHeap (getHeaps state)

-- | Returns False if occurs check failed
-- | Returns True if occurs check was okay
occursCheck :: State -> Address -> Address -> Bool
occursCheck state a1 a2 = not(isReachable state a1 (AddVal a2))
