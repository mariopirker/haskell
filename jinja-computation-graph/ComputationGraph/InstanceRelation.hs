module ComputationGraph.InstanceRelation where

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

-- | Definition 4.7, determines wether a state s1 is instance of a state s2
instanceof :: Prog -> State -> State -> Bool
instanceof p s1 s2 = finalResult 
  where frmss1 = getFrames s1
        frmss2 = getFrames s2

        finalResult = pcsresult && stklocresult && (fst morphresult) && iuresult 
        pcsresult = checkPcs s1 s2
        stklocresult = if(pcsresult == False) then False else (checkStkLocs s1 s2)
        morphresult = if(stklocresult == False) then (False,Map.empty) else (checkMorph p s2 s1) -- is there a morphism from the abstract state to the instance?
        iuresult = if((fst morphresult == False)) then False else (checkIuElems p s2 s1 morphism)

        morphism = snd morphresult 

-- | Point1 of the Definition 4.7 
-- | Checks the pc's for two given list of frames
checkPcs :: State -> State -> Bool
checkPcs state1 state2 = List.and(foldr (\x -> (:)((getPc(fst x)) == (getPc(snd x)))) [] frmszipped)
  where frmszipped = zip frmss1 frmss2 
        frmss1 = getFrames state1
        frmss2 = getFrames state2

-- | Point2 of the Definition 4.7
-- | Checks stk's and loc's of two given list of frames
checkStkLocs :: State -> State -> Bool
checkStkLocs state1 state2 = List.and(foldr (\x -> (:)((checkStk x) && (checkLoc x))) [] frmszipped)
  where frmszipped = zip frmss1 frmss2 
        lengthStk x = length x
        checkStk x = (lengthStk (getStk (fst x)) == lengthStk (getStk (snd x)))

        lengthLoc x = length x
        checkLoc x = (lengthLoc (getLoc (fst x)) == lengthLoc (getLoc (snd x)))

        frmss1 = getFrames state1
        frmss2 = getFrames state2

-- | Point3 of the Definition 4.7
-- | Checks wether there exists a morphism from s1 to s2
checkMorph :: Prog -> State -> State -> (Bool,Morphism)
checkMorph p s1 s2 = checkMorphism p (s1,sg1) (s2,sg2)
  where sg1 = mkStateGraph s1
        sg2 = mkStateGraph s2

-- | Point4 of the Definition 4.7.
-- | Checks for all addresses from the annotations
checkIuElems :: Prog -> State -> State -> Morphism -> Bool
checkIuElems p s1 s2 m = (List.and(foldr (\x -> chMorph x) [] iu)) && iuSubSet
  where iu = getAnnotations s1

        iu'= foldr (\x -> (:) ((HeapNode (AddrNode (fst x))),(HeapNode (AddrNode (snd x))))) [] (getAnnotations s1)
        chMorph x = (:)((Map.findWithDefault (error ("line 58 1")) (HeapNode (AddrNode (fst x))) m) /= (Map.findWithDefault (error "line 58 2") (HeapNode (AddrNode (snd x))) m))

        iuMorph = (foldr(\x -> (:)((Map.findWithDefault (error ((show m)++"NODE:"++(show (fst x)))) (fst x) m),(Map.findWithDefault (error "line 60 2") (snd x) m))) [] iu')
        iuSubSet = List.and(foldr (\x -> (:)(List.elem ((HeapNode (AddrNode (fst x))),(HeapNode (AddrNode (snd x)))) iuMorph)) [] (getAnnotations s2)) 
