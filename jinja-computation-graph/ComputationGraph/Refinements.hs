-- | file: Computationgraph/Refinements.hs
-- | author: Mario Pirker
-- | Refinement is used when information in abstract states s is not concrete enough to execute a given instruction. 

module ComputationGraph.Refinements where

import ComputationGraph.State
import ComputationGraph.HelperFunctions
import ComputationGraph.Program
import ComputationGraph.ComputationGraph
import ComputationGraph.StateGraph
import qualified Data.Map as Map
import qualified Data.List as List

-- | definition 5.1 refinement if heap at address a is an Abstract Class Variable -- 
-- | Class is replaced by (cn, ftable). ftable((Ci, idi)) = vi where vi elem of  {Int, Bool, Class}
-- | heap(a) = Class. subclasses = {cn | cn is subclass of Class}. cn elem subclasses.
-- | (C1, id1)..(Cn, idn) denote fields of cn.(together with defining classes). 
-- | Can only occur during putfield and getfield command
caseDistinction :: Prog -> State -> Address -> ([State],[Constraint])
caseDistinction prog state addr = (cllist,constraints)
       where subclasses     = (getClassVar obj):sclasses
             sclasses       = if (Map.member (getClassVar obj) scr) then (scr Map.! (getClassVar obj)) else []
             scr            = buildSubClassRelation prog
             obj            = heapBef Map.! addr               
             heapsBef       = getHeaps state                   
             heapBef        = curHeap heapsBef                 
             frameLst       = getFrames state 
             fstFrame = getFirstFrame frameLst
             stateId        = (getStateId state)
             annot = getAnnotations state
  
             nullState      = caseDistNullCase addr state

             stateIds = [(stateId+2)..] 

             nullconstraint = "null" 
             constraints = nullconstraint:constraintlist
             constraintlist = foldr (\x -> (:)(snd x)) [] classlist

             classlist' = foldr (\x -> (:)(fst x)) [] classlist
             cllist         = nullState:classlist'
             subclasses' = zip subclasses stateIds

             classlist = caseDistNotNullCase subclasses' prog state addr 

-- | Returns the Same State as before, just with everything replaced by Null that was addr before.
caseDistNullCase addr state = nullState
  where nullState = (State (Heaps nullHeap addrCr) nullFrameLst annot (stateId+1))
       
        nullHeap = Map.delete addr nullHeap'
        nullHeap' = findAndReplaceAddrFTable heapBef (AddVal addr) Null
        nullFrameLst = foldr (\x -> (:)(findAndReplaceFrame x (AddVal addr) (Null))) [] frameLst
        heapBef = curHeap (getHeaps state)                 
        heapsBef = getHeaps state
        frameLst = getFrames state
        fstFrame = getFirstFrame frameLst
        stateId = (getStateId state)
        annot = getAnnotations state

        addrCr 
         | (nlist== []) = 0
         | otherwise = last(nlist)
            where nlist = List.sort(foldr(\x -> (:)(getAddress x)) [] (Map.keys nullHeap'))

-- | Returns the list of States during class instance refinement
caseDistNotNullCase subclasses' prog state addr = return
               where obj = heapBef Map.! addr
                     frameLst = getFrames state
                     annot = getAnnotations state
                     heapsBef = getHeaps state
                     heapBef = curHeap heapsBef
                     constraints = replicate (length retclasses) "not null"

                     return = zip retclasses constraints 
                     retclasses = foldr(\x y -> (calc x y)) [] subclasses'
                     calc x y = 
                       do
                        let maxAddress' = retNewAddr y -- determines the maximum address number 
                        let maxAddress  = if(maxAddress' == 0)then  (curAddrCounter heapsBef) else maxAddress'
                        let classname = fst x
                        let fields    = getFDecl (getFDeclarations prog classname)
                        let fieldsAddr = foldr(\x -> if((fst x)=="int" || (fst x)=="bool") then (++)[] else (:)x) [] fields
                        let fieldsNAddr = foldr(\x -> if((fst x)=="int" || (fst x)=="bool") then (:)x else (++)[]) [] fields
                        let zipped = zip fieldsAddr [(maxAddress+1)..]
                        let ftable'   = foldr (\x -> calc' x) Map.empty fieldsNAddr
                                     where calc' x | (fst x) == "int" = Map.insert x (AbsIntVal "AbsInt")
                                                   | (fst x) == "bool" = Map.insert x (AbsBoolVal "AbsBool")
                        let ftable'' = foldr (\x -> calc'' x) ftable' zipped
                                     where calc'' x = Map.insert (fst x) (AddVal (Addr (snd x)))
                        let cn1Val x  = if x == obj then Just (ObjPair (classname, ftable'')) else Nothing
                        let heapupdated   =  Map.update cn1Val addr heapBef
                        let heapcni = foldr(\x -> add x) heapupdated (Map.keys ftable'')
                                        where add x = if(isAddVal (ftable'' Map.! x))
                                                      then Map.insert (getAddVal (ftable'' Map.! x)) (AbsVariable (ClassVar (fst x)))
                                                      else Map.union Map.empty
                        let retList = (State (Heaps heapcni (maxAddress+(length fieldsAddr))) frameLst annot (snd x)):y
                        retList


-- | Returns the hightest address within a state of list. 
-- | This is needed during class instance refinement and there are more subclasses
retNewAddr listStates | (listStates == []) = 0 
                      | otherwise =  last (List.sort(foldr(\x-> (:)(maxAddr x)) [] listStates))
 where maxAddr state
         | (nlist== []) = 0
         | otherwise = last(nlist)
            where nlist = List.sort(foldr(\x -> (:)(getAddress x)) [] (Map.keys (getHeap (getHeaps state))))

-- | Returns the highest stateId of a list of states
retStateId state listStates | (listStates == []) = getStateId state
                           | otherwise = last (List.sort (foldr(\x -> (:)(getStateId x)) [] listStates))

-- | Instance Refinement can occur when e.g doing a cmpeq/cmpneq on an address stored in a field table. 
instanceRefinement :: Prog -> State -> Address -> [State]
instanceRefinement prog state addr = newstate:[nullState]
       where subclasses     = (getClassVar obj):(scr Map.! (getClassVar obj))
             obj            = heapBef Map.! addr
             scr            = buildSubClassRelation prog
             frameLst       = getFrames state 
             heapsBef       = getHeaps state
             heapBef   = getHeap heapsBef
             addrCr = getAddrCounter heapsBef
             stateId        = (getStateId state)
             annot = getAnnotations state             

             nullHeap       = Map.delete addr nullHeap'
             nullHeap' = findAndReplaceAddrFTable heapBef (AddVal addr) Null
             nullFrameLst = foldr (\x -> (:)(findAndReplaceFrame x (AddVal addr) (Null))) [] frameLst
             nullState      = (State (Heaps nullHeap (curAddrCounter heapsBef)) nullFrameLst annot (stateId+1))

             newAddr = (addrCr +1)
             distHeap = Map.insert (Addr newAddr) obj heapBef 
             heap' = findAndReplaceAddrFTable distHeap (AddVal addr) (AddVal (Addr newAddr))
             newstate = (State (Heaps heap' (newAddr)) frameLst annot (stateId+2)) 


-- | Unsharing step. addr1 and addr2 adresses on the heap such that addr1 =? addr2 potentially represent
-- | the same object. Two refinements steps are obtained: 
-- | 1.) first case forces addresses to be distinct. Just adds (addr1, addr2) to the iu annotation
-- | 2.) substitutes all occurences of q with p (on the heap and in all frames). 
unsharingStep :: Prog -> State -> Address -> Address -> [State] -> [State]
unsharingStep prog state addr1 addr2 states = states'
 where states' = (equal : distinct)

       equal' = if((fst cycCheck') == True) 
               then deleteCycle equal (snd cycCheck')
               else equal

       distinct' = if((fst cycCheck'') == True) 
                   then [deleteCycle (distinct!!0) (snd cycCheck')]
                   else distinct
       cycCheck' = cycleCheck prog equal
       cycCheck'' = cycleCheck prog (distinct!!0)
       distinct = [(State (getHeaps state) (getFrames state) annot' (fst stateIds))]
       annot' = (addr1,addr2):(getAnnotations state)
       stateIds = ((retStateId state states)+1,(retStateId state states)+2)

       heapsBef       = getHeaps state
       heapBef        = curHeap heapsBef
       equal = (State (Heaps heap (curAddrCounter heapsBef)) frames (getAnnotations state) (snd stateIds))
       heap' = findAndReplaceAddrFTable heapBef (AddVal addr2) (AddVal addr1)
       heap = Map.delete (addr2) heap'
       --heap'' = foldr(\x -> if (x == (addr2)) then (Map.insert addr1 (heap' Map.! x)) else Map.insert x (heap' Map.! x)) Map.empty (Map.keys heap')

       frames' = getFrames state
       frames = foldr (\x -> (:)(findAndReplaceFrame x (AddVal addr2) (AddVal addr1))) [] frames'


