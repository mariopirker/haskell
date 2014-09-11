-- | file: ComputationGraph/Instructions.hs
-- | author: Mario Pirker
-- | This file includes all the JBC instructions that are available

module ComputationGraph.Instructions where

import qualified Data.Map as Map 
import qualified Data.List as List
import qualified Data.Char as Char
import ComputationGraph.HelperFunctions
import ComputationGraph.State
import ComputationGraph.Program
import ComputationGraph.Refinements
import ComputationGraph.StateGraph
import ComputationGraph.Morphism
import ComputationGraph.HeapGraph
import ComputationGraph.Unification
import ComputationGraph.ComputationGraph
import ComputationGraph.Summarize
import ComputationGraph.InstanceRelation
import Data.List (elemIndex)

{- -------------------------------------------- -}
{- --------------- operations ----------------- -}
{- -------------------------------------------- -}

-- | Runs the ComputationGraph
--runComputationGraph prog cn mn n = f1
runComputationGraph prog cn mn n = f0
  where --f0 = f1
	f0 = (convertToIndexedList f1)
        f1 = execWrap f3 f2 cn mn [] n
        f2 = createStartState prog cn mn
        f3 = prog

-- | Removes duplicated arrows within the compGraph
removeDuplArrow lst = foldr(\x -> (:)((fst x),(f (snd x)))) [] lst
 where  f x = (List.nub (fst x),List.nub (snd x))

-- | Creates the start state for a given program + class name + method name
createStartState :: Prog -> Cn -> Mn -> State
createStartState p cn mn = State startHeaps startFrame [] startStateId
   where startHeaps  = (Heaps heapStart (length addrVals))
         startFrame = [Frame [] loc' cn mn 0]
         loc'      = paramlocF++locvars

         fields      = (getFDecl (getFDeclarations p cn))
         fields'     = zip [1..] fields 

         addrVals    = (Map.keys heapStart)

         heapThis    = foldr (\x -> calc x) thisReference fields'
           where calc x = case (fst (snd(x))) of
                           "int"  -> Map.union Map.empty
                           "bool" -> Map.union Map.empty
                           _      -> Map.insert (Addr (fst x)) (AbsVariable(ClassVar (fst (snd(x)))))

         thisReference = if (fields == []) then Map.singleton (Addr 0) (ObjPair(cn, Map.empty)) else Map.singleton (Addr 0) (ObjPair(cn,ftableThis))

         ftableThis  = foldr (\x -> setftable x) Map.empty fields'
          where setftable x | ((fst (snd x))=="bool") = Map.insert (snd x) (AbsBoolVal "AbsBool") 
                            | ((fst (snd x))=="int") = Map.insert (snd x) (AbsIntVal "AbsInt")
                            | otherwise  = Map.insert (snd x) (AddVal (Addr (fst x)))

         valsFTable  = Map.elems ftableThis 

         insertlist  = zip [(length(Map.keys heapThis))..] (foldr( \x -> (:)(fst x)) [] params)
         heapStart   =  foldr (\x -> Map.insert (Addr (fst x)) (AbsVariable (ClassVar (snd x)))) heapThis insertlist

         method'   = (method p cn mn)!!0
         maxlocs   = getMaxLocs (methodFourthElem method')
         locvars   = replicate maxlocs Unit
         params    = getParMeth (methodSecondElem method')
         paramloc  = foldr(\x -> (:)(AddVal x)) [] addrVals
         -- delete all addresses that are not stored in registers
         paramlocF = foldr (\x -> List.delete x) paramloc valsFTable 

-- | Because execN returns the counter for the states as second element and we are only intersted in the final 
-- | output, we need to take fst here. 
execWrap prog state startClassName startMethodName path n = f1
 where f1 = exec prog state startClassName startMethodName path n

-- | This function is the wrapper function for executing the Programm
-- | startClassName is startclassname
-- | startMethodName is the starting method where all starts
exec prog state startClassName startMethodName stack n
--    | (fst cycCheck) = [(state,([state],["cycle!!!"]))]
--    | (fst cycCheck) = checkNextStepCompGraph prog state' startClassName startMethodName stack n
    | (length(snd exec')) > 0 && ((snd exec')!!0 == "Program Terminated") = [(state,exec')]
    | (length(snd exec')) > 0 && ((snd exec')!!0 == "Program Finish") = [(state,exec')]
    | (n > 0) && ((curPc state) < n) = checkNextStepCompGraph prog state startClassName startMethodName stack n
    | (n==0) && ((curPc state) < ((length ilist) - 1)) = checkNextStepCompGraph prog state startClassName startMethodName stack n
    | otherwise = [(state,exec')]
          where  exec' = execinstr ((ilist!!(curPc state)),prog, state)
                 cycCheck = cycleCheck prog state
                 ilist = (instr_of prog startClassName startMethodName)


-- | Checks wether the PC has already been reached in that path.
-- | If yes, summarize that two states and delete all successing states between actual state and first state that needed to be summariz-- | and recursively call itself with that new summarized state
checkNextStepCompGraph prog state startCn startMn stack n 
 | (length stack' < 2) = recCall                             -- no summarize takes place when there are less than two elements on the stack.
 | (curPc (head stack')) == (curPc (head (tail stack'))) =   -- if the head from the stack and the second element has same PC, then summarize!
      if ((checkIfInstance (head stack') (head (tail stack')) prog))                  -- if new state is instance of old state, terminate path.
      then recCallInstance
      else recCallSummarize                                  -- if its not an instance, generate summarization and recursively call itself from that state
 | otherwise = recCall
           where 
                 recCall = ((state,exec'):(List.concat(map (\x -> exec prog x startCn startMn stackRec n) (fst exec'))))
                 recCallInstance = [((head (stack')),([(head(tail stack'))],["instance of"]))]
                 recCallSummarize = (xx++(List.concat(map (\x -> exec prog x startCn startMn stackRec n) (fst exec'')))) 

                 xx = (state,([summState'],["Summarize"])):[(summState',([(head (tail (stack')))],[""]))]++[(summState',exec'')]

                 exec' = execinstr ((ilist!!(curPc state)),prog, state)
 
                 exec''= execinstr ((ilist!!(curPc summState')),prog,summState')

                 ilist = (instr_of prog startCn startMn)
                 specialpositions = gatherSummarizePositions prog startCn startMn


                 summState' | (removeAddresses == []) = summState
                            | otherwise = State heapsMod (getFrames summState) (getAnnotations summState) (curStid summState)
                                where heapsMod =  (Heaps (foldr(\x -> Map.delete x) (getHeap(getHeaps summState)) removeAddresses) (curAddrCounter (getHeaps summState)))
                 removeAddresses = foldr(\x -> if((fst (check x))==False) then (:)(snd (check x)) else (++)[]) [] (Map.keys (getHeap(getHeaps summState))) 
                   where check x = isReferenced summState x 
                 summState = summarizeStates prog (head stack') (head(tail stack'))

                 -- Pushes the state' onto the stack if it is a special one.
                 stack' | (List.elem (curPc state) specialpositions) = pushPath state stack
                        | otherwise = stack

                 -- If there has been a summarize (second case), remove both states from stack.
                 stackRec | length stack' < 2 = stack'
                          | (curPc (head stack')) == (curPc (head (tail stack'))) = pushPath summState' (popPath (popPath stack'))
                          | otherwise = stack'


-- | Checks wether the new state is an instance of the old one. 
checkIfInstance stateNew stateOld prog = instanceof prog stateNew' stateOld'
 where stateOld' 
         | ((removeAddresses stateOld) == []) = State (getHeaps stateOld) (getFrames stateOld) (getAnnotations stateOld) (curStid stateOld)
         | otherwise = State heapsMod (getFrames stateOld) (getAnnotations stateOld) (curStid stateOld)
              where heapsMod =  (Heaps (foldr(\x -> Map.delete x) (getHeap(getHeaps stateOld)) (removeAddresses stateOld)) (curAddrCounter (getHeaps stateOld)))
       stateNew'  
          | ((removeAddresses stateNew) == []) = State (getHeaps stateNew) (getFrames stateNew) (getAnnotations stateNew) (curStid stateNew)
          | otherwise = State heapsMod (getFrames stateNew) (getAnnotations stateNew) (curStid stateNew)
              where heapsMod =  (Heaps (foldr(\x -> Map.delete x) (getHeap(getHeaps stateNew)) (removeAddresses stateNew)) (curAddrCounter (getHeaps stateNew)))
       removeAddresses s = foldr(\x -> if((fst (check x))==False) then (:)(snd (check x)) else (++)[]) [] (Map.keys (getHeap(getHeaps s)))
                   where check x = isReferenced s x

-- | Calculates the summarize positions
gatherSummarizePositions prog startClassName startMethodName = List.sort specialpositions 
 where ilist = zip [0..] (instr_of prog startClassName startMethodName)
       specialpositions = foldr(\x -> findPos x ) [] ilist
                              where findPos x = if ((getSpecialPC (snd x)) == 9999999) 
                                                then (++)[]
                                                else (:)((fst x) + (getSpecialPC (snd x)))


-- | Normalize function that gives every state a unique stateid
convertToIndexedList lst = fst(loop lst [] [])
  where
    loop [] sts' xs = (sts', xs)
    loop ((s, (ss, cs)):sts) sts' xs =
      loop sts (sts' ++ [(s{state=fst st}, (convStates, cs))]) xs''  
      where
        st = getIndexListTuple xs s
        (convStates,xs'')  = foldr func ([],snd st) ss
        func y x = (s{state = si} : fst x , xs')
          where
           (si,xs') = getIndexListTuple (snd x) y


--getIndexListTuple :: Eq a => [a] -> a -> (Int , [a])
getIndexListTuple :: [State] -> State -> (Int , [State])
getIndexListTuple sts s =
    case elemIndex (s{state=0}) sts of
         Just i  -> (i,sts)
         Nothing -> (length sts , sts ++ [s{state=0}])

-- | Input: instruction to execute, program and the state before
execinstr :: (Instr, Prog, State) -> ([State],[Constraint])

-- | Load operation. Loads value from Register.
execinstr ((Load nat), prog, state) = ([State (curHeaps state) frame' annot ((curStid state)+1)],[""])
  where frame' = (Frame ((loc'!!(nat)):(curStk state)) loc' (curCn state) (curMn state) ((curPc state)+1)):(tail frmlist)
        loc'   = curLoc state
        frmlist= getFrames state 
        annot = getAnnotations state

-- | Store operation. Stores the value into a Register.
execinstr ((Store nat), prog, state) = ([State (curHeaps state) frame' annot ((curStid state)+1)],[""])
  where frame'  = (Frame (tail stk') (safeReplaceElement (curLoc state) nat (head stk')) (curCn state) (curMn state) ((curPc state)+1)):(tail frmlist)
        stk'    = curStk state
        frmlist= getFrames state
        annot = getAnnotations state

-- | Push operation. Pushes a value on the stack
execinstr ((Push val), prog, state) =  ([State (curHeaps state) frame' annot ((curStid state)+1)],[""])
  where frame' = (Frame (val:(curStk state)) (curLoc state) (curCn state) (curMn state) ((curPc state)+1)):(tail frmlist)
        frmlist= getFrames state
        annot = getAnnotations state

-- | New Cname. Creates a new class
execinstr ((New cn), prog, state) = ([State  (Heaps heap' addrCr) frame' annot ((curStid state)+1)],[""])
  where heap' = Map.insert (Addr addrCr) (AbsVariable(ClassVar cn)) heap'''
        heap'' = curHeaps state
        heap'''= curHeap heap''
        addrCr = ((curAddrCounter heap'')+1)
        frame' = (Frame ((AddVal((last (Map.keys heap')))):(curStk state)) (curLoc state) (curCn state) (curMn state) ((curPc state)+1)):(tail frmlist)
        frmlist= getFrames state
        annot = getAnnotations state

-- | Getfield F C. fetches Field F from object C
execinstr ((Getfield fn cn), prog, state) | (not (isAddVal (head stk'))) = terminate 
                                          | (not (isClassVar (heap'' Map.! (getAddVal(head stk'))))) && (not typcheck) = terminate
                                          | (isClassVar (heap'' Map.! (getAddVal(head stk')))) = (caseDistinction prog state (getAddVal (head stk')))
                                          | typcheck = ([State heap' frame' annot ((curStid state)+1)],[""])
                                          | otherwise = terminate
  where frame' = (Frame (val:(tail stk')) (curLoc state) (curCn state) (curMn state) ((curPc state)+1)):(tail frmlist)
        ftable'= snd(getObjPair(obj))
        obj = ((heap'')Map.!(getAddVal(head stk')))
        cn' = fst(getObjPair(obj))

        typcheck | (cn == cn') = True
                 | List.elem cn' ((buildSubClassRelation prog) Map.! cn) = True
                 | otherwise = False

        val = (foldr(\x -> if(snd x == fn) then (:)(ftable' Map.! x) else (++)[])  [] (Map.keys ftable'))!!0

        heap'  = curHeaps state
        heap'' = curHeap heap'
        stk'   = curStk state
        frmlist= getFrames state
        annot = getAnnotations state

        terminate = terminatePath (curCn state) (curMn state) (curPc state) (curStid state)

-- | Putfield F C.sets the field F in the object C
execinstr ((Putfield fn cn), prog, state) | not(isAddVal (head (tail (stk')))) = terminate
                                          | (isClassVar (heap''' Map.! r)) = (caseDistinction prog state r)
                                          | uncheckresult = (sharingsteps,constraints)
                                          | otherwise = ([State (Heaps heap' (curAddrCounter heap'')) frame' annot ((curStid state)+1)],[""])
  where constraints = List.concat(List.transpose [(replicate (div (length sharingsteps) 2) "equal"),(replicate (div (length sharingsteps) 2) "distinct")])
        sharingsteps = foldr(\x y -> if((fst x)==True)then (add x y) else y) [] uncheck
            where add x y = (unsharingStep prog state (fst(snd x)) (snd(snd x)) y)++y
        r      = getAddVal(head (tail(stk')))
        obj = (heap''' Map.! r)
        d      = fst(getObjPair (obj))
        ftable = snd(getObjPair(obj))

        uncheck = putfieldCheck prog state r
        uncheckresult = List.or ((foldr (\x -> (:) (fst x))) [] uncheck)
        heap'  = (Map.insert (getAddVal (head (tail stk'))) (ObjPair(d,ftable')) heap''')
        heap'' = curHeaps state
        heap'''= curHeap heap''
        frame' = (Frame stk'' (curLoc state) (curCn state) (curMn state) ((curPc state)+1)):(tail frmlist)

        v x    = if x == ((snd(getObjPair(obj))) Map.! ftval) then Just (head stk') else Nothing
        ftval = (foldr(\x -> if(snd x==fn) then (:)x else (++)[]) [] (Map.keys ftable))!!0
        ftable'= Map.update v ftval ftable
        stk'   = curStk state
        stk''  = tail( tail( stk'))
        frmlist= getFrames state
        annot = getAnnotations state

        terminate = terminatePath (curCn state) (curMn state) (curPc state) (curStid state)


-- | Checkcast cname. check if object is of class cname
-- | consider exception-free evaluation, all casts are allowed.
execinstr ((Checkcast cn), prog, state) = ([State (curHeaps state) frameLst annot ((curStid state)+1)],[""])
  where     frameLst  = getFrames state
            annot = getAnnotations state

-- | Invoke mname val. Invoke instance method with nat(=n) parameters
execinstr ((Invoke mn n), prog, state) = ([State heap' frame' annot ((curStid state)+1)],[""])
  where frame'    = (Frame [] loc' d mn 0):frmlist
        heap'     = curHeaps state
        heap''    = curHeap heap'
        annot = getAnnotations state
        -- | parameters in reverse orde
        ps        = take (n-1) stk'  
        -- | lookup of the class name
        c         = fst(getObjPair(heap'' Map.! r))  
        -- | address of class object (where method is executed)
        r         = getAddVal(stk'!!(n))    
        d         = methodFirstElem method'
        method'   = (method prog c mn)!!0
        frmlist   = getFrames state
        -- | local vars are the address val+params in reverse order and rest of locs (Unit)
        loc'      = ((AddVal r):(reverse ps))++loc''         
        maxlocs   = getMaxLocs (methodFourthElem method')
        -- | create sec. list with all fields set to Unit. Length of that list is maxlocs - 1(address val.) - length of paramlist
        loc''     = replicate (maxlocs-1-(length ps)) Unit    
        stk'      = curStk state

-- | returnFromMethod. Returns from method
execinstr ((Return),prog, state) =  if (isFrameEmpty frmlist)
                                then ([State heap' [] annot ((curStid state)+1)],["Program Finish"])
                                else ([State heap' frame' annot ((curStid state)+1)],[""])
  where          heap'        = curHeaps state
                 frame'       = (Frame (v:(drop (n+1) stk'')) loc'' cn'' mn'' (pc''+1)):(tail frmlist)
                 frmlist      = getFrames state
                 v            = head (curStk state)    -- | returns value of top method that is on the top stack
                 hdframe      = head frmlist           -- | frame from that the method was called.(one frame under)
                 n            = length (methodSecondElem( (method prog cn' mn')!!0)) -- | length of the call parameters
                 stk''        = getStk hdframe         -- | stack of frame one under top frame
                 loc''        = getLoc hdframe 
                 cn''         = getCn hdframe 
                 mn''         = getMn hdframe
                 pc''         = getPc hdframe

                 cn'          = curCn state            -- | class name of top frame 
                 mn'          = curMn state            -- | method name of top frame

                 annot = getAnnotations state

-- | Pop operation. Pops the value from the stack
execinstr ((Pop),prog, state) = ([State (curHeaps state) frame' annot ((curStid state)+1)],[""])
  where frame' = (Frame (drop 1 (curStk state)) (curLoc state) (curCn state) (curMn state) ((curPc state)+1)):(tail frmlist)
        frmlist= getFrames state
        annot = getAnnotations state

-- | Iadd. integer addition of the last two elements on the stack
execinstr ((IAdd),prog, state) = ([State (curHeaps state) frame' annot ((curStid state)+1)],[constraint])
  where frame' = (Frame ((sum v' v''):(tail(tail(stk')))) (curLoc state) (curCn state) (curMn state) ((curPc state)+1)):(tail frmlist)
        v'     = getStkn stk' 0
        v''    = getStkn stk' 1
        -- | if v1 is abstract or v2 is abstract write i3 on top of the stack.
        sum v1 v2 | ((isAbstractVal v1) && (isAbstractVal v2)) = AbsIntVal "i3"
                  | ((isAbstractVal v1) && (not (isAbstractVal v2))) = AbsIntVal "i3" 
                  | ((not(isAbstractVal v1)) && (isAbstractVal v2)) = AbsIntVal "i3" 
                  | otherwise = (IntVal ((getIntVal v1)+(getIntVal v2)))
        stk'   = curStk state
        frmlist= getFrames state
        annot = getAnnotations state

        constraint = if((isAbstractVal v') || (isAbstractVal v''))
                     then "i3 = " ++ showAbsVal v' ++ " + " ++ showAbsVal v''
                     else ""

-- | ISub. Interger substraction. takes 0th and 1st elem of the stack and executes operation
execinstr ((ISub),prog, state) = ([State (curHeaps state) frame' annot ((curStid state)+1)],[constraint])
  where frame' = (Frame ((sub v' v''):(tail(tail(stk')))) (curLoc state) (curCn state) (curMn state) ((curPc state)+1)):(tail frmlist)
        v'     = getStkn stk' 0
        v''    = getStkn stk' 1
        sub v1 v2 | ((isAbstractVal v1) && (isAbstractVal v2)) = AbsIntVal "i3"
                  | ((isAbstractVal v1) && (not (isAbstractVal v2))) = AbsIntVal "i3"
                  | ((not(isAbstractVal v1)) && (isAbstractVal v2)) = AbsIntVal "i3"
                  | otherwise = (IntVal ((getIntVal v1)-(getIntVal v2)))


        constraint = if((isAbstractVal v') || (isAbstractVal v''))
                     then "i3 = " ++ showAbsVal v' ++ " - " ++ showAbsVal v''
                     else ""

        stk'   = curStk state
        frmlist= getFrames state
        annot = getAnnotations state


-- | Goto i. Goto relative address (pc+i)
execinstr ((Goto (i)),prog, state) = ([State (curHeaps state) frame' annot ((curStid state)+1)],[""])
  where frame' = (Frame (drop 1 (curStk state)) (curLoc state) (curCn state) (curMn state) pc'):(tail frmlist)
        frmlist= getFrames state
        pc'    = if (i < 0) 
                 then ((curPc state)-(abs i))
                 else ((curPc state)+i)
        annot = getAnnotations state

-- | CmpEq. equality comparison. Compares the last two vales on the stack.
execinstr ((CmpEq),prog, state) 
     | ((fst checkabsvars) == True) = (caseDistinction prog state (snd checkabsvars))
     | ((isAbstractVal (head stk')) || (isAbstractVal (head (tail stk'))) || ((fst checkabsvars) == True)) = stateAbs 
     | otherwise = ([State  (curHeaps state) frame' annot ((curStid state)+1)],[""])
  where stateAbs = ([State (curHeaps state) frameAbs1 annot ((curStid state)+1), State (curHeaps state) frameAbs2 annot ((curStid state)+2)],["a > b", "not(a > b)"])
        frameAbs1=(Frame stk'' (curLoc state) (curCn state) (curMn state) (pc'+1)):(tail frmlist)
        frameAbs2=(Frame stk''' (curLoc state) (curCn state) (curMn state) (pc'+1)):(tail frmlist)
        frame' = (Frame ((cmp' (stk'!!0) (stk'!!1)):(tail(tail stk'))) (curLoc state) (curCn state) (curMn state) ((curPc state)+1)):(tail frmlist)
        cmp' s1 s2 = if (s1==s2) then BoolVal True else BoolVal False
        pc' = curPc state
        stk'   = curStk state
        stk'' = (BoolVal True):(tail (tail stk'))
        stk''' = (BoolVal False):(tail (tail stk'))
        frmlist= getFrames state
        annot = getAnnotations state
        heap = curHeap (curHeaps state)
        -- | gets the two top elements of stack and returns true + address if one of both references abstract class var.
        checkabsvars = checkAbsVar heap (stk'!!0) (stk'!!1)

-- | CmpGeq. equality comparison. Compares the last two vales on the stack and write true if v0 is 
execinstr ((CmpGeq),prog, state) 
          | ((fst checkabsvars) == True) = (caseDistinction prog state (snd checkabsvars))
          | ((isAbstractVal (head stk')) || (isAbstractVal (head (tail stk'))) || ((fst checkabsvars) == True)) = stateAbs 
          | otherwise = ([State  (curHeaps state) frame' annot ((curStid state)+1)],[""])
  where stateAbs = ([State (curHeaps state) frameAbs1 annot ((curStid state)+1), State (curHeaps state) frameAbs2 annot ((curStid state)+2)],["a => b", "not(a => b)"])
        frameAbs1=(Frame stk'' (curLoc state) (curCn state) (curMn state) (pc'+1)):(tail frmlist)
        frameAbs2=(Frame stk''' (curLoc state) (curCn state) (curMn state) (pc'+1)):(tail frmlist)
        frame' = (Frame ((cmp' (stk'!!0) (stk'!!1)):(tail(tail stk'))) (curLoc state) (curCn state) (curMn state) ((curPc state)+1)):(tail frmlist)
        cmp' s1 s2 = if ((s1==s2) || (s1 > s2))
                      then BoolVal True
                      else BoolVal False
        pc' = curPc state
        stk'   = curStk state
        stk'' = (BoolVal True):(tail (tail stk'))
        stk''' = (BoolVal False):(tail (tail stk'))
        frmlist= getFrames state
        annot = getAnnotations state
        heap = getHeap (getHeaps state)

        checkabsvars = checkAbsVar heap (stk'!!0) (stk'!!1)

-- | CmpNeq. equality comparison. Compares the last two values on the stack and returns true if they are not equal. 
execinstr ((CmpNeq),prog, state) 
          | ((fst checkabsvars) == True) = (caseDistinction prog state (snd checkabsvars))
          | (isAbstractVal (head stk')) || (isAbstractVal (head (tail stk'))) = stateAbs
          | otherwise = ([State  (curHeaps state) frame' annot ((curStid state)+1)],[""])
 where  stateAbs = ([State (curHeaps state) frameAbs1 annot ((curStid state)+1), State (curHeaps state) frameAbs2 annot ((curStid state)+2)],["not(a = b)", "a = b"])
        frameAbs1=(Frame stk'' (curLoc state) (curCn state) (curMn state) (pc'+1)):(tail frmlist)
        frameAbs2=(Frame stk''' (curLoc state) (curCn state) (curMn state) (pc'+1)):(tail frmlist)
        frame' = (Frame ((cmp' (stk'!!0) (stk'!!1)):(tail(tail stk'))) (curLoc state) (curCn state) (curMn state) ((curPc state)+1)):(tail frmlist)
        cmp' s1 s2 = if (s1 /= s2) then BoolVal True else BoolVal False
        pc' = curPc state
        stk'   = curStk state
        stk'' = (BoolVal True):(tail (tail stk'))
        stk''' = (BoolVal False):(tail (tail stk'))
        frmlist= getFrames state
        annot = getAnnotations state
        heap = curHeap (curHeaps state)
        checkabsvars = checkAbsVar heap (stk'!!0) (stk'!!1)

-- | Not. Negates the Bool value on the stack.
execinstr ((BNot),prog, state) = if(isAbsBoolVal(head (curStk state)))
                                 then stateAbs
                                 else stateNormal
  where stateNormal = ([State  (curHeaps state) frame' annot ((curStid state)+1)],[""])
        stateAbs = ([State (curHeaps state) frameAbs1 annot ((curStid state)+1), State (curHeaps state) frameAbs2 annot ((curStid state)+2)],["a", "not a"])
        frameAbs1=(Frame stk'' (curLoc state) (curCn state) (curMn state) (pc'+1)):(tail frmlist)
        frameAbs2=(Frame stk''' (curLoc state) (curCn state) (curMn state) (pc'+1)):(tail frmlist)
        frame' = (Frame ((cmp' (stk'!!0)):(tail stk')) (curLoc state) (curCn state) (curMn state) ((curPc state)+1)):(tail frmlist)
        pc' = curPc state
        cmp' s1 = if ((getBoolVal s1)==True) 
                  then BoolVal False
                  else BoolVal True
        stk'   = curStk state
        stk'' = (BoolVal True):(tail stk')
        stk''' = (BoolVal False):(tail stk')
        frmlist= getFrames state
        annot = getAnnotations state

-- | And. Makes a Boolean and Between the two last values on the stack
execinstr ((BAnd),prog, state) = if((isAbsBoolVal(head (curStk state))) || (isAbsBoolVal (head(tail (curStk state)))))
                                 then stateAbs
                                 else stateNormal
  where stateNormal = ([State  (curHeaps state) frame' annot ((curStid state)+1)],[""])
        stateAbs = ([State (curHeaps state) frameAbs1 annot ((curStid state)+1), State (curHeaps state) frameAbs2 annot ((curStid state)+2)],["a v b", "not(a v b)"])
        frameAbs1=(Frame stk'' (curLoc state) (curCn state) (curMn state) (pc'+1)):(tail frmlist)
        frameAbs2=(Frame stk''' (curLoc state) (curCn state) (curMn state) (pc'+1)):(tail frmlist)
        frame' = (Frame ((cmp' (stk'!!0) (stk'!!1)):(tail (tail stk'))) (curLoc state) (curCn state) (curMn state) ((curPc state)+1)):(tail frmlist)
        pc' = curPc state
        cmp' s1 s2 = if (((getBoolVal s1) && (getBoolVal s2))==True)
                     then BoolVal True
                     else BoolVal False
        stk'   = curStk state
        stk'' = (BoolVal True):(tail (tail stk'))
        stk''' = (BoolVal False):(tail (tail stk'))
        frmlist= getFrames state
        annot = getAnnotations state

-- | Or. Makes a Boolean or between the last two values on the stack.
execinstr ((BOr),prog, state) = if((isAbsBoolVal(head (curStk state))) || (isAbsBoolVal (head(tail (curStk state)))))
                                then stateAbs
                                else stateNormal
  where stateNormal = ([State  (curHeaps state) frame' annot ((curStid state)+1)],[""]) 
        stateAbs = ([State (curHeaps state) frameAbs1 annot ((curStid state)+1), State (curHeaps state) frameAbs2 annot ((curStid state)+2)],["a ^ b", "not(a ^ b)"])
        frameAbs1=(Frame stk'' (curLoc state) (curCn state) (curMn state) (pc'+1)):(tail frmlist)
        frameAbs2=(Frame stk''' (curLoc state) (curCn state) (curMn state) (pc'+1)):(tail frmlist)
        frame' = (Frame ((cmp' (stk'!!0) (stk'!!1)):(tail (tail stk'))) (curLoc state) (curCn state) (curMn state) (pc'+1)):(tail frmlist)
        pc' = curPc state
        cmp' s1 s2 = if (((getBoolVal s1)==True) ||  ((getBoolVal s2)==True))
                     then BoolVal True
                     else BoolVal False
        stk'   = curStk state
        stk'' = (BoolVal True):(tail (tail stk'))
        stk''' = (BoolVal False):(tail (tail stk'))
        frmlist= getFrames state
        annot = getAnnotations state


-- | IfFalse int. Branches if the top of the stack is false(top of stack not empty)
execinstr ((IfFalse i),prog, state) = if(isAbsBoolVal(head (curStk state)))
                                      then statesAbs
                                      else ([State (curHeaps state) frame' annot ((curStid state)+1)],[""])
  where frame' = (Frame stk' (curLoc state) (curCn state) (curMn state) pc''):(tail frmlist)
        stk'   = tail (curStk state)
        pc'    = curPc state
        pc''   = if ((head (curStk state))==(BoolVal False))
              then pc'+i
              else pc'+1
        frmlist= getFrames state
        annot = getAnnotations state
        statesAbs = ([State (curHeaps state) frameAbs1 annot ((curStid state)+1), State (curHeaps state) frameAbs2 annot ((curStid state)+2)],["not bool", "bool"])
        frameAbs1=(Frame stk' (curLoc state) (curCn state) (curMn state) (pc'+i)):(tail frmlist)
        frameAbs2=(Frame stk' (curLoc state) (curCn state) (curMn state) (pc'+1)):(tail frmlist)

{- ------------------------------------------------ -}
{- --------------- helper functions --------------- -}
{- ------------------------------------------------ -}

-- | Usage: [a] -> Int -> a -> [a]
-- | the list -> Index of element to replace -> the new element -> The updated list
safeReplaceElement :: [a] -> Int -> a -> [a]
safeReplaceElement xs i x =
  if i >= 0 && i < length xs
    then replaceElement xs i x
    else xs

replaceElement :: [a] -> Int -> a -> [a]
replaceElement xs i x = fore ++ (x : aft)
  where fore = take i xs
        aft  = drop (i+1) xs


-- | Functions displaying the current things of a state on the top frame 
curHeaps :: State -> Heaps
curHeaps s = getHeaps s

curStk :: State -> Stk
curStk s = getStk (getFirstFrame(getFrames s))

curLoc :: State -> Loc
curLoc s = getLoc (getFirstFrame(getFrames s))

curCn :: State -> Cn
curCn s = getCn (getFirstFrame(getFrames s))

curMn :: State -> Mn
curMn s = getMn (getFirstFrame(getFrames s))

curPc :: State -> Pc
curPc s = getPc (getFirstFrame(getFrames s))

curStid :: State -> StateId
curStid s = getStateId s

