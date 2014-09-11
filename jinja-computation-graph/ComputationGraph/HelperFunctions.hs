-- file: ComputationGraph/HelperFunctions.hs
-- This file has all the helperfunctions needed for accessing and changing the states.

module ComputationGraph.HelperFunctions where 

import ComputationGraph.State
import ComputationGraph.Program
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Char as Char

{---------------------------------------------------------}
{-----------------Helper functions and ops----------------}
{---------------------------------------------------------}

-- ### Preliminary Functions ### ---
-- | Returns the keys of a given map
dom :: Map.Map k a -> [k]
dom m = Map.keys m

-- | Returns the values for a given map
rg :: Map.Map k a -> [a]
rg m = Map.elems m

-- ### Functions State ### ---

-- | Input:  Stk and Integer
-- | Output: returns the n'th element of the stack as an AbsVal
getStkn :: Stk -> (Int -> AbsVal)
getStkn s n = (s!!n)

-- | Input: IntVal <value>
-- | Outpiut: value
getIntVal (IntVal v) = v
getIntVal _          = error "Value not Integer AbsVal (getIntVal)"

-- | Input: AddVal <address>
-- | Output: address
getAddVal (AddVal a) = a
getAddVal _          = error "Value not an Address AbsVal (getAddVal)"

-- | Returns true if given AbsVal is of type AddVal. If not, it returns false.
isAddVal (AddVal a) = True
isAddVal _ = False

-- | Input: BoolVal <bool>
-- | Output: bool
getBoolVal (BoolVal b) = b
getBoolVal _           = error "Value not bool AbsVal (getBoolVal)"

-- | Input: ObjPair (cn, ft)
-- | Output: (cn, ft)
getObjPair (ObjPair o) = o
getObjPair _           = error "Value not object pair (getObjPair)"

-- | Input: AbsClassVar a
-- | Output: a
getAbsClassVar (AbsClassVar a) = a
getAbsClassVar _               = error "Value not an AbsClassVar (getAbsClassVar)"

getAbsClassVar2 (AbsClassVar a) s= a
getAbsClassVar2 _ s              = error s

-- | Input: AbsVariable (ClassVar c)
-- | Output: c 
getClassVar (AbsVariable (ClassVar c)) = c 
getClassVar _            = error "Value not a class var (getClassVar)"

-- | Input: AbsVariable (AbsIntVar i))
-- | Output: i
getAbsIntVar (AbsVariable (AbsIntVar i)) = i
getAbsIntVar _ = error "Value not an abstract int var"

-- | Input: AbsVariable (AbsBoolVar i))
-- | Output: i
getAbsBoolVar (AbsVariable (AbsBoolVar i)) = i
getAbsBoolVar _  = error "Value not an abstract bool var"

-- | Input: AbsLocation a
-- | Output: a
getNonAbsLoc (AbsLocation a) = a
getNonAbsLoc _               = error "Value not an AbsLocation"

-- | Input: List of frames
-- | Output: True if list of frames empty, False otherwise
isFrameEmpty :: Frames -> Bool
isFrameEmpty f = if ((length f) ==1)
                 then True
                 else False

-- | Projection functions cl and ft
-- | Input: Object 
-- | Output: if input is ObjPair then it is the Cn of that Object, otherwise the Name of the ClassVar
cl :: Object -> String
cl (ObjPair o)  = fst(o)
cl (AbsVariable (ClassVar c)) = c

-- | Input: Object
-- | Output: Fieldtable of the object
ft :: (Cn,Ft) -> Ft
ft (cn,ft) = ft

-- | Input: Superclass and Subclassrelation
-- | Output: returns the subclasses
getSubClasses scr dn = scr Map.! dn  

-- | Input: State
-- | Output: returns the heaps (heap, addrcounter) of a state
getHeaps :: State -> Heaps
getHeaps (State heap _ _ _ ) = heap

-- | Returns the heap (Mapping from Addresses to Objects) out of a given "Heaps"
getHeap :: Heaps -> Heap
getHeap (Heaps h _ ) = h

-- | Returns the Address Counter out of a given "Heaps"
getAddrCounter :: Heaps -> Int
getAddrCounter (Heaps _ c) = c

-- | Input: State
-- | Output: returns the list of frames of a state
getFrames :: State -> Frames
getFrames (State _ frames _ _ )   = frames

-- | Input: List of frames
-- | Output: returns the first frame of the list of frames
getFirstFrame :: Frames -> Frame
getFirstFrame f = f!!0

-- | Input: List of frames
-- | Output: returns the second frame of the list of frames
getSecondFrame :: Frames -> Frame
getSecondFrame f = f!!1

-- | Input: State
-- | Output: Name of the state(stateId)
getStateId :: State -> StateId
getStateId (State _ _ _ s) = s

-- | Returns the Annotations (is unshared list of addresses) for a given State
getAnnotations :: State -> Iu
getAnnotations (State _ _ a _) = a

-- | Input: Frame
-- | Output: returns the stack of a frame
getStk :: Frame -> Stk
getStk (Frame stk _ _ _ _) = stk

-- | Input: Frame 
-- | Output: returns the local vars of a frame
getLoc :: Frame -> Loc
getLoc (Frame _ loc _ _ _) = loc

-- | Input: Frame
-- | Output: returns the class name of a frame
getCn :: Frame -> Cn
getCn (Frame _ _ cn _ _) = cn

-- | Input: Frame
-- | Output: returns the method name of a frame
getMn :: Frame -> Mn
getMn (Frame _ _ _ mn _) = mn

-- | Input: Frame
-- | Output: returns the program counter of a frame
getPc :: Frame -> Pc
getPc (Frame _ _ _ _ pc) = pc

-- | Returns the current Address counter for a Heap
curAddrCounter :: Heaps -> Int
curAddrCounter h = getAddrCounter h

-- | Input: AbsVal
-- | Output: True, if Input is an Address 
isAddress :: AbsVal -> Bool
isAddress (AddVal a) = True
isAddress _          = False

-- | Returns the Address if the given AbsVal is an address. Otherwise it will return an error
getAddr (AddVal a) = a
getAddr _ = error "AbsVal is not of Type Address (getAddr)"

-- | Returns the Abstract Location of a given AbsVal. If it is not of that type it will return an error
getAbsLoc (AbsLocation a) =a 
getAbsLoc _ = error "AbsVal is not of Type AbsLocation (getAbsLoc)"

-- | Returns the Integer value of an Address. Otherwise it will return an error
getAddress (Addr a) =a 

-- | Returns the Center of a tripel
centerTripel (_, x ,_)     =  x
-- | Returns the first value of a tripel
firstTripel (x,_,_) = x
-- | Returns the last element of a tripel
lastTripel (_,_,x) = x

-- | Input: AbsVal
-- | Output: True, if the value is Abstract(Abstract Integer or Abstract Boolean)
isAbstractVal :: AbsVal -> Bool
isAbstractVal (AbsIntVal a)  = True
isAbstractVal (AbsBoolVal b) = True
isAbstractVal (AbsClassVar c) = True
isAbstractVal _              = False

-- | Returns True if the given AbsVal is of Type AbsBoolVal
isAbsBoolVal (AbsBoolVal b) = True
isAbsBoolVal _ = False

isAbsClassVar (AbsClassVar c) = True
isAbsClassVar _ =  False

-- | Input: Object
-- | Output, True, if Object is a Class Var
isClassVar :: Object -> Bool
isClassVar (AbsVariable (ClassVar cn)) = True
isClassVar _             = False

-- | Returns true if the given Value is of type AbsVariable (AbsIntVar)
isAbsIntVar (AbsVariable (AbsIntVar cn)) = True
isAbsIntVar _             = False

-- | Returns true if the given Value is of type AbsVariable (AbsBoolVar)
isAbsBoolVar (AbsVariable (AbsBoolVar cn)) = True
isAbsBoolVar _             = False

-- | Returns true if the given Value is of type AbsVariable
isAbstractVar (AbsVariable (ClassVar cn)) = True
isAbstractVar (AbsVariable (AbsIntVar cn)) = True
isAbstractVar (AbsVariable (AbsBoolVar cn)) = True
isAbstractVar _ = False

-- | Returns true if the given Object is an ObjPair
isObjPair (ObjPair o) = True
isObjPair _ =  False

-- | Returns true if the given AbsVal is of type AbsLocation
isAbsLocation (AbsLocation a) = True
isAbsLocation _       = False

-- | Returns the Heap of a given Heaps
curHeap :: Heaps -> Heap
curHeap h = getHeap h

-- | Splits a list if a predicate is matching
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith pred (x:xs) | not (pred x) = splitWith pred xs
splitWith pred xs = (takeWhile pred xs):(splitWith pred next)
                    where rest = dropWhile pred xs
                          rev pred x = not (pred x)
                          next = dropWhile (rev pred) rest


-- | Checks if one of the two elements given is an Abstract Variable
checkAbsVar :: Heap -> AbsVal -> AbsVal -> (Bool,Address)
checkAbsVar h a1 a2 = case (f' a1 a2) of
                       (1,a1) -> if(isClassVar (h Map.! a1)) 
                                    then (True,a1) 
                                    else (False,a1)
                       (2,a2) -> if(isClassVar (h Map.! a2))
                                    then (True,a2)
                                    else (False,a2)
                       (3,(Addr 999))  -> (False,(Addr 999))

f' (AddVal a1) _ = (1,a1)
f' _ (AddVal a2) = (2,a2)
f' _ _ = (3,(Addr 999))

-- | Finds the address where e.g the instance refinement is needed. Returns address and fieldtable entry
findAddressForValInFTable :: Heap -> AbsVal -> [(Address,(Cn,Fn))]
findAddressForValInFTable h addr = (List.concat addrs)
  where addrs = foldr (\x -> findAddr x) [] (Map.keys h) 
                 where findAddr x = if(isClassVar (h Map.! x)) 
                                    then (++)[]
                                    else (:)(foldr (\y -> if((fst y) == True)then (:)(x,snd y) else (++)[]) [] fta')
                                             where fta = snd (getObjPair (h Map.! x))
                                                   fta' = findFTable fta addr
-- | Returns the fieldtable entries where the address has been found within the fieldtable
findFTable :: Ft -> AbsVal -> [(Bool,(Cn,Fn))]
findFTable ft addr = foldr (\x -> build x) [] (Map.keys ft)
  where build x = if ( (ft Map.! x) == addr)
                  then (:)(True,x)
                  else (++)[]

-- | Looks for a given address in the ftable and updates it with Null
findAndReplaceAddrFTable :: Heap -> AbsVal -> AbsVal -> Heap
findAndReplaceAddrFTable h addr val = heap'
 where heap' = foldr (\x -> buildHeap x) Map.empty (Map.keys h)
                  where buildHeap x = if(isClassVar (h Map.! x)) 
                                      then Map.insert x (h Map.! x)
                                      else Map.insert x (ObjPair ((fst(getObjPair (h Map.!x))),(buildFTable (snd(getObjPair (h Map.! x))) addr val)))

-- | Looks for a given AbsVal within the fieldtable and replaces all occurences of that with a new AbsVal and returns the new Ft
buildFTable :: Ft -> AbsVal -> AbsVal -> Ft
buildFTable ft addr val = foldr (\x -> build x) Map.empty (Map.keys ft)
  where build x = if ( (ft Map.! x) == addr)
                  then Map.insert x val
                  else Map.insert x (ft Map.! x)

-- | Looks for a given Address in a frame and exchanges all occurences with a given value val
findAndReplaceFrame :: Frame -> AbsVal -> AbsVal -> Frame
findAndReplaceFrame f addr val = Frame stk' loc' (getCn f) (getMn f) (getPc f)
  where stk' = findAndReplaceStk (getStk f) addr val
        loc' = findAndReplaceLoc (getLoc f) addr val 

-- | Looks for a given AbsVal within the Stack and replaces all occurences of that with a 
-- | new AbsVal and returns the new Stack
findAndReplaceStk :: Stk -> AbsVal -> AbsVal -> Stk
findAndReplaceStk [] _ _ = []
findAndReplaceStk (y:ys) addr val | addr == y = val : (findAndReplaceStk ys addr val)
                                  | otherwise = y: (findAndReplaceStk ys addr val)

-- | Looks for a given AbsVal within the Locs and replaces all occurences of that with a 
-- | new AbsVal and returns the new Locs
findAndReplaceLoc :: Loc -> AbsVal -> AbsVal -> Loc
findAndReplaceLoc [] _ _ = []
findAndReplaceLoc (y:ys) addr val | addr == y = val : (findAndReplaceLoc ys addr val)
                                  | otherwise = y: (findAndReplaceLoc ys addr val)

-- | Removes an item from a list
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

-- | Checks if the class(and the superclass) has a field with name Fn
hasField :: Prog -> ClassName -> Fn -> (ClassName,Bool)
hasField prog cn fn | (List.elem fn fields) = (cn,True)
                    | (List.elem fn fieldsSuper) = (superclass,True)
                    | otherwise = (cn++" and "++superclass,False)
  where superclass = getSupClass (buildSubClassRelation prog) cn
        fields = foldr(\x -> (:)(snd x)) [] (getFDecl (getFDeclarations prog cn))
        fieldsSuper = foldr(\x -> (:)(snd x)) [] (getFDecl (getFDeclarations prog superclass))

-- | Pretty Printers Helper Functions
inLvl x = '\n':mult ' ' x
    where
        mult c n
            | n > 0 = c : mult c (n-1)
            | otherwise = []


-- | Display function for the abstract values
showAbsVal :: AbsVal -> String
showAbsVal (AddVal (Addr a)) = "a"++(show a)
showAbsVal (IntVal i)    = show i
showAbsVal (BoolVal b )  = show b
showAbsVal (Null)        = "Null"
showAbsVal (Unit)        = "Unit"
showAbsVal (AbsIntVal i) = i
showAbsVal (AbsBoolVal b)= b
showAbsVal (AbsLocation a)=show a
showAbsVal (AbsClassVar a)=a
