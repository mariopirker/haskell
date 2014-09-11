-- file: state.hs
-- represents a state of the computation graph

module ComputationGraph.State where

import qualified Data.Map as Map 
import qualified Data.Maybe as Maybe (fromMaybe)

{---------------------------------------------------------}
{-----------------Definition of Datatypes-----------------}
{---------------------------------------------------------}

{- --------------- general --------------- -}

-- This state is returned when one path ends in the programm
terminatePath cn mn pc stateid = ([State (Heaps Map.empty 0) [(Frame [] [] cn mn pc)] [] (stateid+1)],["Program Terminated"])

type Constraint = String

data AbsVal = IntVal Int
            | AddVal Address
            | BoolVal Bool
            | Null
            | Unit
            | AbsIntVal AbsInt 
            | AbsBoolVal AbsBool
            | AbsLocation AbsLoc
            | AbsClassVar AbsClass
            deriving(Show,Eq,Ord)

type AbsInt = String 

plus :: AbsInt -> AbsInt -> AbsInt
plus a b = a ++ " plus " ++ b

type AbsBool = String  

type AbsClass = Cn

data AbsVar = ClassVar AbsClass
            | AbsIntVar AbsInt
            | AbsBoolVar AbsBool
            deriving(Eq,Show,Ord)
 
{- --------------- heap --------------- -}
-- address
data Address = Addr Int deriving (Eq, Ord, Show)

data Object = ObjPair (Cn,Ft)
              | AbsVariable AbsVar 
               -- | ClassVar AbsClass        -- abstract class variable like TreeList(?)
               deriving(Eq,Show,Ord)

-- field table
-- Dn is the class where the fieldid is defined.
type Ft = Map.Map (Dn, Fieldid) AbsVal

data FieldValue = AbsVal 
               -- | AbsVar
                deriving(Eq,Show,Ord)

-- Field id
type Fieldid = String

-- Field Name for field access
type Fn = String

-- mapping from addresses to objects.
type Heap = Map.Map Address Object

data Heaps = Heaps { heaps :: Heap
                    ,addrcounter::Int} deriving (Show,Eq,Ord)

-- Abstract Location
type AbsLoc = Int

-- Abstract Location Mapping
type AbsLocationTuple = Map.Map AbsLocPos (AbsLoc,AbsVal)

data AbsLocPos = HeapPos (Address, Dn, Fieldid) 
               | StkPos StkIndex
               | LocPos LocIndex
               deriving(Show,Eq,Ord)

{- --------------- frames --------------- -}
-- program counter
type Pc = Int

-- name of the method
type Mn = String

-- name of superclass
type Dn    = String

-- name of the class
type Cn = String

-- local variables, registers
type Loc = [AbsVal]

-- stack
type Stk = [AbsVal]

-- execution environment
data Frame = Frame {  stk :: Stk
                     , loc :: Loc
                     , cn :: Cn
                     , mn :: Mn
                     , pc :: Pc} deriving (Show,Eq,Ord)

-- list of frames, for each method there is one
type Frames = [Frame]

-- Mappings from (Stack, upper bound of stack, j-th Element) that maps to the value stored at the j-th position
data StkIndex    = StkIndex (String, Int, Int) deriving(Eq,Show,Ord)
getStkIndex :: StkIndex -> Int
getStkIndex (StkIndex (__,_,i)) = i

getStkIndexSet (StkIndex s) =s

getStkFrmIx :: StkIndex -> Int
getStkFrmIx (StkIndex (_,i,_)) = i

data LocIndex    = LocIndex (String, Int, Int) deriving(Eq,Show,Ord)
getLocIndex :: LocIndex -> Int
getLocIndex (LocIndex (_,_,i)) =i

getLocIndexSet (LocIndex s) =s

getLocFrmIx :: LocIndex -> Int
getLocFrmIx (LocIndex (_,i,_)) = i

{- --------------- state --------------- -}

startStateId :: StateId
startStateId = 0

type StateId = Int

-- pair of heap and frames
data State = State {  heap   :: Heaps
                   , frames :: Frames
                   , isUnshared :: Iu
                   , state   :: StateId} deriving(Show,Eq,Ord)


{- ------------ annotations ------------ -}
-- Annotations of addresses in state s, denoted iu. pairs p /= q of addresses,
-- where p,q elem Heap and p /= q
type Iu = [(Address,Address)]
