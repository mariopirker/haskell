-- file: ComputationGraph/Programs.hs
-- author: Mario Pirker
-- This File stores the representation of a JBC programm and some helping functions needed for accessing it.
module ComputationGraph.Program where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Char as Char
import ComputationGraph.State

-- | Subclass relation for the relation between super and subclasses.
-- | Subclassrelation is build up according to program.
type SubClassRelation = Map.Map ClassName [ClassName]

type ClassName = String
type MethodName = String

-- | Program declearation is a List of class Declarations 
-- | data Prog = Prog ([CDecl],SubClassRelation) deriving(Eq,Show)
data Prog = Prog [CDecl] deriving(Eq,Show)

-- | Class Declaration consists of the Class name + the class itself
data CDecl = ClassDecl (ClassName, ClassBody) deriving(Eq,Show)

-- | The class body itself consists out of the class name, a list field declarations and a list of method declarations
data ClassBody = ClassBody {superclass :: ClassName, fielddeclarations :: FDecls, methods :: MDecls} deriving(Eq,Show)

-- | Field declaration is a pair of Fieldname + type
type VName = String
type TypeField = String
data FDecl = FDecl (ClassName, VName) deriving(Eq,Show,Ord)
type FDecls = [FDecl] 

-- | Method declaration is a set of method name, list of parameters, return type and the method body
type MDecls = [MDecl]
data MDecl = MDecl (MethodName,ParMeths, RetMeth, MBody) deriving(Eq,Show)
data ParMeth = ParMeth (String,String) deriving(Eq,Show)
type ParMeths = [ParMeth] 
type RetMeth = String


-- | Method body for a jvm method
-- | jvm-method = nat x nat x instr list
type Mxs   = Int  -- Maximum size of stack
type Mxl   = Int  -- Maximum length of Registers
data MBody = MBody (Mxs, Mxl, ByteCode) deriving(Eq,Show)

-- | This function returns the List of each method (cn, parameters, return values, and the body)
method :: Prog -> Cn -> Mn -> [(Cn, ParMeths, RetMeth, MBody)]
method p cn mn = findlist
   where cbody      = (lookup cn (getCDecl(getCDecls p)))
         superclass = getSuperClass cbody
         findlist   = foldr (\x -> calc x) [] (getMDecl (getMDecls (cbody)))
                     where calc x = do
                                     let currmn = getMethodName x
                                     let methodbody = getMethodBody x
                                     let params = getParamMethod x
                                     let ret = getRetMethod x
                                     if (currmn == mn) then (++)[(superclass, params, ret, methodbody)] else (++)[] 

-- | Returns the class name of the superclass coming from the method output.
methodFirstElem :: (Cn, ParMeths, RetMeth, MBody) -> Cn
methodFirstElem (cn,_,_,_) = cn

-- | Returns the parameters from the method output
methodSecondElem :: (Cn, ParMeths, RetMeth, MBody) -> ParMeths
methodSecondElem (_,par,_,_) = par

-- | Returns the return values from the method output
methodFourthElem :: (Cn, ParMeths, RetMeth, MBody) -> MBody
methodFourthElem (_,_,_,mbody) = mbody

{- ------------- instructions ------------ -}
data Instr = Load Int
           | Store Int
           | Push AbsVal
           | New Cn
           | Getfield String Cn
           | Putfield String Cn
           | Checkcast Cn
           | Invoke String Int
           | Return
           | Pop
           | IAdd
           | ISub
           | Goto Int
           | CmpEq
           | CmpGeq
           | CmpNeq
           | BNot
           | BAnd
           | BOr
           | IfFalse Int
            deriving(Eq,Show,Ord)

-- | Returns true if Instruction is a Goto instruction
isGoto (Goto i) = if (isNegative i) then True else False
isGoto _ = False

-- | Returns true if a given integer is negative
isNegative i | i < 0 = True
             | otherwise =  False

-- | Returns the position to where goto is jumping
getGotoPos (Goto i) = i 

-- | Returns true if Instruction is a IfFalse instruction
isIfFalse (IfFalse i) = True
isIfFalse _ = False

-- | Returns the position to where IfFalse is jumping
getIfFalsePos (IfFalse i) = i

-- | Returns the special programm position (PC)
getSpecialPC cmd | (isGoto cmd) = getGotoPos cmd 
--                 | (isIfFalse cmd) = getIfFalsePos cmd
                 | otherwise = 9999999

-- | The whole ByteCode is a list of Instructions
type ByteCode = [Instr]

{- ################################################
 - ########### Class Declaration ##################
 - ################################################ -}

-- | Getter Functions
getCDecls :: Prog -> [CDecl]
getCDecls (Prog cdecl) = cdecl

-- | getSubClassRelation:: Prog -> SubClassRelation
-- | getSubClassRelation (Prog (_, scr)) = scr
getCDecl :: [CDecl] -> [(ClassName, ClassBody)]
getCDecl [] = []
getCDecl (ClassDecl (cn,classbody):cds) = (cn,classbody):getCDecl cds

-- | returns the ClassName for a given Class Declaration
getCName :: CDecl -> ClassName
getCName (ClassDecl (cn, _))=cn

-- | returns the ClassBody for a given Class Declaration
getCBody :: CDecl -> ClassBody
getCBody (ClassDecl (_, cb)) = cb

-- | returns the FieldDeclarations for a given programm and class name
getFDeclarations :: Prog -> ClassName -> FDecls
getFDeclarations p cn = lookup'
  where lookup' =  getFDecls(lookup cn (getCDecl(getCDecls p)))
 
-- | Returns the ClassName out of a ClassBody
getClassName :: ClassBody -> ClassName
getClassName (ClassBody cn _ _) = cn

-- | Returns the List Classnames+FieldNames from a given List of FDecls
getFDecl :: [FDecl] -> [(ClassName,VName)]
getFDecl [] = []
getFDecl (FDecl (classname,vname):cds) = (classname,vname):getFDecl cds

-- | Returns the Field Declarations of a given ClassBody
getFDecls :: Maybe ClassBody -> FDecls
getFDecls (Just (ClassBody _ fdecl _)) = fdecl
getFDecls Nothing = error "Class not found and so no FieldDeclarations returned (getFDecls)"

-- | Returns the Name of the SuperClass for a given ClassName
getSuperClass :: Maybe ClassBody -> ClassName
getSuperClass (Just(ClassBody dn _ _)) = dn
getSuperClass Nothing = error "Class not found and so no superclass returned (getSuperClass)"


{- get Bytecode helper function for a classname cn and method name mn -}
-- | Returns the Bytecode for a given classname and methodname
instr_of :: Prog -> ClassName -> MethodName -> ByteCode
instr_of p cn mn = findlist
  where findlist = foldr (\x -> calc x) [] (getMDecl (getMDecls (lookup cn (getCDecl(getCDecls p)))))
                     where calc x = do 
                                     let currmn = getMethodName x 
                                     if (currmn == mn) then (++)(getBytecode(getMethodBody x)) else (++)[]

{- ################################################
 - ########## Method Declaration ################## 
 - ################################################ -}

-- | Returns the Method declarations for a classbody
getMDecls :: Maybe ClassBody -> MDecls 
getMDecls (Just (ClassBody _ _ mdecls)) =  mdecls
getMDecls Nothing = error "Method not found (getMDecls)"

-- | Returns the list of method declarations
getMDecl :: [MDecl] -> [(MethodName,ParMeths,RetMeth,MBody)]
getMDecl [] = []
getMDecl ((MDecl (methodName, parMeths, retMeth, mBody)):cds) = (methodName,parMeths,retMeth,mBody):getMDecl cds

-- | Returns the Methodname of the Method declaration
getMethodName :: (MethodName,ParMeths,RetMeth,MBody) -> MethodName
getMethodName (mname,_,_,_) = mname

-- | Returns the parameters of a method
getParamMethod :: (MethodName,ParMeths,RetMeth,MBody) -> ParMeths
getParamMethod(_,par,_,_) = par

-- | Returns list of (partype,parname) out of [ParMeth]
getParMeth :: [ParMeth] -> [(String,String)]
getParMeth [] = []
getParMeth (ParMeth (partype,parname):cds) = (partype, parname):getParMeth cds

-- | Returns the return value of a method
getRetMethod :: (MethodName,ParMeths,RetMeth,MBody) -> RetMeth
getRetMethod(_,_,ret,_) = ret

{- ################################################
 - ########## Method Body #########################
 - ################################################ -}

-- | Returns the Methodbody of the Method declaration
getMethodBody :: (MethodName,ParMeths,RetMeth,MBody) -> MBody
getMethodBody (_,_,_,mbody) = mbody

-- | Returns the Bytecode of a given Method body
getBytecode :: MBody -> ByteCode
getBytecode (MBody (_,_,mbody)) = mbody

-- | Returns the MaxStack of a given MethodBody
getMaxStack :: MBody -> Mxs
getMaxStack (MBody (mxs,_,_)) = mxs

-- | Returns the max local vars of a given MethodBody
getMaxLocs :: MBody -> Mxl
getMaxLocs (MBody (_,mxl,_)) = mxl

{- end of : helper functions for getting bytecode of a given class and method -}

{- ############################################ -}
{- builds the subclassrelation out of a program -}
{- ############################################ -}

-- | Returns the SuperClass out of the ClassBody
getSuperClass' :: ClassBody -> ClassName 
getSuperClass' (ClassBody dn _ _) = dn

-- | Builds the SubClassRelation for a given Programm
buildSubClassRelation :: Prog -> SubClassRelation
buildSubClassRelation prog = foldr (\x -> insert x) Map.empty (getCDecls prog)
             where insert x= do
                              let cdecl    = x
                              let cbody    = getCBody cdecl
                              let cname    = getCName cdecl
                              let sclass   = getSuperClass' cbody
                              let f key new_value old_value = new_value++old_value
                              Map.insertWithKey f sclass [cname]

-- | Returns true if cn is subclass of dn
isSubClass :: SubClassRelation -> ClassName -> ClassName -> Bool 
isSubClass scr dn cn = if(Map.member dn scr) then elem cn (scr Map.! dn) else False

-- | Returns the super class out of a given class and subclass relation. 
-- | This is easy because in java you can only inherit from one class and multiple inheritation is not possible
getSupClass :: SubClassRelation -> ClassName -> ClassName 
getSupClass scr cn = if(supclass == []) then [] else supclass!!0
 where supclass = (foldr (\x -> if(elem cn (scr Map.! x))then (:)x else (++) []) [] (Map.keys scr))

-- | Returns the List of parent classes in the class hierarchy of a given class. 
-- | Function stops if it has reached the top level class object.
getParentClasses :: Prog -> [ClassName] -> ClassName -> [ClassName]
getParentClasses p pcl "Object" = pcl  
getParentClasses p pcl cn = getParentClasses p (pcl++[(getSupClass scr cn)]) (getSupClass scr cn) 
 where scr = buildSubClassRelation p
 

-- | This function searches upwards the class hierarchie and returns the most concrete same fatherclass of cn1 and cn2
-- | It is needed during the summarization when two objects needs to be summarized and they need to find the most concrete father class
searchUpwards :: Prog -> ClassName -> ClassName -> ClassName
searchUpwards p cn1' cn2' | ((getSupClass scr cn1) == (getSupClass scr cn2)) = (getSupClass scr cn1)
                          | (isSubClass scr cn1 cn2) = cn1
                          | (isSubClass scr cn2 cn1) = cn2
                          | otherwise = if(length cn1sclasses)>(length cn2sclasses)
                                        then (reverse(foldr(\x -> if(List.elem x cn2sclasses) then (:)x else (++)[]) [] cn1sclasses))!!0
                                        else (reverse(foldr(\x -> if(List.elem x cn1sclasses) then (:)x else (++)[]) [] cn2sclasses))!!0
 where cn1 = toUpperCn cn1'
       cn2 = toUpperCn cn2'
       scr = buildSubClassRelation p 
       cn1sclasses= reverse(getParentClasses p [] cn1)
       cn2sclasses= reverse(getParentClasses p [] cn2)

-- | Returns the Cn with First Letter capital (e.g list --> List)
toUpperCn :: String -> String
toUpperCn s | (s==[]) = s
            | (Char.isLower (head s)) = ((Char.toUpper(head s)):(tail s))
            | otherwise = s

{- end of : builds the subclassrelation out of a program -}
{- ##################################################### -}
