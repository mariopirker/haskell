-- | file: ComputationGraph/PrettyState.hs
-- | author: Mario Pirker
-- | This module is for displaying the whole programm and also single states over a pretty printer
module ComputationGraph.PrettyState where

import ComputationGraph.State
import ComputationGraph.HelperFunctions
import ComputationGraph.Program
--import ComputationGraph.Instructions
import qualified Data.Map as Map
import Data.List

{-

-- | Writes the output of a whole program into a file called StatesOutput.txt
prettyProgram states prog cn mn = do 
                            let prStates = showProgram states prog cn mn
                            writeFile "StatesOutput.txt" prStates

-- | Writes the output of one state into a file called StateOutput.txt
prettyState state prog = do 
                          let prState = showState state prog
                          writeFile "StateOutput.txt" prState

-- | Displays the whole Programm
showProgram :: [[State]] -> Prog -> Cn -> Mn -> String 
showProgram s p cn mn = (showStartState p cn mn) ++ ("\n")++("\n") ++ showArrow ++ ("\n")++("\n") ++ program
 where program = (foldr(\x -> (++)((showStates x p) ++ ("\n") ++ showArrow ++ ("\n")++("\n"))) "" s )

-- | String for an Arrow between two states
showArrow = "                       | " ++ "\n" ++
            "                       | " ++ "\n" ++
            "                       | " ++ "\n" ++
            "                       v "

-- | Shows the startstate 
showStartState :: Prog -> Cn -> Mn -> String
showStartState p cn mn = showState startstate p
 where startstate = createStartState p cn mn

-- | Displays all states
showStates :: [State] -> Prog -> String
showStates s p = foldr(\x -> (++)((showState x p)++ ("\n")++("\n"))) "" s
-}

-- | Shows the State
showState :: State -> Prog -> String
showState s p = output
 where heap = getHeap (getHeaps s)
       frames = getFrames s
       stateId = getStateId s     
       annot = getAnnotations s
       

       output ="------------------------------------------------------------" ++ "\n" ++
               "State: s" ++  show stateId ++ "\n" ++ 
               "------------------------------------------------------------" ++ "\n" ++ 
                showFrames frames p ++ 
                "-------------" ++ "\n" ++
                showHeap heap ++ 
                "-------------" ++ "\n" ++
                showAnnot annot ++ "\n" ++
                "------------------------------------------------------------ " 


-- | Displays all the Frames
showFrames :: Frames -> Prog -> String 
showFrames frames p = foldr (\x -> (++)((showFrame (fst x) p)++"\n")) "" zipped
 where zipped = zip frames [1..]

-- | Displays a single Frame
showFrame :: Frame -> Prog -> String
showFrame f p = output   
  where stk = getStk f
        loc = getLoc f
        pc = getPc f 
        cn = getCn f
        mn = getMn f       
 
        output = 
           (showPc pc p cn mn) ++ " | " ++ showStk stk ++" | " ++ showLoc loc  
      
           
-- | Displays the Stk
showStk :: Stk -> String
showStk stk = if(stkstring=="") then "empty" else stkstring
 where stkstring = (foldr(\x -> (++)(showAbsVal x++if (x==(last stk)) then "" else ",")) "" stk)

-- | Displays the Registers
showLoc :: Loc -> String 
showLoc loc = (foldr(\x -> (++)("reg"++(show ((snd x)-1)) ++ "=" ++ showAbsVal (fst x) ++if ((snd x)==(last nums)) then "" else "," )) "" zipped)
 where zipped = zip loc nums
       nums = [1..(length loc)]

-- | Displays the Programm Counter
showPc :: Pc -> Prog -> ClassName -> MethodName -> String
showPc pc p cn mn = "PC: " ++ (show pc) ++ "(\"" ++ (showCommand p cn mn pc) ++"\")"

-- | Displays a command of the instruction list
showCommand :: Prog -> ClassName -> MethodName -> Int -> String
showCommand p cn mn pos = showInstr ((instr_of p cn mn)!!pos)

-- | Shows the instruction
showInstr :: Instr -> String
showInstr (Load i) = "Load " ++ show i
showInstr (Store i) = "Store " ++ show i
showInstr (Push a) = "Push " ++ showAbsVal a
showInstr (New cn) = "New " ++ cn
showInstr (Getfield s cn) = "Getfield " ++ s ++ " " ++ cn
showInstr (Putfield s cn) = "Putfield " ++ s ++ " " ++ cn
showInstr (Checkcast cn) = "Checkcast " ++ cn
showInstr (Invoke s i) = "Invoke " ++ s ++ (show i)
showInstr (Return) = "Return"
showInstr (Pop) = "Pop"
showInstr (IAdd) = "IAdd"
showInstr (ISub) = "ISub"
showInstr (Goto i) = "Goto " ++ (show i)
showInstr (CmpEq) = "CmpEq"
showInstr (CmpNeq) = "CmpNeq"
showInstr (CmpGeq) = "CmpGeq"
showInstr (BNot) = "BNot"
showInstr (BAnd) = "BAnd"
showInstr (BOr) = "BOr"
showInstr (IfFalse i) = "IFFalse " ++ (show i)

-- | Displays the class name
showCn :: Cn -> String
showCn cn = "Cn: " ++ cn

-- | Displays the method name
showMn :: Mn -> String
showMn mn = "Mn: " ++ mn

-- | Displays the Heap
showHeap :: Heap -> String 
showHeap h = output 
 where addresses = Map.keys h 
       objects = Map.elems h 

       output = foldr (\x -> (++)(showHeapEntry ((fst x),(snd x)) ++ "\n") ) "" zipped
       zipped = zip addresses objects 

-- | Displays one single Address for the Heap
showHeapEntry :: (Address, Object) -> String
showHeapEntry (Addr a,o) | isClassVar o = printClassVar o
                         | isObjPair o = printObjPair o                       
 where printClassVar x = "a"++(show a)++"="++(getClassVar x)  
       printObjPair x = "a"++(show a)++"="++(cn x)++"("++showFTable (ft x)++")"
        where cn x = fst (getObjPair x)
              ft x = snd (getObjPair x)

-- | Displays the ftable
showFTable :: Ft -> String
showFTable ft = foldr(\x -> (++)((fst x)++"."++(snd x)++"="++(showAbsVal (ft Map.! x))++if (x==(last ftkeys)) then "" else ",")) "" ftkeys
 where ftkeys = Map.keys ft 

-- | Displays the Annotations
showAnnot :: Iu -> String
showAnnot iu = if(iu==[]) then "empty" else annotstring
   where annotstring = foldr(\x -> (++)("a"++(show (fst x))++" != " ++ "a"++(show (snd x))++",")) "" iu

