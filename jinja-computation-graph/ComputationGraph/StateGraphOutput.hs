-- | file: ComputationGraph/StateGraphOutput.hs
-- | author: Mario Pirker
-- | This file is for doing a Pretty print on the stategraph
module ComputationGraph.StateGraphOutput (
prettySG,showStateGraph
) where

import ComputationGraph.HeapGraph
import ComputationGraph.StateGraph
import ComputationGraph.State
import ComputationGraph.HelperFunctions
import ComputationGraph.Program
--import ComputationGraph.Instructions
import qualified Data.Map as Map
import Data.List
--import Parser.Bcparser
import System.Environment (getArgs)

{- test code -}
{-
-- | Parses an input file, makes a programm out of it and then runs the Pretty StateGraph programm and writes it into a file called sgoutput.txt
testPrettySG inputFile className methodname= do
    input         <- readFile inputFile
    let parsedinp = parseString input
    let program   = Prog parsedinp
    let state     = createStartState program className methodname
    let heapio    = runPrettySG state
    writeFile "sgoutput.txt" heapio

-- | Parses the input file 
parseString s = (bcparse . lexer) s

{- end test code -}
-}
-- | Makes the Output string out of a given State
runPrettySG :: State -> String
runPrettySG state = prettySG sg 
  where sg   = mkStateGraph state

-- | Makes the Output string for a given StateGraph and HeapGraph
prettySG :: StateGraph -> String 
prettySG sg = showStateGraph sg 

-- | Displays the State and HeapGraph
showStateGraph :: StateGraph -> String 
showStateGraph s = output 
  where nodes       = getNodesSG s
        succs       = getSuccessorsSG s
        nlabel      = getNLabelsSG s
        elabel      = getELabelsSG s 
        output =
          "StateGraph: " ++ 
          (inLvl 1) ++ "Registers and Stack:  (_,frmi,ix)" ++
          "  "      ++ showRegStks nlabel ++ 
          (inLvl 1) ++ "HeapNodes: " ++
          " "       ++ showHeapNodesAddr nodes ++ "\n" ++
          "  "      ++ showHeapNodes nlabel ++
          (inLvl 1) ++ "AbsLocNodes: " ++
          "  "      ++ showAbsLocNodes nlabel ++
          (inLvl 1) ++ "Successors: " ++ 
          "  "      ++ showSuccs succs nlabel ++
--          (inLvl 1) ++ "Edge-Labels: " ++ 
          "  "      ++ showELabels elabel nlabel

-- | Show function for the Stk and Registers
showRegStks :: NodeLabelSG -> String 
showRegStks nlabel  = foldr (\x -> print x) "" regstk
 where  print x  = (++)((inLvl 3) ++ (node x))
        node x   = (showRegStk (nlabel Map.! x))
        regstk = foldr (\x -> if((isNLabelStk (nlabel Map.! x)==True)||(isNLabelLoc (nlabel Map.! x)==True))then (:)x else (++)[]) [] (Map.keys nlabel)

-- | Shows a given Stk or Loc Label
showRegStk :: LabelSG -> String
showRegStk (NLabelLoc l) = show l
showRegStk (NLabelStk l) = show l
showRegStk _             = "not an reg or stk"


-- | Shows an Address
showHeapLabel :: LabelSG -> String
showHeapLabel (NLabelHG(NLabel s)) = "Addr: " ++ show s

-- | Displays the addresses
showHeapNodesAddr :: NodesSG -> String
showHeapNodesAddr nodes = foldr (\x -> print x) "" heap
 where  print x  = (++)((inLvl 3) ++ (node x))
        node x   = (showAddr (getAddrNode (getHeapNode x)))
        heap     = foldr (\x -> if(isHeapNode x)then (:)x else (++)[]) [] (nodes)

-- | Show function for the heap nodes
showHeapNodes :: NodeLabelSG -> String
showHeapNodes nlabel  = foldr (\x -> print x) "" heap
 where  print x  = (++)((inLvl 3) ++ (node x))
        node x   = (showHeapLabel (nlabel Map.! x))
        heap     = foldr (\x -> if((isNLabelHG (nlabel Map.! x))==True)then (:)x else (++)[]) [] (Map.keys nlabel)

-- | Displays an address as a string
showAddr :: Address -> String
showAddr (Addr a) = "a"++show a

-- | Show AbsLocation Nodes
showAbsLocNodes :: NodeLabelSG -> String
showAbsLocNodes nlabel  = foldr (\x -> print x) "" heap
 where  print x  = (++)((inLvl 3) ++ (node x))
        node x   = (showAbsLocNodeLabel (nlabel Map.! x))
        heap     = foldr (\x -> if((isNLabelAbsLoc (nlabel Map.! x))==True)then (:)x else (++)[]) [] (Map.keys nlabel)

-- | Disprays and Abstract Locatiom Node Label
showAbsLocNodeLabel :: LabelSG -> String 
showAbsLocNodeLabel (NLabelAbsLoc a) = "AbsLoc: " ++ showAbsVal a

-- | Show functions for successors
showSuccs :: SuccessorsSG -> NodeLabelSG -> String
showSuccs succs nlabel = foldr (\x -> print x) "" (Map.keys succs)
  where print x  = (++)((inLvl 3) ++ (node x) ++ " --> " ++ (succes x))
        node x   = (showNLabel (nlabel Map.! x))
        succes x = (foldl (++) "" (intersperse ", " ((map (\x-> (showSucc x nlabel)) (succs Map.! x)))))

showSucc :: NodeSG -> NodeLabelSG -> String
showSucc (HeapNode s) nlabel = showAddr(getAddrNode s)  
showSucc (NAddrNode s) nlabel = "absloc "++show((nlabel Map.! (NAddrNode s)))


showNLabel :: LabelSG -> String
showNLabel (NLabelStk s)      = show s
showNLabel (NLabelLoc s)      = show s
showNLabel (NLabelAbsLoc s)   = "AbsLoc: " ++ showAbsVal s
showNLabel (NLabelHG s)       = showHeapLabel (NLabelHG s)

-- | Show function for the Edge Label
showELabel :: LabelSG -> String
showELabel (NLabelHG(ELabel e)) = showELabel' e

showELabel' :: (Dn,Fieldid) -> String
showELabel' (dn,fieldid) = "(" ++ show dn ++ "," ++ show fieldid ++ ")"

showELabels :: EdgeLabelSG -> NodeLabelSG -> String
showELabels elabels nlabel = foldr (\x -> print x) "" (Map.keys elabels)
  where print x = (++)((inLvl 3) ++ (edge x) ++ " hasLabel " ++ (labelEd x))
        edge x  = "("++(showNLabelEd (nlabel Map.! (fst x)))++","++(showSucc (snd x) nlabel)++")"
        labelEd x = showELabel (elabels Map.! x)

showNLabelEd :: LabelSG -> String
showNLabelEd (NLabelAbsLoc a) = "NonAddr: " ++ showAbsVal a
showNLabelEd (NLabelHG(NLabel s)) = "Addr: " ++ s

