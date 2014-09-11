module HeapGraphOutput (
prettyHG
) where

import ComputationGraph.HeapGraph
import ComputationGraph.State
import ComputationGraph.HelperFunctions
import ComputationGraph.Program
import ComputationGraph.Instructions
import qualified Data.Map as Map
import Data.List
import Parser.Bcparser
import System.Environment (getArgs)

{- test code -}

testPrettyHG inputFile className methodname= do
    input         <- readFile inputFile
    let parsedinp = parseString input
    let program   = Prog parsedinp
    let state     = createStartState program className methodname
    let heapio    = runPrettyHG state
    writeFile "hgoutput.txt" heapio

parseString s = (bcparse . lexer) s

{- end test code -}

runPrettyHG :: State -> String
runPrettyHG state = prettyHG hg
  where hg   = mkHeapGraph heap
        heap = (getHeaps state)

prettyHG :: HeapGraph -> String 
prettyHG hg = showHeapGraph hg

showHeapGraph :: HeapGraph -> String 
showHeapGraph h = output 
  where nodes  = getNodesHG h
        succs  = getSuccessorsHG h
        nlabel = getNLabelsHG h
        elabel = getELabelsHG h 
        output =
          "HeapGraph: " ++ 
          (inLvl 1) ++ "Nodes: " ++ 
          "  "      ++ showNodes nlabel ++  
          (inLvl 1) ++ "Successors: " ++ 
          "  "      ++ showSuccs succs nlabel ++
          --(inLvl 1) ++ "Node-Labels: " ++ 
          --(inLvl 2) ++ showNLabels nlabel ++ 
          (inLvl 1) ++ "Edge-Labels: " ++ 
          "  "      ++ showELabels elabel nlabel

-- | Show functions for Nodes
showNodes :: NodeLabelHG -> String 
--showNodes nodes = foldl (++) "" (intersperse (inLvl 3) (map showNabel (Map.keys))
showNodes nlabel = foldr (\x -> print x) "" (Map.keys nlabel)
 where  print x  = (++)((inLvl 3) ++ (node x))
        node x   = (showNLabel (nlabel Map.! x))

showNode :: NodeHG -> String
showNode (AddrNode a) =  showAddr a 
showNode (NonAddrNode a) = show a 

-- | Show function for abstract values
showAbsVal :: AbsVal -> String
showAbsVal (AddVal a)    = showAddr a
showAbsVal (IntVal i)    = show i
showAbsVal (BoolVal b )  = show b
showAbsVal (Null)        = "Null"
showAbsVal (Unit)        = "Unit"
showAbsVal (AbsIntVal i) = show i
showAbsVal (AbsBoolVal b)= show b
showAbsVal (AbsLocation a)=show a
showAbsVal (AbsClassVar a)=a

showAddr :: Address -> String 
showAddr (Addr a) = show a

-- | Show function for successors
showSuccs :: SuccessorsHG -> NodeLabelHG -> String 
showSuccs succs nlabel = foldr (\x -> print x) "" (Map.keys succs)
  where print x  = (++)((inLvl 3) ++ (node x) ++ " --> " ++ (succes x))
        node x   = (showNLabel (nlabel Map.! x))
        succes x = (foldl (++) "" (intersperse ", " ((map showSucc (succs Map.! x)))))

showSucc :: SuccessorNodesHG -> String
showSucc (SuccNodeAbsVal s) = show s
showSucc (SuccNodeAbsLoc s) = show s

-- | Show function for node labels
showNLabels :: NodeLabelHG -> String
showNLabels nlabels = foldl (++) "" (intersperse (inLvl 3) (map showNLabel (Map.elems nlabels)))

showNLabel :: LabelHG -> String
showNLabel (NLabelAbs a) = "NonAddr: " ++ showAbsVal a
showNLabel (NLabel s) = "Addr: " ++ s


-- | Show function for edge labels
showELabel :: LabelHG -> String
showELabel (ELabel e) = showELabel' e
showELabel (ELabelZ e) = show e

showELabel' :: (Dn,Fieldid) -> String
showELabel' (dn,fieldid) = "(" ++ show dn ++ "," ++ show fieldid ++ ")"

showELabels :: EdgeLabelHG -> NodeLabelHG -> String
showELabels elabels nlabel = foldr (\x -> print x) "" (Map.keys elabels)
  where print x = (++)((inLvl 3) ++ (edge x) ++ " hasLabel " ++ (labelEd x))
        edge x  = "("++(showNLabel (nlabel Map.! (fst x)))++","++(showSucc (snd x))++")"
        labelEd x = showELabel (elabels Map.! x)

--showELabels elabels = foldl (++) "" (intersperse (inLvl 3) (map showELabel (Map.elems elabels)))

inLvl x = '\n':mult ' ' x
    where
        mult c n
            | n > 0 = c : mult c (n-1)
            | otherwise = []
