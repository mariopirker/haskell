-- file: ComputationGraph/PrettyUnification.hs
-- author: Mario Pirker
-- This module is calulating the Unification and then writes it into a file so you can see what address is unifying and what not
module ComputationGraph.PrettyUnification where

import ComputationGraph.State
import ComputationGraph.HelperFunctions
import ComputationGraph.Program
import ComputationGraph.Instructions
import Testing.UnificationTest
import qualified Data.Map as Map
import Data.List

-- | Writes the Unification Result into a file. 
prettyUnificationResult prog state addr n = do
                                             let result = putfieldTest prog state addr n  
                                             let prResult = showUnifResult result
                                             writeFile "UnificationResult.txt" prResult

-- | Prints all the Unification Results
showUnifResult :: [(Bool,(Address,Address))] -> String
showUnifResult result = foldr (\x -> (++)((showResult x)++ "\n")) "" result

-- | Prints one Unification Result
showResult :: (Bool, (Address,Address)) -> String
showResult (b, (Addr a1,Addr a2)) = if(b==True)
                                    then "a"++show a1++" unifys with "++"a"++show a2
                                    else "a"++show a1++" does not unify with "++"a"++show a2



