module Testing.GraphVizTest where

import Data.Graph.Inductive
import Data.GraphViz

------------------------------------------------

gnomes :: Gr String String
gnomes = mkGraph [(1, "Collect underpants"), (3, "Profit")] [(1, 3, "?")] :: Gr String String

convGrToDotGraph g = graphToDot nonClusteredParams g 


printToFile fp g = runGraphviz (convGrToDotGraph g) DotOutput fp

