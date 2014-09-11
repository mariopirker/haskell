module Testing.DotTest where


import Testing.Dot
import Data.GraphViz.Exception

gnomes :: Gr String String
gnomes = mkGraph [(1, "Collect underpants"), (3, "Profit")] [(1, 3, "?")] :: Gr String String

graphToDotPng :: FilePath -> [(String,[String])] -> IO Bool
graphToDotPng fpre g = handle (\(e::GraphvizException) -> return False)
                       $ addExtension (runGraphviz (graphToDot g)) Png fpre >> return True

