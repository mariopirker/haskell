import qualified Data.Map as Map
import qualified Data.List as List

-- calculates the permutation
selections [] = []
selections (x:xs) = (x, xs) : [ (y, x:ys) | (y,ys) <- selections xs ]

permute [] = [[]]
permute xs = [y:ps | (y,ys) <- selections xs, ps <- permute ys]


wrapper list = foldr (\x y -> Map.insert (fst x) (snd x) y) Map.empty list

mapping xs ys = foldr (\x -> (++)(loop1 x)) [] xperm
 where loop1 elem1 = foldr(\x -> (++)(loop2 elem1 x)) [] yperm 
       loop2 elem1 elem2 = zipWith duple elem1 elem2
 
       xperm = (permute xs)
       yperm = (permute ys)

duple x s = (x,s) 
