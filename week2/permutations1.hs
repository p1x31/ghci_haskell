import qualified Data.List (permutations)

import Data.List (delete)

main = mapM_ print (permutations [1,2,3])

permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [ x:ys | x <- xs, ys <- Data.List.permutations (delete x xs)]