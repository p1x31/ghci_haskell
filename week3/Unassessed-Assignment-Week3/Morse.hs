module Morse where
import MorseLib

codeWord :: String -> [MorseUnit]
codeWord [] = []
codeWord (x:xs) = (codeSymbol x) ++ shortGap ++ (codeWord xs)

codeText :: [String] -> [MorseUnit]
codeText [] = []
codeText (x:[]) = (codeWord x) 
codeText (x:xs) = (codeWord x) ++ mediumGap ++ (codeText xs)

encode :: String -> [MorseUnit]
--encode [] = []
encode xs = (codeText (words xs))

{-decode :: [MorseUnit] -> String-}
{-
toTree :: MorseTable -> MorseTree
toTree [] = Nil
totree xss = Branch1 (toTree yss) (toTree zss)
  where
    yss = [ys | (y:ys) <- xss]
    zss = [zs | (z:zs) <- xss]
I'm stuck here
to consturct tree from morse table
pattern match
Nil -> root of the tree
dit goes on the left
dah goes on the right
Branch1 with a char
Branch0 no prefix?
-}
toTree :: MorseTable -> MorseTree
toTree [] = Nil
toTree [([], c)] = Leaf c
toTree t = case cs of
             []  -> Branch0 (toTree zss) (toTree yss)
             [c] -> Branch1 c (toTree zss) (toTree yss)
  where
    cs = [c | ([], c) <- t]
    yss = takeDah t
    zss = takeDit t

takeDit :: MorseTable -> MorseTable
takeDit t = [(drop 2 m, c) | (m, c) <- t, take 2 m == dit]

takeDah :: MorseTable -> MorseTable
takeDah t = [(drop 4 m, c) | (m, c) <- t, take 4 m == dah]
