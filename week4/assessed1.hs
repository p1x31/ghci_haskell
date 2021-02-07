
import Data.Function (on)
import Data.List (sortBy)
import Data.Char

--sortBy (compare `on` snd) [...]

-- frequency
-- string -> [(Char,Int)]

without :: Eq a => [a] -> a -> [a]
xs `without` y = [x | x <- xs, x /= y]

rmdup :: Eq a => [a] -> [a]
rmdup [] = []
rmdup (x:xs) = x:rmdup(xs `without` x)

frequency :: [Char] -> [(Char,Int)] 
frequency xs = [(y,count y xs) | y <- rmdup xs]

count :: Char -> [Char] -> Int
count c xs = sum [1 | x <- xs , c == x] 
{-
ordfreq :: [(Char,Int)] -> [(Char,Int)]
--ordfreq [()] -> [()]
--ordfreq xs = [  | (_,y) <- ordfreq xs]
ordfreq xs = [ sortBy (snd xs) | y <- ordfreq xs]
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  
-}
getDigit :: Int -> String
getDigit (x:xs) = if (isDigit x) then x:(getDigit xs)
else []
{- write a recursive function that gets the characters while they are digits and
stops once you reach the start of the tree.

How to parse tree?
Sequence? take the first n chars and read them with read into a tree type

-}
