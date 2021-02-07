-- See dicttree-verbose.hs for explanations.

without :: Eq a => [a] -> a -> [a]
xs `without` y = [x | x <- xs, x /= y]

rmdup :: Eq a => [a] -> [a]
rmdup [] = []
rmdup (x:xs) = x:rmdup(xs `without` x)

values :: Eq a => [(a, b)] -> a -> [b]
values es x = [y | (x', y) <- es, x' == x]

-- Two tree equal if both starts with the ex: EndOr
-- Compare pairs for equality
-- accepts the same strings
-- strings that generates list and remove duplicates to check if
-- two tree are equal
data Tree = EndOr Tree | Branch [(Char,Tree)] deriving Show

strings :: Tree -> [String]
strings (EndOr t) = "" : strings t
strings (Branch es) = [x:xs | (x,t) <- es, xs <- strings t]

has :: Tree -> String -> Bool
(EndOr t)   `has` "" = True
(EndOr t)   `has` xs = t `has` xs
(Branch es) `has` "" = False
(Branch es) `has` (x:xs) = or [t `has` xs | t <- values es x]

predict :: Tree -> String -> [String]
predict t xs = [xs ++ ys | ys <- strings(subtree t xs)]
{- 
if this holds:  pre-condition: promist about the input
then this holds:  post-condition: promise about the output
correctness of the algorithm
-}
subtree :: Tree -> String -> Tree
subtree (EndOr t)    "" = t
subtree (EndOr t)    xs = subtree t xs
subtree (Branch es)  "" = Branch es
subtree (Branch es)  (x:xs) = head [subtree t xs | t <- values es x]

-- This will work properly for "good" trees, where I consider a tree
-- to be good, if in any (Branch es), the same label doens't occur
-- more than once.

size :: Tree -> Int
size (EndOr t) = 1 + size t
size (Branch ts) = sum [size t | (c,t) <- ts]

height :: Tree -> Int
height (EndOr t) = height t
height (Branch []) = 0
height (Branch ts) = maximum [1+height t | (c,t) <- ts]

-- This constructs good trees, provided the input list is sorted:
-- eg. factorial fuction with negative input
tree :: [String] -> Tree
tree xss | "" `elem` xss = EndOr t
         | otherwise     = t
 where
-- lots of times 'A' 'B' remove duplicates from that list
   -- defined locally in the where clause
  heads = rmdup [x | (x:xs) <- xss]
-- list comprehension within list comprehension
  startingWith h = ([xs | (x:xs) <- xss, x == h])
-- look at the time
-- ward starting with diffirent letters
  t = Branch [(h, tree (startingWith h)) | h <- heads]

{-
How Eq class is defined
if defines equals automatially defenition of not equals by negation
Interfaces with the default methods 
-}
class Eq a where
  (==), (/=) :: a -> a -> Bool
  x /= y = not (x ==y)

instance Eq Bool whre
  False == False == True
  True == True = True
 _    == _ = True

{-
in order to define Ord a is defined in Eq
-}
