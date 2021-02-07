{- Chapter 2. All exercises in Section 2.7(1-5) -}
double x = x + x

quadruple x = double (double x)

-- Factorial of a positive integer:
factorial n = product [1..n]

-- Average of a list of integers:
average ns = sum ns `div` length ns

a = b + c
  where
        b = 1
        c = 2
d = a * 2

{- section 2
          (2^3*4)
          (2*3)+(4*5)
          2+(3*(4^5))
-}

n = a `div` length xs
  where
        a = 10
        xs = [1,2,3,4,5]
-- *** Exception: Prelude.!!: index too large Brackets are !important
last' ns = ns !! ((length ns)-1)

last2 ns = head (reverse ns)

init' ns = take ((length ns)-1) ns

init2 ns = reverse ( tail (reverse ns) )

--Book solution

last3 xs = xs !! (length xs - 1)

{- Chapter 3. All exercises in Section 3.11(1-5)-}

{- exercise 1

['a','b','c'] :: [Char]
('a','b','c') :: (Char, Char, Char)
[(False,'0'),(True,'1')] :: [(Bool, Char)]
([False, True],['0','1']) :: ([Bool], [Char])
[tail,init,reverse] :: [[a] -> [a]]
 -}
 -- exercise 2

bools = [True,True,False]

nums = [[1,2,3],[12,3],[1,23]]

add x y z = x+y+z

copy x = (x,x)

apply f a = f a

-- exercise 3
{-
second xs = head (tail xs)
second :: [a] -> a
swap :: (a,b) -> (b,a)
pair :: Int -> Int -> (a,b)
double :: a -> a
palindrome :: [a] -> [b] - > Bool
twice :: (a -> b) ->  a -> b
-}
--exercese 4
{-
*Main> :type second
second :: [a] -> a
*Main> swap (x,y) = (y,x)
*Main> :type swap
swap :: (t1, t) -> (t, t1)
*Main> pair x y = (y,x)
*Main> :type pair
pair :: t1 -> t -> (t, t1)
*Main> double x = x*2
*Main> :type double
double :: Num a => a -> a
*Main> palindrome xs = reverse xs == xs
*Main> :type palindrome
palindrome :: Eq a => [a] -> Bool
*Main> twice f x = f (f x)
*Main> :type twice
twice :: (t -> t) -> t -> t
-}
--exercise 5
{-
Hint:two functions of the same type are equal if they always return equal results for equal arguments

1.Because not all functions are polymorphic functions
2.It is fesible for all basic types or when function can encode or decode data
 -}
{-

Chapter 4. Exercises 4.8(1-4). For further understanding, do (5-8)

Chapter 5. Exercises 5.7(1-5). For further understanding, do (6-10) -}