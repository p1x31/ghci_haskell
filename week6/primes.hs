-- cross out every three

-- list on numbers from funtion
-- 2 3 -4 5 -6 7 -8 -9 -10
-- T T  T T  T T  T  T  T
-- cross in twos
-- T T  F T  F T  F  F  F
-- [][][][] [][] [] []  []
-- [] true
-- [] [] [2] [] [2.3] [] [2] [] [2]
-- put twos into every to
-- start with next emtpty tup three into every three
{-
Lazy evaluation - call by name + sharing
e.g. square x = x+x
square (fib 20) == fib 20 * fib 20 = _*_
SHARING
 square (fib 20) = x * x = _*_
where x = fib 20
replace w x with 10946
second time no evaluation

OCaml call by value: The argument of function is evaluated exactly once
Call by name:  " " " Is evaluated 0,1,2, or here
Call by need:  "" "" "" at most once
