
factors n = [x | x <- [1..n], n `mod` x == 0]

prime n = factors n == [1,n]

prime' n = length(factors n) == 2

primesUpto n = [p | p <- [1..n], prime p]

primesUpto' n = filter (\x -> factors x == [1,x]) [1..n]

primesUpto2 n = filter prime [1..n]