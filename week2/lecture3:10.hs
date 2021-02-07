factors n =
        [x | x <- [1..n], n `mod` x == 0]

prime n = factors n == [1,n]

prime` n = length(factors n) == 2