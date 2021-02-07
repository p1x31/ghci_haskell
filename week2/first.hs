f [] = []
f (x:xs) = f ys ++ [x] ++ f zs
  where ys = [a | a <- xs, a <= x]
        zs = [a | a <- xs, a > x]