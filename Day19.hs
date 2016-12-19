step [] (x:[]) = x
step ys []     = step [] (reverse ys)
step ys (x:[]) = step [x] (tail $ reverse ys)
step ys (x:xs) = step (x:ys) (tail xs)

ex1 = step [] [1..5]

solution1 = step [] [1..3014603]

go [x] = x
go xs  = go $ ys ++ (zs ++ [y])
  where i = (length xs) `div` 2
        (y:ys,_:zs) = splitAt i xs

ex2 = go [1..5]
solution2 = go [1..3014603]

main = print solution2

data L = L L Int | E 
  deriving Show

delete (L (L n y) x,fst) = (L n 9,fst)

next (L E y,fst) = fst
next (L n y,fst) = n

nextN n l = iter n next l

iter 0 f x = x
iter n f x = iter (n-1) f (f x)

fromList :: [Int] -> L
fromList []     = E
fromList (x:xs) = L (fromList xs) x

toList 0 l = []
toList y (L n x) = x : toList (y-1) n
