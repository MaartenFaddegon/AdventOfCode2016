module Day19 where

step [] (x:[]) = x
step ys []     = step [] (reverse ys)
step ys (x:[]) = step [x] (tail $ reverse ys)
step ys (x:xs) = step (x:ys) (tail xs)

ex1 = step [] [1..5]

solution1 = step [] [1..3014603]


-- Part 2 is quadratic due to repeatedly finding the middle
-- see the C solution with a midpoint-cursor in a circualar linked list.
-- There are some fancy structures in Haskell that can get close (time log n)
-- but in this case maybe we should just recognize that C is the right tool 
-- for the job here.

go [x] = x
go xs  = go $ ys ++ (zs ++ [y])
  where i = (length xs) `div` 2
        (y:ys,_:zs) = splitAt i xs

ex2 = go [1..5]
solution2 = go [1..3014603]
