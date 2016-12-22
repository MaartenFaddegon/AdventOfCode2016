import Queue

main = print solution2

step [] (x:[]) = x
step ys []     = step [] (reverse ys)
step ys (x:[]) = step [x] (tail $ reverse ys)
step ys (x:xs) = step (x:ys) (tail xs)

ex1 = step [] [1..5]

solution1 = step [] [1..3014603]


-- go: a naive but quadratic implementation of part 2
go [x] = x
go xs  = go $ ys ++ (zs ++ [y])
  where i = (length xs) `div` 2
        (y:ys,_:zs) = splitAt i xs

ex2 = kill [1..5]
solution2 = kill [1..3014603]

kill xs = kill' m (queueFromList $ rotate (m `div` 2) xs) where m = length xs

kill' :: Int -> Queue Int -> Int
kill' 1 q = fst (queueHead q)
kill' m q | m `mod` 2 == 1 = kill' (m-1) (skip1 q')
          | otherwise      = kill' (m-1) q'
  where (_,q') = queueHead q


skip1 q = queue q' [x]
  where (x,q') = queueHead q

rotate m xs = zs ++ ys where (ys,zs) = splitAt m xs
