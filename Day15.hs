data Disk = D Int Int Int

main = do
  print $ solution1
  print $ solution2

solution1 = search input
solution2 = search $ input ++ [(D 7 11 0)]

search :: [Disk] -> Int
search ds = search' 0
  where 
  search' t | all (found t) ds = t
            | otherwise        = search' (t+1)

found :: Int -> Disk -> Bool
found t (D d m p) = ((p+t+d) `mod` m) == 0


-- Disc #1 has 17 positions; at time=0, it is at position 1.
-- Disc #2 has 7 positions; at time=0, it is at position 0.
-- Disc #3 has 19 positions; at time=0, it is at position 2.
-- Disc #4 has 5 positions; at time=0, it is at position 0.
-- Disc #5 has 3 positions; at time=0, it is at position 0.
-- Disc #6 has 13 positions; at time=0, it is at position 5.
input =
  [ D 1 17 1
  , D 2  7 0
  , D 3 19 2
  , D 4  5 0
  , D 5  3 0
  , D 6 13 5
  ]
