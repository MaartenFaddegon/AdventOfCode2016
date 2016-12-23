import Data.List

data Direction   = Lft | Rght               deriving Show
data Value       = Letter Char
                 | Position Int
                 | Positions Int Int        deriving Show
data Instruction = Swap Value Value
                 | Move Value Value
                 | Rotate Direction Int
                 | RotateB Value
                 | Reverse Value            deriving Show

main = print solution2

solution1 = foldl eval "abcdefgh" instructions

-- There are only 8! = 40320 permutations of "abcdefgh", so we
-- can find the input by just trying all and comparing the output.
solution2 = filter (\s -> foldl eval s instructions == "fbgdceah") (permutations "abcdefgh")

-- An alternative solution that is faster (but was a bit more work to write)
-- is to reverse the instructions.
solution2' = foldl lave "fbgdceah" $ reverse instructions

-- inverse evaluation for part 2
lave s (Swap (Position i) (Position j)) = swapP i j s
lave s (Swap (Letter c)   (Letter d))   = swapL c d s
lave s (Rotate Lft i)                   = rot Rght i s
lave s (Rotate Rght i)                  = rot Lft i s
lave s (RotateB (Letter c))             = bRot c s
lave s (Reverse (Positions i j))        = rev i j s
lave s (Move (Position i) (Position j)) = mv j i s
lave _ i = error $ "lave undefined for instruction " ++ show i

-- eval for part 1
eval s (Swap (Position i) (Position j)) = swapP i j s
eval s (Swap (Letter c)   (Letter d))   = swapL c d s
eval s (Rotate d i)                     = rot d i s
eval s (RotateB (Letter c))             = rotB c s
eval s (Reverse (Positions i j))        = rev i j s
eval s (Move (Position i) (Position j)) = mv i j s
eval _ i = error $ "eval undefined for instruction " ++ show i

swapP i j s | i > j = swapP j i s
swapP i j s = as ++ d:(cs ++ b:ds)
  where
  (as, b:bs) = splitAt i s
  (cs, d:ds) = splitAt (j-i-1) bs

swapL c d = map swp
  where
  swp x | x == c = d
  swp x | x == d = c
  swp x          = x

rot Lft i s  = ys ++ xs where (xs,ys) = splitAt (i `mod` (length s)) s
rot Rght i s = rot Lft (length s - i) s

rotB c s = rot Rght (i+j+1) s
  where
  (Just i) = findIndex (==c) s
  j = if i > 3 then 1 else 0

bRot c s = rot Lft j s
  where
  (Just i) = findIndex (==c) s
  (Just j) = (lookup i lut)

lut = mkLut "abcdefgh"

mkLut s2 = map (\c -> let s = rotB c s2; (Just i) = findIndex (==c) s; (Just j) = findIndex (==c) s2 in (i,(i+8-j) `mod` 8)) s2

rev i j s | i > j = rev j i s
rev i j s = as ++ (reverse bs) ++ cs
  where
  (as, s') = splitAt i s
  (bs, cs) = splitAt (j-i+1) s'

mv i j s = cs ++ (b:ds)
  where
  (as, b:bs) = splitAt i s
  (cs,ds)    = splitAt j (as++bs)

ex0 = swapP 4 0 "abcde" == "ebcda"
ex1 = swapL 'd' 'b' "ebcda" == "edcba"
ex2 = rev 0 4 "edcba" == "abcde"
ex3 = rot Lft 1 "abcde" == "bcdea"
ex4 = mv 1 4 "bcdea" == "bdeac"
ex5 = mv 3 0 "bdeac" == "abdec"
ex6 = rotB 'b' "abdec" == "ecabd"
ex7 = rotB 'd' "ecabd" == "decab"

instructions = 
  [ Swap (Position 7) (Position 1)
  , Swap (Letter 'e') (Letter 'd')
  , Swap (Position 7) (Position 6)
  , Move (Position 4) (Position 0)
  , Move (Position 1) (Position 4)
  , Move (Position 6) (Position 5)
  , Rotate Rght 1
  , Swap (Letter 'e') (Letter 'b')
  , Reverse (Positions 3 7)
  , Swap (Position 2) (Position 6)
  , Reverse (Positions 2 4)
  , Reverse (Positions 1 4)
  , Reverse (Positions 5 7)
  , Rotate Lft 2
  , Swap (Letter 'g') (Letter 'f')
  , RotateB (Letter 'a')
  , Swap (Letter 'b') (Letter 'h')
  , Swap (Position 0) (Position 3)
  , Move (Position 4) (Position 7)
  , RotateB (Letter 'g')
  , Swap (Letter 'f') (Letter 'e')
  , Move (Position 1) (Position 5)
  , Swap (Letter 'd') (Letter 'e')
  , Move (Position 5) (Position 2)
  , Move (Position 6) (Position 5)
  , Rotate Rght 6
  , Rotate Lft 4
  , Reverse (Positions 0 3)
  , Swap (Letter 'g') (Letter 'c')
  , Swap (Letter 'f') (Letter 'e')
  , Reverse (Positions 6 7)
  , Move (Position 6) (Position 1)
  , Rotate Lft 2
  , Rotate Lft 5
  , Swap (Position 3) (Position 6)
  , Reverse (Positions 1 5)
  , Rotate Rght 6
  , Swap (Letter 'a') (Letter 'b')
  , Reverse (Positions 3 4)
  , RotateB (Letter 'f')
  , Swap (Position 2) (Position 6)
  , Reverse (Positions 5 6)
  , Swap (Letter 'h') (Letter 'e')
  , Reverse (Positions 0 4)
  , RotateB (Letter 'g')
  , RotateB (Letter 'd')
  , RotateB (Letter 'b')
  , Swap (Position 5) (Position 1)
  , RotateB (Letter 'f')
  , Move (Position 1) (Position 5)
  , Rotate Rght 0
  , RotateB (Letter 'e')
  , Move (Position 0) (Position 1)
  , Swap (Position 7) (Position 2)
  , Rotate Lft 3
  , Reverse (Positions 0 1)
  , Rotate Rght 7
  , Rotate Rght 5
  , Swap (Position 2) (Position 0)
  , Swap (Letter 'g') (Letter 'a')
  , Rotate Lft 0
  , RotateB (Letter 'f')
  , Swap (Position 5) (Position 1)
  , Rotate Rght 0
  , Rotate Lft 5
  , Swap (Letter 'e') (Letter 'a')
  , Swap (Position 5) (Position 4)
  , Reverse (Positions 2 5)
  , Swap (Letter 'e') (Letter 'a')
  , Swap (Position 3) (Position 7)
  , Reverse (Positions 0 2)
  , Swap (Letter 'a') (Letter 'b')
  , Swap (Position 7) (Position 1)
  , Move (Position 1) (Position 6)
  , Rotate Rght 1
  , Reverse (Positions 2 6)
  , RotateB (Letter 'b')
  , Move (Position 1) (Position 0)
  , Swap (Position 7) (Position 3)
  , Move (Position 6) (Position 5)
  , Rotate Rght 4
  , Reverse (Positions 2 7)
  , Reverse (Positions 3 4)
  , Reverse (Positions 4 5)
  , RotateB (Letter 'f')
  , Reverse (Positions 0 5)
  , Reverse (Positions 3 4)
  , Move (Position 1) (Position 2)
  , Rotate Lft 4
  , Swap (Position 7) (Position 6)
  , Rotate Rght 1
  , Move (Position 5) (Position 2)
  , Rotate Rght 5
  , Swap (Position 7) (Position 4)
  , Swap (Letter 'a') (Letter 'e')
  , RotateB (Letter 'e')
  , Swap (Position 7) (Position 1)
  , Swap (Position 7) (Position 3)
  , Move (Position 7) (Position 1)
  , Swap (Position 7) (Position 4)
  ]
