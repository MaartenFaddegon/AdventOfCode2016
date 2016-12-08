module Day08 where
import Data.List

data Pixel = Off | Lit deriving Eq
type Screen = [Pixel]

data Rot = Row | Column

instance Show Pixel where
  show Off = "."
  show Lit = "#"

printScreen :: Screen -> IO ()
printScreen = putStrLn . unlines . map (concat . map show) . toRows

w = 50
h = 6

take' 0 _ _  = []
take' n m xs = take m xs : take' (n-1) m (drop m xs)

s0 :: Screen
s0 = take (h*w) (repeat Off)

toRows,toCols :: Screen -> [[Pixel]]
toRows = take' h w
toCols = transpose . toRows

fromRows,fromCols :: [[Pixel]] -> Screen
fromRows = concat
fromCols = fromRows . transpose

rect :: Int -> Int -> Screen -> Screen
rect x y = fromRows . rect' x y . toRows
rect' _ 0 rs     = rs
rect' x y (r:rs) = r' : rect' x (y-1) rs
  where 
  r' = (take x $ repeat Lit) ++ (drop x r)

update fn 0 (y:ys) = fn y : ys
update fn x (y:ys) = y : update fn (x-1) ys

rot n zs = ys ++ xs
  where
  (xs,ys) = splitAt n zs

rotate Column x n = fromCols . update (rot $ h - n) x . toCols
rotate Row    x n = fromRows . update (rot $ w - n) x . toRows

howManyLit = length . filter (==Lit)

ex1 = rotate Column 1 1 . rect 3 2 $ s0
ex2 = rotate Row 0 4 . rotate Column 1 1 . rect 3 2 $ s0
ex3 = rotate Column 1 1 . rotate Row 0 4 . rotate Column 1 1 . rect 3 2 $ s0

solution1 = howManyLit $ eval input

eval :: [(Screen -> Screen)] -> Screen
eval = foldl ap s0
  where
  ap s f = f s

input =
  [ rect 1 1
  , rotate Row 0 5
  , rect 1 1
  , rotate Row 0 5
  , rect 1 1
  , rotate Row 0 5
  , rect 1 1
  , rotate Row 0 5
  , rect 1 1
  , rotate Row 0 2
  , rect 1 1
  , rotate Row 0 2
  , rect 1 1
  , rotate Row 0 3
  , rect 1 1
  , rotate Row 0 3
  , rect 2 1
  , rotate Row 0 2
  , rect 1 1
  , rotate Row 0 3
  , rect 2 1
  , rotate Row 0 2
  , rect 1 1
  , rotate Row 0 3
  , rect 2 1
  , rotate Row 0 5
  , rect 4 1
  , rotate Row 0 5
  , rotate Column 0 1
  , rect 4 1
  , rotate Row 0 10
  , rotate Column 5 2
  , rotate Column 0 1
  , rect 9 1
  , rotate Row 2 5
  , rotate Row 0 5
  , rotate Column 0 1
  , rect 4 1
  , rotate Row 2 5
  , rotate Row 0 5
  , rotate Column 0 1
  , rect 4 1
  , rotate Column 40 1
  , rotate Column 27 1
  , rotate Column 22 1
  , rotate Column 17 1
  , rotate Column 12 1
  , rotate Column 7 1
  , rotate Column 2 1
  , rotate Row 2 5
  , rotate Row 1 3
  , rotate Row 0 5
  , rect 1 3
  , rotate Row 2 10
  , rotate Row 1 7
  , rotate Row 0 2
  , rotate Column 3 2
  , rotate Column 2 1
  , rotate Column 0 1
  , rect 4 1
  , rotate Row 2 5
  , rotate Row 1 3
  , rotate Row 0 3
  , rect 1 3
  , rotate Column 45 1
  , rotate Row 2 7
  , rotate Row 1 10
  , rotate Row 0 2
  , rotate Column 3 1
  , rotate Column 2 2
  , rotate Column 0 1
  , rect 4 1
  , rotate Row 2 13
  , rotate Row 0 5
  , rotate Column 3 1
  , rotate Column 0 1
  , rect 4 1
  , rotate Row 3 10
  , rotate Row 2 10
  , rotate Row 0 5
  , rotate Column 3 1
  , rotate Column 2 1
  , rotate Column 0 1
  , rect 4 1
  , rotate Row 3 8
  , rotate Row 0 5
  , rotate Column 3 1
  , rotate Column 2 1
  , rotate Column 0 1
  , rect 4 1
  , rotate Row 3 17
  , rotate Row 2 20
  , rotate Row 0 15
  , rotate Column 13 1
  , rotate Column 12 3
  , rotate Column 10 1
  , rotate Column 8 1
  , rotate Column 7 2
  , rotate Column 6 1
  , rotate Column 5 1
  , rotate Column 3 1
  , rotate Column 2 2
  , rotate Column 0 1
  , rect 14 1
  , rotate Row 1 47
  , rotate Column 9 1
  , rotate Column 4 1
  , rotate Row 3 3
  , rotate Row 2 10
  , rotate Row 1 8
  , rotate Row 0 5
  , rotate Column 2 2
  , rotate Column 0 2
  , rect 3 2
  , rotate Row 3 12
  , rotate Row 2 10
  , rotate Row 0 10
  , rotate Column 8 1
  , rotate Column 7 3
  , rotate Column 5 1
  , rotate Column 3 1
  , rotate Column 2 1
  , rotate Column 1 1
  , rotate Column 0 1
  , rect 9 1
  , rotate Row 0 20
  , rotate Column 46 1
  , rotate Row 4 17
  , rotate Row 3 10
  , rotate Row 2 10
  , rotate Row 1 5
  , rotate Column 8 1
  , rotate Column 7 1
  , rotate Column 6 1
  , rotate Column 5 1
  , rotate Column 3 1
  , rotate Column 2 2
  , rotate Column 1 1
  , rotate Column 0 1
  , rect 9 1
  , rotate Column 32 4
  , rotate Row 4 33
  , rotate Row 3 5
  , rotate Row 2 15
  , rotate Row 0 15
  , rotate Column 13 1
  , rotate Column 12 3
  , rotate Column 10 1
  , rotate Column 8 1
  , rotate Column 7 2
  , rotate Column 6 1
  , rotate Column 5 1
  , rotate Column 3 1
  , rotate Column 2 1
  , rotate Column 1 1
  , rotate Column 0 1
  , rect 14 1
  , rotate Column 39 3
  , rotate Column 35 4
  , rotate Column 20 4
  , rotate Column 19 3
  , rotate Column 10 4
  , rotate Column 9 3
  , rotate Column 8 3
  , rotate Column 5 4
  , rotate Column 4 3
  , rotate Row 5 5
  , rotate Row 4 5
  , rotate Row 3 33
  , rotate Row 1 30
  , rotate Column 48 1
  , rotate Column 47 5
  , rotate Column 46 5
  , rotate Column 45 1
  , rotate Column 43 1
  , rotate Column 38 3
  , rotate Column 37 3
  , rotate Column 36 5
  , rotate Column 35 1
  , rotate Column 33 1
  , rotate Column 32 5
  , rotate Column 31 5
  , rotate Column 30 1
  , rotate Column 23 4
  , rotate Column 22 3
  , rotate Column 21 3
  , rotate Column 20 1
  , rotate Column 12 2
  , rotate Column 11 2
  , rotate Column 3 5
  , rotate Column 2 5
  , rotate Column 1 3
  , rotate Column 0 4
  ]
