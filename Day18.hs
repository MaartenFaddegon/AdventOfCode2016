import Data.IntMap.Strict (IntMap,(!))
import qualified Data.IntMap.Strict as IntMap

data Tile = S  -- safe tile
          | T  -- trap
          deriving (Show, Eq)

main = do
  print solution1
  print solution2

solution1 = countSafe . take 40 $ room input
solution2 = countSafe . take 400000 $ room input

input = parse "^.^^^.^..^....^^....^^^^.^^.^...^^.^.^^.^^.^^..^.^...^.^..^.^^.^..^.....^^^.^.^^^..^^...^^^...^...^."

ex0 = putStr . unlines . map show . take 3 $ room ex0input
ex0input = parse "..^^."

ex1 = print . countSafe . take 3 $ room ex0input

countSafe = length . filter (==S) . concat

parse []      = []
parse ('.':t) = S : parse t
parse ('^':t) = T : parse t

room r0 = iterate row r0

row prev = row' (S : prev ++ [S])

row' (l:c:r:t) = tile l c r : row' (c:r:t)
row' xs | length xs < 3 = []


-- A tile is a trap only in one of the following situations:
-- 
--     Its left and center tiles are traps, but its right tile is not.
--     Its center and right tiles are traps, but its left tile is not.
--     Only its left tile is a trap.
--     Only its right tile is a trap.

tile T T S = T
tile S T T = T
tile T S S = T
tile S S T = T
tile _ _ _ = S
