import Data.Hash.MD5
import Queue

data State     = S (Int,Int) String
data Direction = U | D | L | R       deriving Show

main = print solution2

ex1 = search "ihgpwlah"

solution2 = search "dmypynyp"

search :: String -> Int
search s = (bfs (queueFromList [S (0,0) s]) 0) - (length s)

bfs :: Queue State -> Int -> Int
bfs q x | queueEmpty q = x
        | finalState s = bfs q' (length p)
        | otherwise    = bfs (queue q' ns) x
  where 
  (s, q') = queueHead q
  (S _ p) = s
  ns      = filter onGrid . map (applyDirection s) . directions $ p

finalState (S (3,3) _) = True
finalState _           = False

onGrid (S (x,y) _) = x >= 0 && x < 4 && y >= 0 && y < 4

applyDirection (S (x,y) s) U = S (x,y-1) $ s ++ "U"
applyDirection (S (x,y) s) D = S (x,y+1) $ s ++ "D"
applyDirection (S (x,y) s) L = S (x-1,y) $ s ++ "L"
applyDirection (S (x,y) s) R = S (x+1,y) $ s ++ "R"

directions :: String -> [Direction]
directions s = map fst . filter (open . snd) . zip [U,D,L,R] . md5s $ Str s

open 'b' = True
open 'c' = True
open 'd' = True
open 'e' = True
open 'f' = True
open _   = False
