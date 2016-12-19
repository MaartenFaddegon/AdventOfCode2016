module Day17solution1 where
import Data.Hash.MD5
import Queue

data State     = S (Int,Int) String
data Direction = U | D | L | R       deriving Show

ex0 = directions "hijkl"

ex1 = search "ihgpwlah"

solution1 = search "dmypynyp"

search :: String -> String
search s = drop (length s) . bfs . queueFromList $ [S (0,0) s]

bfs :: Queue State -> String
bfs q | finalState s = p
      | otherwise    = bfs (queue q' ns)
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
