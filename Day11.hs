import Data.IntMap.Strict (IntMap,(!))
import qualified Data.IntMap.Strict as IntMap
import Data.List

data Isotope = H | L | T | Pl | St | Pr | R                  deriving (Show,Eq,Ord)
data Thing   = M Isotope | G Isotope                         deriving (Show,Eq,Ord)
data State   = S {elevator :: Int, things :: IntMap [Thing]} deriving (Show, Eq)

-- The fourth floor contains nothing relevant.
-- The third floor contains a promethium generator, a promethium-compatible microchip, 
--                          a ruthenium generator, and a ruthenium-compatible microchip.
-- The second floor contains a plutonium-compatible microchip and a strontium-compatible microchip.
-- The first floor contains a thulium generator, a thulium-compatible microchip, 
--                          a plutonium generator, and a strontium generator.

main = solution1

solution1 = do
  let ss = bfs' s0

  putStrLn "solution: "
  putStrLn . unlines . map show $ ss
  putStrLn $ "solved in " ++ show (length ss - 1) ++ " steps"
  where s0 = S 1 $ IntMap.fromList [ (4,[])
                                   , (3,[G Pr, M Pr, G R, M R])
                                   , (2,[M Pl, M St])
                                   , (1,[G T, M T, G Pl, G St])
                                   ]

ex1 = search [s0]
  where s0 = S 1 $ IntMap.fromList [ (4,sort [])
                                   , (3,sort [G L])
                                   , (2,sort [G H])
                                   , (1,sort [M H, M L])
                                   ]
data Queue a = Queue [a] [a]

queueHead (Queue [] enq)      = (x, Queue deq []) where (x:deq) = reverse enq
queueHead (Queue (x:deq) enq) = (x, Queue deq enq)

queue (Queue deq enq) xs = Queue deq (xs ++ enq)

queueFromList enq = Queue enq []

bfs' :: State -> [State]
bfs' s = bfs $ queueFromList [[s]]

bfs :: Queue [State] -> [State]
bfs q | finalState (head ss) = ss
      | otherwise            = bfs . queue q' . map (:ss) $ nextStates ss
  where 
  (ss, q') = queueHead q

search :: [State] -> Maybe [State]
search ss | finalState (head ss) = Just ss
search ss = shortest . map (\n -> search (n:ss)) . nextStates $ ss

shortest :: [Maybe [State]] -> Maybe [State]
shortest = shortest' Nothing
shortest' x []     = x
shortest' x (y:ms) | x `shorter` y = shortest' x ms
                   | otherwise     = shortest' y ms
  where
  shorter Nothing  Nothing  = True
  shorter (Just _) Nothing  = True
  shorter Nothing  (Just _) = False
  shorter (Just m) (Just n) = length m < length n

finalState :: State -> Bool
finalState (S e m) = e == 4 && all (\i -> (m ! i) == []) [1..3]

nextStates :: [State] -> [State]
nextStates ss = 
  filter (\n -> safe n && not (n `elem` ss))
    $ case e of 1 -> up
                4 -> down
                _ -> up ++ down
  where
  s@(S e ts) = head ss
  cs   = combinations (ts ! e)
  up   = map (moveTo s (e+1)) cs
  down = map (moveTo s (e-1)) cs

moveTo :: State -> Int -> [Thing] -> State
moveTo (S e m) e' ts = S e' $ IntMap.insertWith merge e' ts
                            $ IntMap.adjust (sort . (\\ts)) e m
  where 
  merge xs ys = sort (xs ++ ys)

combinations :: [Thing] -> [[Thing]]
combinations ts = [[t] | t <- ts] ++ [[t1,t2] | t1 <- ts, t2 <-ts, t1 /= t2]

safe :: State -> Bool
safe = all safeFloor . things

safeFloor :: [Thing] -> Bool
safeFloor ts = case (removePairs ts) of
  []  -> True
  ts' -> case (any isG ts', any isM ts') of
           (True,True)   -> False
           (True,False)  -> True
           (False,True)  -> True
           (False,False) -> True

removePairs [] = []
removePairs (t:ts) | opposite t `elem` ts = removePairs (filter (/=(opposite t)) ts)
                   | otherwise            = t : removePairs ts

isG (G _) = True
isG _     = False

isM (M _) = True
isM _     = False

opposite (M i) = G i
opposite (G i) = M i
