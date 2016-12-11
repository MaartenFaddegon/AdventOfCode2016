import Data.IntMap.Strict (IntMap,(!))
import qualified Data.IntMap.Strict as IntMap
import Data.List
import Data.Int

data Isotope = T | Pl | St | Pr | R                          deriving (Show,Eq,Ord)
data Thing   = P | M Isotope | G Isotope                     deriving (Show,Eq,Ord)
data State   = S {elevator :: Int, things :: IntMap [Thing]} deriving Eq

instance Show State where
  show (S e ts) = "elevator at " ++ show e ++ "\n"
                   ++ unlines (map show $ reverse $ IntMap.toList ts)

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

s0 = S 1 $ IntMap.fromList [ (4,[])
                           , (3,[G Pr, M Pr, G R, M R])
                           , (2,[M Pl, M St])
                           , (1,[G T, M T, G Pl, G St])
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
      | otherwise            = bfs . queue q' . map (:ss) . prune $ nextStates ss
  where 
  (ss, q') = queueHead q

prune []     = []
prune (s:ss) | s `hasEquiv` ss = prune ss
             | otherwise       = s : prune ss

finalState :: State -> Bool
finalState (S e m) = e == 4 && all (\i -> (m ! i) == []) [1..3]

nextStates :: [State] -> [State]
nextStates ss = 
  filter (\n -> safe n && not (n `hasEquiv` ss))
    $ case e of 1 -> up
                4 -> down
                _ -> up ++ down
  where
  s@(S e ts) = head ss
  up   = map (moveTo s (e+1)) $ upCombinations   (ts ! e)
  down = if all (\e' -> (ts ! e') == []) [1..e-1]
           then [] -- don't bring things down if all empty below
           else map (moveTo s (e-1)) $ downCombinations (ts ! e)

hasEquiv s1 ss = any (\s2 -> s1' == s2) $ map simplifySt ss
  where s1' = simplifySt s1

simplifySt (S e ts) = S e (IntMap.map simplify ts)

moveTo :: State -> Int -> [Thing] -> State
moveTo (S e m) e' ts = S e' $ IntMap.insertWith (++) e' ts
                            $ IntMap.adjust (\\ts) e m

combinations :: [Thing] -> [[Thing]]
combinations ts = [[t1,t2] | t1 <- ts, t2 <-ts, t1 /= t2] ++ [[t] | t <- ts]

upCombinations :: [Thing] -> [[Thing]]
upCombinations ts = case [[t1,t2] | t1 <- ts, t2 <-ts, t1 /= t2] of
 []       ->  [[t] | t <- ts]
 bringTwo -> bringTwo

downCombinations :: [Thing] -> [[Thing]]
downCombinations ts = case [[t] | t <- ts]  of
 []       -> [[t1,t2] | t1 <- ts, t2 <-ts, t1 /= t2]
 bringOne -> bringOne

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

simplify :: [Thing] -> [Thing]
simplify = sort . simplify'
simplify' [] = []
simplify' (t:ts) | opposite t `elem` ts = P : (simplify' (filter (/=(opposite t)) ts))
                 | otherwise            = t : simplify' ts

isG (G _) = True
isG _     = False

isM (M _) = True
isM _     = False

opposite (M i) = G i
opposite (G i) = M i
opposite x     = error $ "Opposite of " ++ show x