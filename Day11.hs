import Data.IntMap.Strict (IntMap,(!))
import qualified Data.IntMap.Strict as IntMap
import Data.List
import Debug.Trace
import Data.Bits
import Data.Int

data Isotope = T | Pl | St | Pr | R                          deriving (Show,Eq,Ord)
data Thing   = P | M Isotope | G Isotope                     deriving (Show,Eq,Ord)
data State   = S {elevator :: Int, things :: IntMap [Thing], moves :: Int} deriving Eq

encode :: Thing -> Int64 -- takes 10 bits
encode (M T)  = 1
encode (M Pl) = 2
encode (M St) = 4
encode (M Pr) = 8
encode (M R)  = 16
encode (G T)  = 32
encode (G Pl) = 64
encode (G St) = 128
encode (G Pr) = 256
encode (G R)  = 512

encodeFloor :: [Thing] -> Int64
encodeFloor = foldl (\i t -> i .|. (encode t)) 0

fingerprint :: State -> Int64
fingerprint (S e ts _) = foldl (.|.) (fromIntegral e) floors
  where
  floors = [shiftL (encodeFloor (ts ! i)) (10*i+4) | i <- [1..4]]

instance Show State where
  show (S e ts mvs) = "elevator at " ++ show e ++ "\n"
                     ++ unlines (map show $ reverse $ IntMap.toList ts)
                     ++ "\n" ++ show mvs ++ "moves\n"

-- The fourth floor contains nothing relevant.
-- The third floor contains a promethium generator, a promethium-compatible microchip, 
--                          a ruthenium generator, and a ruthenium-compatible microchip.
-- The second floor contains a plutonium-compatible microchip and a strontium-compatible microchip.
-- The first floor contains a thulium generator, a thulium-compatible microchip, 
--                          a plutonium generator, and a strontium generator.

main = print solution1

solution1 = bfs' s0

s0 = S 1 ( IntMap.fromList [ (4,[])
                           , (3,[G Pr, M Pr, G R, M R])
                           , (2,[M Pl, M St])
                           , (1,[G T, M T, G Pl, G St])
                           ]) 0
data Queue a = Queue [a] [a]

exs0 = S 1 ( IntMap.fromList [ (4,[])
                             , (3,[])
                             , (2,[G T, M Pl])
                             , (1,[M T])
                             ]) 0

exs1 = S 1 ( IntMap.fromList [ (4,[])
                             , (3,[G Pl])
                             , (2,[M Pl, G T])
                             , (1,[M T])
                             ]) 1

queueHead (Queue [] [])       = error "empty queue!"
queueHead (Queue [] enq)      = (x, Queue deq []) where (x:deq) = reverse enq
queueHead (Queue (x:deq) enq) = (x, Queue deq enq)

queue (Queue deq enq) xs = Queue deq (xs ++ enq)

queueFromList enq = Queue enq []

bfs' :: State -> Int
bfs' s = bfs [fingerprint s] $ queueFromList [s]

bfs :: [Int64] -> Queue State -> Int
bfs seen q | finalState s = moves s
           | otherwise    = bfs seen' q''
  where 
  (s_observed, q') = queueHead q
  s = trace ("{{{\n" ++ show s_observed ++ "}}}\n") s_observed
  q'' = queue q' . map fst . prune seen' . addFingerprint . nextStates $ s
  seen' = (fingerprint s):seen

addFingerprint = map (\s -> (s,fingerprint s))

prune seen = filter (\(_,p) -> not $ p `elem` seen)
-- prune seen = filter (\s1 -> not $ s1 `hasEquiv` seen)

finalState :: State -> Bool
finalState (S e m _) = e == 4 && all (\i -> (m ! i) == []) [1..3]

nextStates :: State -> [State]
nextStates (s@(S e ts mvs)) = filter (\n -> safe n)
    $ case e of 1 -> up
                4 -> down
                _ -> up ++ down
  where
  up   = map (moveTo s (e+1)) $ upCombinations   (ts ! e)
  down = if all (\e' -> (ts ! e') == []) [1..e-1]
           then [] -- don't bring things down if all empty below
           else map (moveTo s (e-1)) $ downCombinations (ts ! e)

hasEquiv :: State -> [State] -> Bool
hasEquiv s1 = any (\s2 -> s1' `equiv` s2)
  where s1' = simplifySt s1

equiv (S e ts _) (S e' ts' _) = e == e && ts == ts'

simplifySt (S e ts mvs) = S e (IntMap.map simplify ts) mvs

moveTo :: State -> Int -> [Thing] -> State
moveTo (S e m mvs) e' ts = S e' ( IntMap.insertWith (++) e' ts
                                $ IntMap.adjust (\\ts) e m) (mvs+1)

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
