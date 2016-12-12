import Data.Int
import Data.Bits
import Data.Vector(Vector,(//),(!))
import qualified Data.Vector as Vector
import Data.Set (Set)
import qualified Data.Set as Set
import Queue
import Debug.Trace

type State = Vector Int64
type Fingerprint = [Int64]

isos = [1,2,4,8,16]

isoBits = length isos

lvtrIdx = 8
mvsIdx  = 9

showStates = unlines . map showState

showState :: State -> String
showState s = (concat . reverse 
               $ [show i ++ ":" ++ (e i) ++ showFloor (s ! (2*i)) (s ! (2*i+1)) | i <- [0..3]]
              ) ++ "after " ++ show mvs ++ " moves\n"
  where 
  e j = if (fromIntegral j) == (s ! lvtrIdx) then " E " else "   "
  mvs = s ! mvsIdx

showFloor :: Int64 -> Int64 -> String
showFloor ms gs = (concat $ map (\m -> "M " ++ show m ++ " ") (toList ms))
                  ++ (concat $ map (\g -> "G " ++ show g ++ " ") (toList gs)) ++ "\n"

-- The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
-- The second floor contains a hydrogen generator.
-- The third floor contains a lithium generator.
-- The fourth floor contains nothing relevant.
--
-- hydrogen = 1
-- lithium  = 2

--                     microchips generators elevator moves
exs0 :: State          ---------- ---------- -------- -----
exs0 = Vector.fromList [ 1 .|. 2   , 0
                       , 0         , 1
                       , 0         , 2
                       , 0         , 0       , 0      , 0 ]

-- The first floor contains a thulium generator, a thulium-compatible microchip, a plutonium generator, and a strontium generator.
-- The second floor contains a plutonium-compatible microchip and a strontium-compatible microchip.
-- The third floor contains a promethium generator, a promethium-compatible microchip, a ruthenium generator, and a ruthenium-compatible microchip.
-- The fourth floor contains nothing relevant.
--
th = 1
pl = 2
st = 4
pr = 8
ru = 16

--                        microchips      generators 
input1 :: State           ----------  -----------------
input1 = Vector.fromList [ th        , th .|. pl .|. st
                         , pl .|. st , 0
                         , pr .|. ru , pr .|. ru
                         , 0         , 0
--                       elevator moves
                         -------- -----
                         , 0      , 0 ]


main = print (bfs' input1)

ex1 = putStr . showStates . map snd $ ex1'
ex1' = nextStates seen0 exs0
seen0 = Set.singleton . fingerprint $ exs0
(fphex1,hex1) = head ex1'

ex2 = putStr . showStates . map snd $ ex2'
ex2' = nextStates seen1 hex1
seen1 = Set.insert fphex1 seen0

ex3 = bfs' exs0

bfs' :: State -> Int64
bfs' s = bfs seen0 $ queueFromList [s]
  where 
  seen0 = Set.singleton . fingerprint $ s

bfs :: Set Fingerprint -> Queue State -> Int64
bfs seen q | finalState s = s ! mvsIdx
           | otherwise    = bfs seen' (queue q' ns)
  where 
  (s, q') = queueHead q
  (fps,ns) = unzip $ nextStates seen s
  seen' = foldr Set.insert seen fps

finalState :: State -> Bool
finalState s = all (\i -> (s ! (2*i) == 0) && (s ! (2*i+1) == 0)) [0..2]

nextStates :: Set [Int64] -> State -> [([Int64],State)]
nextStates seen = prune seen . tag . filter safe . nextStates'
  where 
  tag = map (\s -> (fingerprint s, s))

nextStates' :: State -> [State]
nextStates' s = [moveTo s e' x | e' <- e's, x <- combinations ms_e gs_e]
  where
  e   = s ! lvtrIdx
  e's = case e of
         0 -> [1]
         3 -> [2]
         _ -> [e+1, e-1]
  mvs = s ! mvsIdx
  ms_e = toList (s ! (fromIntegral $ 2*e))
  gs_e = toList (s ! (fromIntegral $ 2*e+1))

prune :: Set Fingerprint -> [(Fingerprint,State)] -> [(Fingerprint,State)]
prune seen = filter (\(fp,_) -> fp `Set.notMember` seen)

safe :: State -> Bool
safe s = all (\i -> safeFloor (s!(2*i)) (s!(2*i+1))) [0..3]
safeFloor _  0  = True
safeFloor ms gs = (ms .&. gs) `xor` ms == 0

combinations  ms gs = (combinations2 ms gs) ++ (combinations1 ms gs)
combinations1 ms gs =  [(m,0)  | m <- ms]                -- one of each
                    ++ [(0,g)  | g <- gs]
combinations2 ms gs =  [(m,g)  | m <- ms, g <- gs]       -- one of each
                    ++ [(0,g)  | g <- combinations2' gs] -- two of either
                    ++ [(m,0)  | m <- combinations2' ms]

combinations2' []     = []
combinations2' (t1:ts) = [t1 .|. t2 | t2 <- ts] ++ combinations2' ts
  
toList :: Int64 -> [Int64]
toList x = [y | i <- isos, let y = x .&. i, y /= 0]

moveTo :: State -> Int64 -> (Int64,Int64) -> State
moveTo s j' (ms,gs) = s // [ (2*i,  ms_i `xor` ms)     -- remove from floor i
                           , (2*i+1,gs_i `xor` gs)
                           , (2*j,  ms_j `xor` ms)     -- add to floor j
                           , (2*j+1,gs_j `xor` gs)
                           , (lvtrIdx, fromIntegral j) -- elevator at floor j
                           , (mvsIdx, mvs+1)           -- one more move
                           ]
  where i = fromIntegral (s ! lvtrIdx)
        j = fromIntegral j'
        ms_i = s ! (2*i)
        gs_i = s ! (2*i+1)
        ms_j = s ! (2*j)
        gs_j = s ! (2*j+1)
        mvs  = s ! mvsIdx

fingerprint :: State -> Fingerprint
fingerprint s = (s ! lvtrIdx) : [fingerprintFloor (s ! (2*i)) (s ! (2*i+1)) | i <- [0..3]]
 
fingerprintFloor :: Int64 -> Int64 -> Int64
fingerprintFloor ms gs = 
  (shiftL (isoCount ps) (2*isoBits)) .|. (shiftL (ms `xor` ps) isoBits) .|. (gs `xor` ps)
  where
  ps = ms .&. gs

isoCount xs = foldl (\i b -> if xs .&. b == 0 then i else i+1) 0 isos
