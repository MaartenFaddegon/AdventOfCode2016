import Data.Word
import Data.Bits
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Set (Set)
import qualified Data.Set as Set
import Queue

favourite = 1352

main = print solution2

solution2 = bfs seen0 open0 $ queueFromList [(1,1,0)]
  where 
  seen0 = Set.singleton . index $ (1,1,0)
  open0 = Set.empty

bfs :: Set Int -> Set Int -> Queue (Int,Int,Int) -> Int
bfs seen open q | queueEmpty q    = Set.size open
                | isWall (x,y)    = bfs seen open q'
                | otherwise       = bfs seen' open' (queue q' xys)
  where 
  ((x,y,steps), q') = queueHead q
  xys = prune seen . filterPos . combinations $ (x,y,steps)
  seen' = foldr (Set.insert . index) seen xys
  open' = Set.insert (index (x,y,steps)) open 

prune seen = filter (\c -> (index c) `Set.notMember` seen)

filterPos = filter (\(x,y,s) -> x >= 0 && y >= 0 && s <= 50)

combinations (x,y,s) = [(x+1,y,s'), (x,y+1,s'), 
                       (x-1,y,s'), (x,y-1,s')]
                       where s' = s+1

index :: (Int, Int, Int) -> Int
index (x,y,_) = (shiftL x 32) .|. y

isWall :: (Int,Int) -> Bool
isWall (x',y') = c .&. 1 == 1
  where
  x = fromIntegral x'
  y = fromIntegral y'
  c = bitcount $ x*x + 3*x + 2*x*y + y + y*y + favourite

bitcount :: Word64 -> Word64
bitcount v = c3
  where
  c1 = ((v .&. 0xfff) * 0x1001001001001 .&. 0x84210842108421) `mod` 0x1f
  c2 = c1 + (((shiftR (v .&. 0xfff000) 12) * 0x1001001001001 .&. 0x84210842108421) `mod` 0x1f)
  c3 = c2 + (((shiftR v 24) * 0x1001001001001 .&. 0x84210842108421) `mod` 0x1f)
