import Data.IntMap.Strict (IntMap,(!))
import qualified Data.IntMap.Strict as IntMap
import Data.List
import Data.Char
import Queue

data MazeLocation = W     -- wall                   '#'
                  | C     -- corridor               '.'
                  | I Int -- interesting location   0,1,2...

data Maze = M (IntMap MazeLocation) (IntMap Int)

main = do
  input <- readFile "Day24.map"
  let m = parseMaze input
      w = 11; h = 5
      t = maxInterest m
  putStrLn (showMaze m w h)

  -- compute distance from any location of interest to any other
  let ds = concat . map (distances m [0..t]) $ [0..t]
      dm = IntMap.fromList ds 
  mapM_ (\(x,d) -> let (i,j) = coord x in 
                   putStrLn $ "distance from " ++ show i ++ " to " 
                              ++ show j ++ ": " ++ show d) ds

  -- compute cost for the different routes
  let cs = reverse . sortOn snd . map (\route -> let route' = 0:route ++ [0] in (route', cost dm route')) 
           $ permutations [1..t]
  mapM_ (\(r,c) -> putStrLn $ "it takes " ++ show c ++ " steps for the route " ++ show r) cs

cost dm [_]      = 0
cost dm (i:j:is) = (dm ! (idx i j)) + cost dm (j:is)

maxInterest (M _ im) = maximum . map fst . IntMap.toList $ im

distances (M lm im) is i = [(idx i j, distance fm im j) | j <- js]
  where fm = flow (im ! i) lm
        js = filter (/=i) is

distance fm im i = fm ! (im ! i)

flow :: Int -> IntMap MazeLocation -> IntMap Int
flow i = flow' (queueFromList [i]) (IntMap.fromList [(i, 0)])

flow' :: Queue Int -> IntMap Int -> IntMap MazeLocation -> IntMap Int
flow' q fm m | queueEmpty q = fm
             | otherwise    = flow' (queue q' cs) fm' m
  where
  fm'        = foldr (uncurry IntMap.insert) fm (zip cs (repeat $ (fm ! i) + 1))
  (i,q')     = queueHead q
  (x,y)      = coord i
  cs         = filter (\j -> IntMap.notMember j fm && corridor (IntMap.lookup j m)) 
                      [idx x' y' | (x',y') <- [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]]

corridor Nothing  = False
corridor (Just W) = False
corridor _        = True

instance Show MazeLocation where
  show W     = " #"
  show C     = " ."
  show (I i) = " " ++ show i

showFlow fm w h =
  unlines $ [showLine y | y <- [0..h-1]]
  where
  showLine y = concat [ show4 $ IntMap.lookup (idx x y) fm | x <- [0..w-1]]

show4 Nothing  = "   #"
show4 (Just i) = (case length s of 1 -> "   "; 2 -> "  "; 3 -> " "; _ -> "") ++ s where s = show i

showMaze (M lm im) w h =
  unlines $ [showLine y | y <- [0..h-1]]
  ++ ("\ninteresting locations: " : [ show l ++ " at " ++ show (coord i) | (l,i) <- IntMap.toList im])
  where
  showLine y = concat [ show $ lm ! (idx x y) | x <- [0..w-1]]

maxWidth = 200

idx x y | x < maxWidth = x+y*maxWidth

coord i = (i `mod` maxWidth, i `div` maxWidth)

parseMaze :: String -> Maze
parseMaze = foldl parseLine maze0 . zip [0..] . lines
  where
  maze0 = M IntMap.empty IntMap.empty

parseLine :: Maze -> (Int, String) -> Maze
parseLine m (y,s) = foldl (parseLoc y) m . zip [0..] $ s

parseLoc :: Int -> Maze -> (Int, Char) -> Maze
parseLoc y (M lm im) (x,c) = M lm' im'
  where
  lm' = IntMap.insert (idx x y) l lm
  im' = case l of (I i) -> IntMap.insert i (idx x y) im
                  _     -> im
  l = case c of '#' -> W
                '.' -> C
                _   -> I (digitToInt c)
