data Rotation = R | L deriving (Show, Eq)

data Instruction = I Rotation Int deriving (Show, Eq)

data Direction = N | E | S | W deriving (Show, Eq)

type Position = (Int, Int)

data State = State{direction :: Direction, position :: Position} deriving (Show, Eq)

instructions = [I R 3, I L 2, I L 2, I R 4, I L 1, I R 2, I R 3, I R 4, I L 2, I R 4, I L 2, I L 5, I L 1, I R 5, I R 2, I R 2, I L 1, I R 4, I R 1, I L 5, I L 3, I R 4, I R 3, I R 1, I L 1, I L 5, I L 4, I L 2, I R 5, I L 3, I L 4, I R 3, I R 1, I L 3, I R 1, I L 3, I R 3, I L 4, I R 2, I R 5, I L 190, I R 2, I L 3, I R 47, I R 4, I L 3, I R 78, I L 1, I R 3, I R 190, I R 4, I L 3, I R 4, I R 2, I R 5, I R 3, I R 4, I R 3, I L 1, I L 4, I R 3, I L 4, I R 1, I L 4, I L 5, I R 3, I L 3, I L 4, I R 1, I R 2, I L 4, I L 3, I R 3, I R 3, I L 2, I L 5, I R 1, I L 4, I L 1, I R 5, I L 5, I R 1, I R 5, I L 4, I R 2, I L 2, I R 1, I L 5, I L 4, I R 4, I R 4, I R 3, I R 2, I R 3, I L 1, I R 4, I R 5, I L 2, I L 5, I L 4, I L 1, I R 4, I L 4, I R 4, I L 4, I R 1, I R 5, I L 1, I R 1, I L 5, I R 5, I R 1, I R 1, I L 3, I L 1, I R 4, I L 1, I L 4, I L 4, I L 3, I R 1, I R 4, I R 1, I R 1, I R 2, I L 5, I L 2, I R 4, I L 1, I R 3, I L 5, I L 2, I R 5, I L 4, I R 5, I L 5, I R 3, I R 4, I L 3, I L 3, I L 2, I R 2, I L 5, I L 5, I R 3, I R 4, I R 3, I R 4, I R 3, I R 1]

rotate :: Instruction -> State -> State
rotate (I R _) (State N p) = State E p
rotate (I R _) (State E p) = State S p
rotate (I R _) (State S p) = State W p
rotate (I R _) (State W p) = State N p
rotate (I L _) (State N p) = State W p
rotate (I L _) (State E p) = State N p
rotate (I L _) (State S p) = State E p
rotate (I L _) (State W p) = State S p

move :: Instruction -> State -> State
move (I _ d) (State N (x,y)) = State N (x,y+d)
move (I _ d) (State E (x,y)) = State E (x+d,y)
move (I _ d) (State S (x,y)) = State S (x,y-d)
move (I _ d) (State W (x,y)) = State W (x-d,y)

ap :: Instruction -> State -> State
ap i = (move i) . (rotate i)

eval :: [Instruction] -> State
eval = foldl (flip ap) s_0

s_0 = State N (0,0)

s_z :: State
s_z = eval instructions

distance :: State -> State -> Int
distance State{position=(a,b)} State{position=(c,d)} = abs(a-c) + abs(b-d)

ex1  = eval [I R 2, I L 3]
ex1' = distance s_0 ex1 == 5
ex2  = eval [I R 2, I R 2, I R 2]
ex2' = distance s_0 ex2 == 2
ex3  = eval [I R 5, I L 5, I R 5, I R 3]
ex3' = distance s_0 ex3 == 12

solution1 = distance s_0 s_z

-----

move' :: Instruction -> State -> [State]
move' (I _ d) (State N (x,y)) = [State N (x,y+d') | d' <- [1..d]]
move' (I _ d) (State E (x,y)) = [State E (x+d',y) | d' <- [1..d]]
move' (I _ d) (State S (x,y)) = [State S (x,y-d') | d' <- [1..d]]
move' (I _ d) (State W (x,y)) = [State W (x-d',y) | d' <- [1..d]]

ap' :: Instruction -> [State] -> [State]
ap' i = (move' i) . (rotate i) . last

eval' :: [Instruction] -> [State]
eval' = concat . scanl (flip ap') [s_0]

twice []                   = Nothing
twice (x:xs) | x `elem` xs = Just x
             | otherwise   = twice xs


distance' :: Position -> Position -> Int
distance' (a,b) (c,d) = abs(a-c) + abs(b-d)

ex4 = map position $ eval' [I R 8, I R 4, I R 4, I R 8]
ex4' = twice ex4

solution2 = do 
  p <- twice . map position . eval' $ instructions
  return $ distance' (0,0) p

main = do
  putStr "Solution 1: "
  print solution1
  putStr "Solution 2: "
  print solution2
