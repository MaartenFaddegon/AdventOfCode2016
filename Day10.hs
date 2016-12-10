module Day10 where
import Data.Char
import Data.List
import Data.IntMap.Strict (IntMap,(!))
import qualified Data.IntMap.Strict as IntMap

data Value = Value Int              deriving Show
data Reg   = Bot Int | Output Int   deriving Show
data Stmt  = Move Value Reg
           | CmpMove Reg Reg Reg    deriving Show

type Env = (IntMap [Int], IntMap [Int])

parseDec :: String -> (Int, String)
parseDec (c:rest) | isSpace c = parseDec rest
                  | otherwise = parseDec' 0 (c:rest)
parseDec' i (c:rest) | isDigit c = parseDec' (digitToInt c + 10*i) rest
                     | otherwise = (i,c:rest)
parseDec' i []                   = (i,[])

parse :: String -> [Stmt]
parse = map parseStmt . lines

parseStmt :: String -> Stmt
parseStmt s | "value" `isPrefixOf` s = let
  (v,s1) = parseValue s
  (r,_) = parseReg $ expect "goes to" s1
  in Move v r

parseStmt s1 | "bot" `isPrefixOf` s1 = let
  (r1,s2)  = parseReg s1
  (r2,s3) = parseReg $ expect "gives low to" s2
  (r3,_)  = parseReg $ expect "and high to"  s3
  in CmpMove r1 r2 r3

expect :: String -> String -> String
expect s (c:rest) | isSpace c = expect s rest
                  | otherwise = expect' s (c:rest)
expect' [] s = s
expect' (c1:s1) (c2:s2) | c1 == c2 = expect' s1 s2
                        | otherwise = error $ "Unexpected \"" ++ (c2:s2) ++ "\"\n  "
                                               ++ "Expected \"" ++ (c1:s1) ++ "\""

parseReg :: String -> (Reg, String)
parseReg (c:rest) | isSpace c = parseReg rest

parseReg s | "bot" `isPrefixOf` s = let
  (i,s1) = parseDec $ dropPrefix "bot" s
  in (Bot i,s1)

parseReg s | "output" `isPrefixOf` s = let
  (i,s1) = parseDec $ dropPrefix "output" s
  in (Output i,s1)

parseReg s = error $ "Unexpected register: \"" ++ s ++ "\"\n  Expected \"bot\" or \"output\""

parseValue :: String -> (Value,String)
parseValue s = (Value v,rest)
  where (v,rest) = parseDec $ dropPrefix "value" s

dropPrefix p s = drop (length p) s

ex1 = parseStmt "value 5 goes to bot 2"
ex2 = parseStmt "bot 2 gives low to bot 1 and high to bot 0"
ex3 = do
  s <- readFile "Day10.example"
  print $ parse s

set :: Int -> Reg -> Env -> Env
set v (Bot i)    (envb,envo) = (IntMap.insertWith (++) i [v] envb, envo)
set v (Output i) (envb,envo) = (envb, IntMap.insertWith (++) i [v] envo)

get :: Reg -> Env -> [Int]
get (Bot i) (envb,_) = envb ! i

clear :: Reg -> Env -> Env
clear (Bot i) (envb,envo) = (IntMap.delete i envb, envo)

move (Value v) r1 env = set v r1 env

cmpMove r1 r2 r3 env = set v2 r3 . set v1 r2 . clear r1 $ env
  where [v1,v2] = sort (get r1 env)

eval env (Move v r)         = move v r env
eval env (CmpMove r1 r2 r3) = cmpMove r1 r2 r3 env

isMove (Move _ _) = True
isMove _          = False

solve stmts = solve' env0 otherStmts
  where 
  env0 = foldl eval (IntMap.empty,IntMap.empty) moveStmts
  (moveStmts,otherStmts) = (filter isMove stmts, filter (not . isMove) stmts)

solve' :: Env -> [Stmt] -> [(Int,Int,Int)]
solve' env stmts = case IntMap.toList (IntMap.filter hasTwo (fst env)) of
  []          -> []
  ((i,[x,y]):_) -> case filter (moveFrom i) stmts of 
    (s:_) -> (i,x,y) : solve' (eval env s) stmts

hasTwo = (==2) . length

moveFrom i (CmpMove (Bot j) _ _) = i == j
moveFrom _ _                     = False

ex4 = do
  s <- readFile "Day10.example"
  putStr. unlines . map show . solve $ parse s

solution1 = do
  s <- readFile "Day10.input"
  print . filter (\(i,x,y) -> sort [x,y] == [17,61]) . solve . parse $ s

solve2 stmts = solve2' env0 otherStmts
  where 
  env0 = foldl eval (IntMap.empty,IntMap.empty) moveStmts
  (moveStmts,otherStmts) = (filter isMove stmts, filter (not . isMove) stmts)

solve2' :: Env -> [Stmt] -> [Env]
solve2' env stmts = case IntMap.toList (IntMap.filter hasTwo (fst env)) of
  []          -> [env]
  ((i,[x,y]):_) -> case filter (moveFrom i) stmts of 
    (s:_) -> env : solve2' (eval env s) stmts

solution2 = do
  s <- readFile "Day10.input"
  print . mulOut . last . solve2 . parse $ s

  where mulOut (_,envo) = let [x] = envo ! 0
                              [y] = envo ! 1
                              [z] = envo ! 2
                          in x*y*z
