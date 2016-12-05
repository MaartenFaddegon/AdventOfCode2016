import Data.Hash.MD5
import Data.Char
import Data.List

hash :: String -> Int -> String
hash s i = md5s (Str $ s ++ show i)

search s i = case take 6 (hash s i) of
  ['0','0','0','0','0',x] -> x : search s (i+1)
  _                       ->     search s (i+1)

ex1 = take 4 $ search exInput 0

solution1 = take 8 $ search input 0

search2 :: String -> Int -> [Int] -> [(Int, Char)]
search2 s i ps = case hash s i of
  '0':'0':'0':'0':'0':x:y:_ -> case midx x of
                                 Just i  -> if i `elem` ps then srch ps else (i, y) : srch (i:ps)
                                 Nothing -> srch ps
  _                         -> srch ps
  where
  srch = search2 s (i+1)

midx c | c >= '0' && c < '8' = Just $ ord c - ord '0'
       | otherwise           = Nothing

ex2 = search2 exInput 0 []

solution2 = do
  let xs = take 8 $ search2 input 0 []
  print xs
  putStrLn $ "code: " ++ show (map snd . sortOn fst $ xs)

exInput = "abc"

input = "ojvtpuvg"

main = solution2
