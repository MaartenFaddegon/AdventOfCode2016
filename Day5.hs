import Data.Hash.MD5

hash :: String -> Int -> String
hash s i = md5s (Str $ s ++ show i)

search s i = case take 6 (hash s i) of
  ['0','0','0','0','0',x] -> x : search s (i+1)
  _                       ->     search s (i+1)

ex1 = take 4 $ search exInput 0

solution1 = take 8 $ search input 0

exInput = "abc"

input = "ojvtpuvg"
