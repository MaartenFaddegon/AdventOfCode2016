import Prelude hiding (negate)

main = print solution2


ex1 = dragonTail1 (5,"11111")
ex2 = checksum (dragonTail 20 "10000")

solution1 = checksum (dragonTail 272       "11100010111110100")
solution2 = checksum (dragonTail 35651584 "11100010111110100")


checksum :: String -> String
checksum x = let c0 = checksum' x in loop (length x) x
  where 
  loop :: Int -> String -> String
  loop len c | len `mod` 2 == 1 = c
             | otherwise        = loop (len `div` 2) (checksum' c)


checksum' (a:b:rest) = (if a == b then '1' else '0') : checksum' rest
checksum' [] = []

dragonTail len input = take len . snd $ until ((>=len) . fst) dragonTail1 (length input, input)

dragonTail1 :: (Int, String) -> (Int, String)
dragonTail1 (len,a) = (2*len+1, a ++ ('0' : b))
  where b = negate . reverse $ a


negate = map negate'
negate' '0' = '1'
negate' '1' = '0'
