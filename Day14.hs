import Data.Hash.MD5
import Data.List
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

data HashCache = H (Int -> String) (IntMap (Maybe Char, Maybe Char))

ex0 = threeOfTheSame (hash "abc" 0x18)
ex1 = snd $ getHash    (initCache (hash "abc")) 0x18
ex2 = snd $ getHash    (initCache (hash "abc")) 0x39
ex3 = snd $ getHash    (initCache (hash "abc")) 0x816
ex3' = snd $ search5   (initCache (hash "abc")) 0x39 1 'e'
ex4 = head $ searchKey (initCache (hash "abc")) 0
ex5 = (searchKey (initCache $ hash "abc") 0) !! 63

solution1 = (searchKey (initCache $ hash "ahsbgdzn") 0) !! 63

ex6 = (stretchedHash "abc" 0)

solution2 = (searchKey (initCache $ stretchedHash "ahsbgdzn") 0) !! 63

main = print solution2


searchKey :: HashCache -> Int -> [Int]
searchKey m i = case x of 
    Just c  -> searchKey' m'' i c
    Nothing -> searchKey  m'' (i+1)
  where 
  (m',(x,_)) = getHash m i
  m'' = deleteFromCache i m'

searchKey' :: HashCache -> Int -> Char -> [Int]
searchKey' m i c = case search5 m i 1 c of
  (m',True)  -> i : searchKey m' (i+1)
  (m',False) ->     searchKey m' (i+1)

search5 m i j c | j > 1000  = (m, False)
                | otherwise = if x == Just c then (m',True) else search5 m' i (j+1) c
  where (m',(_,x)) = getHash m (i+j)

deleteFromCache :: Int -> HashCache -> HashCache
deleteFromCache i (H s m) = H s (IntMap.delete i m)

getHash :: HashCache -> Int -> (HashCache, (Maybe Char, Maybe Char))
getHash (H s m) i = case IntMap.lookup i m of
  Just x  -> (H s m, x)
  Nothing -> (H s (IntMap.insert i y m), y)
  where
  h = s i
  y = (threeOfTheSame h, fiveOfTheSame h)

initCache s = H s IntMap.empty

stretchedHash :: String -> Int -> String
stretchedHash s i  = stretchedHash' 2016 (hash s i)
stretchedHash' 0 s = s
stretchedHash' n s = stretchedHash' (n-1) $ md5s (Str s)

hash :: String -> Int -> String
hash s i = md5s (Str $ s ++ show i)

threeOfTheSame (a:b:c:rest) | a == b && b == c = Just a
                            | otherwise        = threeOfTheSame (b:c:rest)
threeOfTheSame _ = Nothing

fiveOfTheSame (a:b:c:d:e:rest) | a == b && b == c && c == d && d == e = Just a
                               | otherwise = fiveOfTheSame (b:c:d:e:rest)
fiveOfTheSame _ = Nothing
