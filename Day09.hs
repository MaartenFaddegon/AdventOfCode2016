import Data.Char
import Test.QuickCheck

data Marker = M Int Int deriving (Show, Eq)

ex1 = parse "ADVENT"            == "ADVENT"
ex2 = parse "A(1x5)BC"          == "ABBBBBC"
ex3 = parse "A(2x2)BCD(2x2)EFG" == "ABCBCDEFEFG"
ex4 = parse "X(8x2)(3x3)ABCY"   == "X(3x3)ABC(3x3)ABCY"

solution1 = do
  s <- readFile "Day9.input"
  print . length . parse $ s

parse ('(':rest)           = let (s1,s2) = uncurry applyMarker $ parseMarker ('(':rest)
                             in s1 ++ parse s2
parse (c:rest) | isSpace c = parse rest
parse (c:rest)             = c : parse rest
parse []                   = []

parseMarker :: String -> (Marker, String)
parseMarker ('(':rest) = parseMarker' (parseDec 0 rest )

parseMarker' :: (Int, String) -> (Marker,String)
parseMarker' (i,'x':rest) = (M i j, rest')
  where
  (j,')':rest') = parseDec 0 rest
parseMarker' (_,s) = error $ "parseMarker' cannot be applied to " ++ s

parseDec :: Int -> String -> (Int, String)
parseDec i (c:rest) | isDigit c = parseDec (digitToInt c + 10*i) rest
                    | otherwise = (i,c:rest)
parseDec i []                   = (i,[])

applyMarker :: Marker -> String -> (String, String)
applyMarker (M i j) s = (s1,s2)
  where
  s1 = concat . take j . repeat . take i $ s
  s2 = drop i s


prop_parseDec :: NonNegative Int -> String -> Property
prop_parseDec (NonNegative i) s = p ==> parseDec 0 (show i ++ s) == (i,s)
  where p = case s of []  -> True
                      c:_ -> not (isDigit c)

prop_parseMarker :: NonNegative Int -> NonNegative Int -> String -> Bool
prop_parseMarker (NonNegative i) (NonNegative j) s = 
  parseMarker (m++s) == (M i j, s)
  where
  m = "(" ++ show i ++ "x" ++ show j ++ ")"
