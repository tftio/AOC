module Day22 where
depth  = 5913
target = Point 8 701
mouth  = Point 0 0

data Point = Point Int Int deriving (Show, Eq, Ord)
data Region = Rocky | Narrow | Wet deriving (Show, Eq, Ord)

geologicIndex :: Point -> Int
geologicIndex p | p == target || p == mouth = 0
geologicIndex (Point x 0) = x * 16807
geologicIndex (Point 0 y) = y * 48271
geologicIndex (Point x y) = geologicIndex (Point (x - 1) y) * geologicIndex (Point x (y - 1))

erosionLevel :: Point -> Int -> Int
erosionLevel p d = geologicIndex p + d

regionType :: Point -> Int -> Region
regionType p d =
  case erosionLevel p d of
    i | i `mod` 3 == 0 -> Rocky
    i | i `mod` 3 == 1 -> Wet
    i | i `mod` 3 == 2 -> Narrow

charToRegion :: Char -> Region
charToRegion c = case c of
  '=' -> Wet
  '|' -> Narrow
  '.' -> Rocky
  _   -> error ("Cannot derive region from '" ++ show c ++ "'")

regionToChar :: Region -> Char
regionToChar Wet = '='
regionToChar Narrow = '|'
regionToChar Rocky = '.'

