module Day23 where
import Data.List (sortBy, minimumBy)
import Data.List.Split
import Data.Set hiding (map, split, filter)
import Data.SBV hiding (split)

data Nanobot = Nanobot Int Int Int Int deriving (Show, Eq, Ord)

testStr = "pos=<0,0,0>, r=4\npos=<1,0,0>, r=1\npos=<4,0,0>, r=3\npos=<0,2,0>, r=1\npos=<0,5,0>, r=3\npos=<0,0,3>, r=1\npos=<1,1,1>, r=1\npos=<1,1,2>, r=1\npos=<1,3,1>, r=1"

testStr2 = "pos=<10,12,12>, r=2\npos=<12,14,12>, r=2\npos=<16,12,12>, r=4\npos=<14,14,14>, r=6\npos=<50,50,50>, r=200\npos=<10,10,10>, r=5"

distance :: Nanobot -> Nanobot -> Int
distance (Nanobot x y z _) (Nanobot x' y' z' _) = manhattanDistance (x, y, z) (x', y', z')

manhattanDistance :: (Int, Int, Int) -> (Int, Int, Int) -> Int
manhattanDistance (x, y, z) (x', y', z') =
  abs (x - x') + abs (y - y') + abs (z - z')

botDistanceFromPoint :: Nanobot -> (Int, Int, Int) -> Int
botDistanceFromPoint (Nanobot x y z _) = manhattanDistance (x, y, z)

findLargest :: [Nanobot] -> Nanobot
findLargest = minimumBy (\(Nanobot _ _ _ r) (Nanobot _ _ _ r') -> compare r' r)

parseLine :: String -> Nanobot
parseLine str =
  Nanobot x y z r
    where
      (x:y:z:r:_) = map (\s -> read s :: Int) (splitter str)
      splitter = split (condense . dropDelims . dropInnerBlanks . dropInitBlank . dropFinalBlank $ oneOf "abcdefghijklmnopqrstuvwxyz=<>,:[] ")

isCloseEnough :: Nanobot -> Nanobot -> Bool
isCloseEnough a@(Nanobot _ _ _ r) b = distance a b <= r

part01 s =
  length (filter (isCloseEnough l) bots)
  where
    bots = map parseLine (lines s)
    l    = findLargest bots

maxPoint :: [Nanobot] -> (Int, Int, Int)
maxPoint bots =
  (maximum xs, maximum ys, maximum zs)
  where
    xs = map (\(Nanobot x _ _ _) -> x) bots
    ys = map (\(Nanobot _ y _ _) -> y) bots
    zs = map (\(Nanobot _ _ z _) -> z) bots    

-- findCoordinate :: [Nanobot] -> (Int, Int, Int)
findCoordinate bots =
  [ (x,y,x) | x <- [0..maxX], y <- [0..maxY], z <- [0..maxZ]]
  where
    (maxX, maxY, maxZ) = maxPoint bots
