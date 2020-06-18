{-# LANGUAGE OverloadedStrings #-}

module Day03 where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List.Split (splitOn)
import qualified Data.Vector as Vec

type Point = (Int, Int)
type Path = Map.Map Point Int

manhattanDistance :: Point -> Point -> Int
manhattanDistance (x, y) (x', y') = abs (x - x') + abs (y - y')

testDataStr :: String
testDataStr = "R8,U5,L5,D3\nU7,R6,D4,L4"

testDataStr2 :: String
testDataStr2 = "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"

testDataStr3 :: String
testDataStr3 = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7"

parseInput :: String -> [Path]
parseInput = map parseWire . lines

parseWire :: String -> Path
parseWire = Map.fromList . run (0, 0) Vec.empty . concatMap toDirection . splitOn ","
  where
    run _      acc []            = zip (Vec.toList acc) [1..]
    run (x, y) acc ((x', y'):ps) =
      run newP (Vec.snoc acc newP) ps
      where
        newP = (x + x', y + y')

toDirection :: String -> [Point]
toDirection input =
  case d of
    'R' -> replicate m (1, 0)
    'L' -> replicate m (-1, 0)
    'U' -> replicate m (0, 1)
    'D' -> replicate m (0, -1)
    _   -> error "Invalid direction"
  where
    d = head input
    m = read (tail input) :: Int

getIntersections :: Path -> Path -> [Point]
getIntersections a b =
  Set.toList . Set.intersection (Map.keysSet a) $ Map.keysSet b

getPaths :: String -> (Path, Path)
getPaths s =
  (head ps, head . tail $ ps)
  where
    ps = parseInput s

solve01 :: String -> Int
solve01 s =
  minimum . map (manhattanDistance (0, 0)) . getIntersections a $ b
  where
    (a, b) = getPaths s

solve02 :: String -> Int
solve02 s =
  minimum . map cost $ ints
  where
    ints = getIntersections a b
    (a, b) = getPaths s
    cost i = get i a + get i b
      where
        get k m =
          case Map.lookup k m of
            Just v -> v
            Nothing -> error "missing point"

solve :: IO ()
solve = do
  d <- readFile "data/Day03.txt"
  print (solve01 d)
  print (solve02 d)

testAll :: Bool
testAll =
  solve01 testDataStr == 6 &&
  solve01 testDataStr2 == 159 &&
  solve01 testDataStr3 == 135 &&
  solve02 testDataStr == 30 &&
  solve02 testDataStr2 == 610 &&
  solve02 testDataStr3 == 410
  
