-- |
module Day10 where
import           Data.List.Split
import qualified Data.Set        as Set

type Point = (Int, Int)
type Velocity = (Int, Int)
type Pt = (Point, Velocity)

data TimeV = Forward | Backwards deriving (Show)

lineToPt :: [Char] -> Pt
lineToPt l = case xs of (x:y:dx:dy:[]) -> ((x, y), (dx, dy))
                        _              -> error "invalid line"
  where
    xs = map (\x -> read x :: Int) (split (dropInitBlank . dropFinalBlank . condense . dropDelims $ oneOf "abcdefghijklmnopqrstuvwxyz=<>, ") l)

pointOfPt :: Pt -> Point
pointOfPt = fst

velocityOfPt :: Pt -> Velocity
velocityOfPt = snd

updatePoint :: TimeV -> Pt -> Pt
updatePoint v ((x, y), (dx, dy)) = ((x + dx', y + dy'), (dx, dy))
  where
    direction = case v of Forward   -> 1
                          Backwards -> (-1)
    dx' = dx * direction
    dy' = dy * direction

incrementPt :: Pt -> Pt
incrementPt = updatePoint Forward

decrementPt :: Pt -> Pt
decrementPt = updatePoint Backwards

boxDimensionFromPts :: [Pt] -> (Int, Int)
boxDimensionFromPts pts = (minX + maxX, minY + maxY)
  where
    points = map pointOfPt pts
    xs     = map fst points
    ys     = map snd points
    minX   = (abs . minimum) xs
    maxX   = (abs . maximum) xs
    minY   = (abs . minimum) ys
    maxY   = (abs . maximum) ys

boxSize :: Point -> Int
boxSize (x, y) = x * y

findWord :: [Pt] -> [Pt]
findWord pts | (boxSize (boxDimensionFromPts (map incrementPt pts))) >= (boxSize (boxDimensionFromPts pts)) = pts
findWord pts = findWord (map incrementPt pts)

paintPoints pts = foldl toChar [] allPoints
  where

    pts' = map fst pts
    xs = map fst pts'
    ys = map snd pts'

    maxX = maximum xs
    minX = minimum xs
    maxY = maximum ys
    minY = minimum ys

    allPoints = [(x, y) | y <- reverse [minY..maxY], x <- [minX..maxX] ]

    pointChar pt = if (pt `elem` pts') then '.' else ' '
    toChar  c pt@(x, y) = if x == maxX then c ++ [pointChar pt, '\n'] else c ++ [pointChar pt]


testData = "position=< 9,  1> velocity=< 0,  2>\nposition=< 7,  0> velocity=<-1,  0>\nposition=< 3, -2> velocity=<-1,  1>\nposition=< 6, 10> velocity=<-2, -1>\nposition=< 2, -4> velocity=< 2,  2>\nposition=<-6, 10> velocity=< 2, -2>\nposition=< 1,  8> velocity=< 1, -1>\nposition=< 1,  7> velocity=< 1,  0>\nposition=<-3, 11> velocity=< 1, -2>\nposition=< 7,  6> velocity=<-1, -1>\nposition=<-2,  3> velocity=< 1,  0>\nposition=<-4,  3> velocity=< 2,  0>\nposition=<10, -3> velocity=<-1,  1>\nposition=< 5, 11> velocity=< 1, -2>\nposition=< 4,  7> velocity=< 0, -1>\nposition=< 8, -2> velocity=< 0,  1>\nposition=<15,  0> velocity=<-2,  0>\nposition=< 1,  6> velocity=< 1,  0>\nposition=< 8,  9> velocity=< 0, -1>\nposition=< 3,  3> velocity=<-1,  1>\nposition=< 0,  5> velocity=< 0, -1>\nposition=<-2,  2> velocity=< 2,  0>\nposition=< 5, -2> velocity=< 1,  2>\nposition=< 1,  4> velocity=< 2,  1>\nposition=<-2,  7> velocity=< 2, -2>\nposition=< 3,  6> velocity=<-1, -1>\nposition=< 5,  0> velocity=< 1,  0>\nposition=<-6,  0> velocity=< 2,  0>\nposition=< 5,  9> velocity=< 1, -2>\nposition=<14,  7> velocity=<-2,  0>\nposition=<-3,  6> velocity=< 2, -1>\n"

parseData = (map lineToPt) . lines

solve = findWord . parseData

solve01 = do
  d <- readFile "data/Day10.txt"
  (print . paintPoints . solve) d
