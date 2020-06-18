module Day06 where
import           Data.List
import           Data.List.Split
import qualified Data.Map        as Map
import           Data.Maybe

type Point = (Int, Int)
type Box   = (Point, Point)

testData = "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9\n"
testSolution = [((3,4), 9), ((5,5), 17)]

getData = map barf . map parseLine . lines where
  parseLine = map (\s -> read s :: Int) . (split (condense . dropDelims . dropInnerBlanks . dropInitBlank . dropFinalBlank $ oneOf " ,"))
  barf [a,b] = (a, b)
  barf _     = error "must be a pair"

buildBox :: [Point] -> Box
buildBox points = ((minX, minY), (maxX, maxY)) where
  xs   = map fst points
  ys   = map snd points
  minX = minimum xs
  minY = minimum ys
  maxX = maximum xs
  maxY = maximum ys

pointsFromBox :: Box -> [Point]
pointsFromBox ((x, y), (x', y')) = [ (x'', y'') | x'' <- [x - 1..x' + 1], y'' <- [y - 1..y' + 1]]

-- for every point, measure it's distance to one of the coords:




sortDistances :: Point -> [Point] -> [(Point, Int)]
sortDistances pt coords = sortOn snd (zip coords (map (\p -> manhattanDistance p pt) coords))

dropOutsidePoints :: [Point] -> Box -> [Point]
dropOutsidePoints points ((ax, ay), (bx, by)) = filter insideBox points where
  insideBox :: Point -> Bool
  insideBox (x, y) = (x > ax) && (y > ay) && (x < bx) && (y < by)

manhattanDistance :: Point -> Point -> Int
manhattanDistance (x, y) (x', y') = abs (x - x') + abs (y - y')




solve01 str =
  (head . reverse . sortOn snd)
  (filter (\(pt, _) -> pt `elem` insidePoints)
   (map (\l -> (head l, length l))
     ((group . sort) (map (fst . head) (mapMaybe closestValidPoint allPossiblePts)))))
  where
    pts            = getData str
    box            = buildBox pts
    insidePoints   = dropOutsidePoints pts box
    allPossiblePts = pointsFromBox box
    hasDupes    ds = f == s where
      f = head ds
      s = head (tail ds)
    closestValidPoint point =
      if hasDupes (map snd (sortDistances point pts)) then
        Nothing
      else
        Just (sortDistances point pts)
      -- let
      --   distances = sortDistances point pts in
      -- if hasDupes (map snd distances) then
      --   Nothing
      -- else
      --   Just (sortDistances point pts)

solution01 = do
  s <- readFile "data/Day06.txt"
  print (solve01 s)
