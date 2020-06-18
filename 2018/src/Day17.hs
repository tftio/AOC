module Day17 where
import Data.List.Split
import qualified Data.List as List
import qualified Data.Map

data Piece = Clay | Sand | WaterAtRest | WaterFalling deriving (Eq, Ord, Show)

type Point = (Int, Int)
type Board = Data.Map.Map Point Piece

makeSplit delims = (dropInitBlank . dropFinalBlank . condense . dropDelims $ oneOf delims)

clay :: String -> Board
clay = foldr (\m m' -> Data.Map.union m m') Data.Map.empty . map Data.Map.fromList . filter (\l -> length l > 3) . map lineToPoints . lines

lineToPoints "" = []
lineToPoints s  =
  map (\p -> (p, Clay)) pts
  where
      pts = if length x == 1 then
              zip (List.repeat (head x)) y
            else
              zip x (List.repeat (head y)) 

      ls = split (makeSplit ":,= ") s
      [x, y] = map strToRange (if (head ls) == "x" then
                                 [ls !! 1, ls !! 3]
                               else
                                 [ls !! 3, ls !! 1])
      strToRange s =
        [from..to]
        where
          [from, to] = map (\s -> read s :: Int) (if List.elem '.' s then
                                                    split (makeSplit ".") s
                                                  else
                                                    [s, s])
boardSize :: Board -> (Point, Point)
boardSize b =
  ((minX, 0), (maxX, maxY))
  where
    maxX = maximum xs
    maxY = maximum ys
    minX = minimum xs
    (xs, ys) = (unzip . map fst . Data.Map.toList) b
    
testData = List.intercalate "\n" ["x=495, y=2..7", "y=7, x=495..501", "x=501, y=3..7", "x=498, y=2..4", "x=506, y=1..2", "x=498, y=10..13", "x=504, y=10..13", "y=13, x=498..504"]

nextClay :: Point -> Board -> (Point, Piece)
nextClay (spoutX, spoutY) =
  head . List.sortBy (\((_, y), _) ((_, y'), _) -> compare y y') . filter (\((x, y), p) -> x == spoutX && y > spoutY) . Data.Map.toList

paint :: Board -> [String]
paint board =
  map (\y -> map toChar (zip [minX..maxX] (List.repeat y))) [minY..maxY]
  where
    toChar p = if p == (500,0) then '+' else
                 case (Data.Map.findWithDefault Sand p board) of
                   Clay         -> '#'
                   Sand         -> '.'
                   WaterAtRest  -> '~'
                   WaterFalling -> '|'
    ((minX, minY), (maxX, maxY)) = boardSize board

waterInBoard :: Board -> Int
waterInBoard board =
  (length . filter f . Data.Map.toList) board
  where
    f ((x, y), p) = (y > minY) &&
                    (y <= maxY) &&
                    (p == WaterAtRest || p == WaterFalling)
    ((_, minY), (_, maxY)) = boardSize board
    
solve01 :: Board -> Int
solve01 _ = 0

test01 = solve01 (clay testData) == 57

