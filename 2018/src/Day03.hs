module Day03 where
import qualified Data.Set as Set
import qualified Data.Map as M
import Data.List.Split
       
type Point  = (Int, Int)
type Size   = (Int, Int)
type Square = (Int, Point, Size)
type Points = M.Map Point Int

pointsFromSquare :: Square -> Points
pointsFromSquare (_, (x, y), (w, h)) =
  M.fromList [((x', y'), 1) | x' <- [x .. (x + w - 1)], y' <- [y .. (y + h - 1)]]

parseInput :: [Char] -> Square
parseInput str = 
  (n, (x, y), (w, h)) where
  n:x:y:w:h:[] =
    map (\x -> read x :: Int) (split (condense . dropDelims . dropInnerBlanks . dropInitBlank . dropFinalBlank $ oneOf "# @,:x") str)

squareIntersects :: Square -> Points -> Bool
squareIntersects s map =
  M.size (M.filter (\a -> a > 1) (M.intersection map (pointsFromSquare s))) > 0

testData = "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2\n"

squares   = map parseInput . lines
createMap = M.unionsWith (+) . map pointsFromSquare . squares

solution01 = M.size . M.filter (\a -> a > 1) . createMap
solution02 input =
  filter (\s -> not (squareIntersects s m)) (squares input)
  where
    m = createMap input
  
solve01 = do
  sqs <- readFile "./data/Day03.txt"
  print (solution01 sqs)

solve02 = do
  sqs <- readFile "./data/Day03.txt"
  print (solution02 sqs)

