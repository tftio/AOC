module Day18 where
import qualified Data.Map.Strict as Map
import Data.List (repeat, intercalate)
import Data.Maybe (isJust, mapMaybe)

testData = map (\l -> intercalate "\n" l) [
  -- Initial state:
  [".#.#...|#.",".....#|##|",".|..|...#.","..|#.....#","#.#|||#|#|","...#.||...",".|....|...","||...#|.#|","|.||||..|.","...#.|..|."],

  -- After 1 minute:
  [".......##.","......|###",".|..|...#.","..|#||...#","..##||.|#|","...#||||..","||...|||..","|||||.||.|","||||||||||","....||..|."],

  -- After 2 minutes:
  [".......#..","......|#..",".|.|||....","..##|||..#","..###|||#|","...#|||||.","|||||||||.","||||||||||","||||||||||",".|||||||||"],

  -- After 3 minutes:
  [".......#..","....|||#..",".|.||||...","..###|||.#","...##|||#|",".||##|||||","||||||||||","||||||||||","||||||||||","||||||||||"],

  -- After 4 minutes:
  [".....|.#..","...||||#..",".|.#||||..","..###||||#","...###||#|","|||##|||||","||||||||||","||||||||||","||||||||||","||||||||||"],

  -- After 5 minutes:
  ["....|||#..","...||||#..",".|.##||||.","..####|||#",".|.###||#|","|||###||||","||||||||||","||||||||||","||||||||||","||||||||||"],

  -- After 6 minutes:
  ["...||||#..","...||||#..",".|.###|||.","..#.##|||#","|||#.##|#|","|||###||||","||||#|||||","||||||||||","||||||||||","||||||||||"],

  -- After 7 minutes:
  ["...||||#..","..||#|##..",".|.####||.","||#..##||#","||##.##|#|","|||####|||","|||###||||","||||||||||","||||||||||","||||||||||"],

  -- After 8 minutes:
  ["..||||##..","..|#####..","|||#####|.","||#...##|#","||##..###|","||##.###||","|||####|||","||||#|||||","||||||||||","||||||||||"],

  -- After 9 minutes:
  ["..||###...",".||#####..","||##...##.","||#....###","|##....##|","||##..###|","||######||","|||###||||","||||||||||","||||||||||"],

  -- After 10 minutes:
  [".||##.....","||###.....","||##......","|##.....##","|##.....##","|##....##|","||##.####|","||#####|||","||||#|||||","||||||||||"]
  ]
  
data Acre = Open | Tree | Lumberyard deriving (Show, Eq, Ord)
data Point = Point Int Int deriving (Show, Eq, Ord)
type Area = Map.Map Point Acre

charToAcre :: Char -> Acre
charToAcre c = case c of
  '.' -> Open
  '|' -> Tree
  '#' -> Lumberyard
  otherwise -> error ("Cannot parse '" ++ [c] ++ "'")

acreToChar :: Acre -> Char
acreToChar Open = '.'
acreToChar Tree = '|'
acreToChar Lumberyard = '#'

parseInput :: String -> Area
parseInput s = 
  foldr (\m m' -> Map.union m m') Map.empty (map (\(y, l) -> stringToLine y l) (zip [0..] (lines s)))
  where
    stringToLine y s = Map.fromList (zip (map (\(x, y) -> Point x y) (zip [0..] (repeat y))) (map charToAcre s))

evolvePoint :: Area -> Point -> Acre
evolvePoint a p =
  case acre of
    Open | (pred 3 Tree) -> Tree
    Tree | (pred 3 Lumberyard) -> Lumberyard
    Lumberyard | (pred 1 Lumberyard) && (pred 1 Tree) -> Lumberyard
    Lumberyard -> Open
    _ -> acre
  where
    acre = a Map.! p
    pts  = pointsAround a p
    pred ct t = (length . filter (\a -> a == t)) pts >= ct

    pointsAround a (Point x y) =
      mapMaybe (\p -> Map.lookup p a) [Point (x + 1) y, Point (x - 1) y, Point x (y + 1), Point x (y - 1),
                                       Point (x + 1) (y + 1), Point (x + 1) (y - 1), Point (x - 1) (y + 1), Point (x - 1) (y - 1)]

runMinute :: Area -> Area
runMinute a = 
  (Map.fromList . map (\(p, _) -> (p, evolvePoint a p)) . Map.toList) a

solve :: Int  -> Area -> Int
solve ct area =
  resourceValue (aux ct area)
  where
    aux 0 a = a
    aux i a = aux (i - 1) (runMinute a)

floyd :: (Eq a) => (a -> a) -> a -> (a, Int, Int)
floyd f term = go (f term) (f . f $ term)
  where
    go      t h     | t == h    = findMu term h 0
                    | otherwise = go (f t) (f . f $ h)
    findMu  t h mu  | t == h    = (t, mu, findLam t (f t) 1)
                    | otherwise = findMu (f t) (f h) (mu + 1)
    findLam t h lam | t == h    = lam
                    | otherwise = findLam t (f h) (lam + 1)

resourceValue :: Area -> Int
resourceValue area =
  trees * lumberyards
  where
    trees = Map.size (Map.filter (\a -> a == Tree) area)
    lumberyards = Map.size (Map.filter (\a -> a == Lumberyard) area)

part01 m = solve 10 m
part02 m =
      let (a', mu, lambda) = floyd runMinute m
      in solve ((1000000000 - mu) `rem` lambda) a'

solver :: IO()
solver = do
  m <- parseInput <$> readFile "data/Day18.txt"
  
  print $ part01 m
  print $ part02 m
