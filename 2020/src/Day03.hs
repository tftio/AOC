-- | 

module Day03 where

testInput :: [String]
testInput = ["..##.......",
             "#...#...#..",
             ".#....#..#.",
             "..#.#...#.#",
             ".#...##..#.",
             "..#.##.....",
             ".#.#.#....#",
             ".#........#",
             "#.##...#...",
             "#...##....#",
             ".#..#...#.#"]

testData :: [[Int]]
testData = map stringToRow testInput

stringToRow :: [Char] -> [Int]
stringToRow = cycle . map (\c -> if c == '#' then 1 else 0)

answer :: (Int, Int) -> [[Int]] -> Int
answer (x,y) =
  sum . zipWith (\i r -> r !! (i * x)) [0..] . stride (y - 1)

stride :: Int -> [a] -> [a]
stride s = go
  where
    go (x:xs) = x : go (drop s xs)
    go _      = []

solve01 :: [[Int]] -> Int
solve01 = answer (3,1)

solve02 :: [[Int]] -> Int
solve02 m =
  product (map (`answer` m) [(1,1), (3,1), (5,1), (7,1), (1,2)])
  
solution :: IO ()
solution = do
  s <- map stringToRow . lines <$> readFile "data/Day03.txt"
  print (solve01 s)
  print (solve02 s)
