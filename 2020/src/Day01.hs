module Day01 where

testData :: [Int]
testData = parseInput "1721\n979\n366\n299\n675\n1456"

parseInput :: String -> [Int]
parseInput = map read . lines

solve01 :: [Int] -> Int
solve01 xs = head [a * b | a <- xs, let b = 2020 - a, b `elem` xs]

solve02 :: [Int] -> Int
solve02 xs = head [a * b * c | a <- xs, b <- xs, a /= b, let c = 2020 - a - b, c `elem` xs]

solution :: IO ()
solution = do
  s <- readFile "data/Day01.txt"
  let s' = parseInput s
  print (solve01 s')
  print (solve02 s')

