-- | 

module Day10 where
import Data.Sort (sort)
import Data.List (subsequences)

parseInput :: String -> [Int]
parseInput = sort . map read . lines

outputJoltage :: [Int] -> Int
outputJoltage = (+) 3 . maximum 

solve01 :: [Int] -> Int
solve01 jolts = a * b
  where
    (a, b) = go 0 0 ([0] ++ jolts ++ [outputJoltage jolts]) 
    go o t []  = (o, t)
    go o t [_] = (o, t)
    go o t (j:js) =
      if head js - j == 1 then
        go (o + 1) t js
      else
        go o (t + 1) js

isLegalInterval :: [Int] -> Bool
isLegalInterval ls = (== length ls - 1) . length . filter (<= 3) $ zipWith (\a b -> abs (a - b)) ls (tail ls)

findValidIntervals :: Int -> Int -> [Int] -> [[Int]]
findValidIntervals f l ls =
  filter (\l' -> (not . null $ l') && abs (f - head l') <= 3 && abs (l - last l') <= 3 && isLegalInterval l') (subsequences ls)

solve02 :: [Int] -> Int
solve02 ints =

  product $ zipWith (\i l -> length $ findValidIntervals (getFirstFrom i) (getLastFrom i) l) [0..] chunks
  
  where
    chunks = splitIntoIntervals ints

    getFirstFrom :: Int -> Int
    getFirstFrom 0 = 0
    getFirstFrom idx | idx == length chunks = error "Invalid idx"
    getFirstFrom idx =
      last $ chunks !! (idx - 1)

    getLastFrom :: Int -> Int
    getLastFrom idx | idx == length chunks - 1 =
                      3 + last (chunks !! idx)
    getLastFrom idx =
      head (chunks !! (idx + 1)) 

splitIntoIntervals :: [Int] -> [[Int]]
splitIntoIntervals ints =
  go (head ints) [] [] (tail ints)
  where
    go :: Int -> [Int] -> [[Int]] -> [Int] -> [[Int]]
    go i sub acc [] = acc ++ [sub ++ [i]]
    go i sub acc (i':is) =
      if (i' - i) > 3 then
        error "Invalid sequence"
      else if (i' - i) == 3 then
        go i' [] (acc ++ [sub ++ [i]]) is
      else
        go i' (sub ++ [i]) acc is

solution :: IO ()
solution = do
  jolts <- parseInput <$> readFile "data/Day10.txt"
  print (solve01 jolts)
  print (solve02 jolts)
