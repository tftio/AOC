-- | 

module Day10 where
import Data.Sort (sort)
import Data.List (subsequences)

testStr = "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4"
testData = parseInput testStr

testStr2 = "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3"

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
      
test :: [Int]
test = reverse [144,143,142,141,140]

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
      
          
      
  
      
      
  

testData2 = parseInput $ testStr2

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
