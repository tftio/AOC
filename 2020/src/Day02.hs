-- | 

module Day02 where
import Data.List.Split (splitOn)

type Password = String

data Policy = Policy Char Int Int deriving Show

type Result = (Policy, Password)

testString = "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc"
testData   = (map parseInput . lines) testString

parseInput :: String -> Result
parseInput s = (Policy c min max, password)
  where [min,max]  = getMinMax part1
        c          = head      part2
        password   = getPwd    part3

        [part1,part2,part3] = splitOn " " s
        getMinMax = map read . splitOn "-"
        getPwd = id

testPassword01 :: Result -> Bool
testPassword01 (Policy c min max, pwd) =
  min <= c' && c' <= max
  where
    c' = length (filter (== c) pwd)

testPassword02 :: Result -> Bool
testPassword02 (Policy c a b, pwd) =
  (pwd !! a' == c && pwd !! b' /= c) ||
  (pwd !! a' /= c && pwd !! b' == c)
  where
    a' = a - 1
    b' = b - 1
  
solve01 = length . filter (== True) . map (testPassword01 . parseInput) . lines
solve02 = length . filter (== True) . map (testPassword02 . parseInput) . lines 

solution = do
  s <- readFile "data/Day02.txt"
  print (solve01 s)
  print (solve02 s)   
