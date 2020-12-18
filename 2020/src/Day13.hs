module Day13 where

import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Data.Sort (sortOn)

testStr = "939\n7,13,x,x,59,x,31,19"

-- parseInput :: String -> (Int, [Int])
parseInput s = (start, sortOn fst . map toRange . mapMaybe readMaybe . splitOn "," $ xs)
  where [x,xs] = lines s
        start = read x :: Int
        toRange p = head $ [(p, i) | i <- [0,p..], i > start]

solve01 :: String -> Int
solve01 s =
  let
    (start, ps) = parseInput s
    (bus, m) = head ps
  in
    bus * (m - start)
-- solve01 :: Int -> [Maybe Int] -> Int
-- solve01 start =
--   routeNum * earliestTime
--   where
--     map (\i -> head $ [(x, i) | x <- [0,i..], x > start])
    

 
solution = do
  s <- readFile "data/Day13.txt"
  print (solve01 s)
  
