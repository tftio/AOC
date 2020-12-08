-- | 

module Day05 where
import Data.Sort

testData :: [(String, Int)]
testData = [("FBFBBFFRLR", 357),
            ("BFFFBBFRRR", 567),
            ("FFFBBBFRRR", 119),
            ("BBFFBBFRLL", 820)]

test :: Bool
test = all (\(s, v) -> seatId s == v) testData

seatId :: String -> Int
seatId = toInt . stringToInts
  where
    stringToInts = map (\c -> if c == 'B' || c == 'R' then 1 else 0)
    toInt l = sum $ zipWith (\e m -> m * 2 ^ e) [0..(length l)] (reverse l)

solve01 :: [Int] -> Int
solve01 = maximum 

solve02 :: [Int] -> Int
solve02 l =
  head $ filter (\x -> (x - 1) `elem` l && x `notElem` l && (x + 1) `elem` l) [0..1024]

solution :: IO () 
solution = do
  passes <- map seatId . lines <$> readFile "data/Day05.txt"
  print (solve01 passes)
  print (solve02 (sort passes))
