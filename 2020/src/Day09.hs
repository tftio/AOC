module Day09 where
import Data.Set (fromList, Set, member)

testStr :: String
testStr = "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576\n"

testData :: [Int]
testData = parseData testStr

parseData :: String -> [Int]
parseData = map read . lines

getSums :: [Int] -> Set Int
getSums ns = fromList [ x + y | y <- ns, x <- ns, x /= y ]

solve01 :: [Int] -> [Int] -> Int
solve01 _ [] = error "Can't find a solution"
solve01 prelude (i:is) =
  if not . member i . getSums $ prelude then i
  else
    solve01 (tail prelude ++ [i]) is

solve02 :: Int -> [Int] -> Int
solve02 targetSum ints =
  minimum ls + maximum ls
  where
    ls = go 0 2
    go start len | checkSum start len == targetSum = slice start len ints
    go start len | checkSum start len <  targetSum = go start (len + 1) 
    go start len | checkSum start len >  targetSum = go (start + 1) 2 -- reset to one further, an a min length of two
    checkSum s l = sum (slice s l ints)
    slice s l = take l . drop s

solution :: IO ()
solution = do
  ints <- parseData <$> readFile "data/Day09.txt"
  let part01 = solve01 (take 25 ints) (drop 25 ints)
  let part02 = solve02 part01 ints
  print (part01, part02)
  
