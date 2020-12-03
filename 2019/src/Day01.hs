-- | 

module Day01 where


fuel :: Int -> Int
fuel = subtract 2 . floor . (/ 3.0) . fromIntegral

doInt :: [Char] -> Int
doInt s = read s :: Int

stringToFuel :: [Char] -> Int
stringToFuel s = read s :: Int

solve01 :: IO ()
solve01 = do
  m <- readFile "data/Day01.txt"
  let answer = sum . map (fuel . doInt) . lines $ m
  print answer

bigFuel :: Int -> Int
bigFuel mass =
  aux mass 0
  where
    aux m acc | fuel m == 0 = acc
    aux m acc = aux (fuel m) (acc + fuel m)

solve02 :: IO ()
solve02 = do
  m <- readFile "data/Day01.txt"
  let answer = sum . map (bigFuel . doInt) . lines $ m
  print answer
  
