-- | 

module Day01 where

fuel :: Int -> Int
fuel m = if v > 0 then v else 0
  where
    v = (floor . (/ 3.0) . fromIntegral $  m) - 2

doInt :: [Char] -> Int
doInt s = read s :: Int

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
  
