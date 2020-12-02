-- | 

module Day00 where

test :: Bool
test =
  fuel 12 == 2 &&
  fuel 14 == 2 &&
  fuel 1969 == 654 &&
  fuel 100756 == 33583    

fuel :: Int -> Int
fuel = max 0 . subtract 2 . flip div 3 

stringToMass :: String -> Int
stringToMass s = read s :: Int

computeAnswer :: String -> Int
computeAnswer =
  sum . map (fuel . stringToMass) . lines

newFuel :: Int -> Int
newFuel = id

helper :: Int -> Int -> Int
helper mass runningTotal | fuel mass <= 0 = runningTotal
helper mass runningTotal | otherwise      =
  helper (fuel mass) (runningTotal + fuel mass)
  
part01 :: IO ()
part01 = do
  raw <- readFile "data/Day00.txt"
  print (computeAnswer raw)
