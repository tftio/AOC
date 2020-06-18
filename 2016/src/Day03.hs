module Day03 where

type Triangle = (Int, Int, Int)

intsFromStr :: String -> [Int]
intsFromStr input =
  map (\i -> read i ::Int) (words input)

take3 :: [Int] -> [Triangle]
take3 =
  take3' []
  where
      take3' acc [] = acc
      take3' acc ints' = take3' ((i,j,k):acc) rest
        where [i,j,k] = take 3 ints'
              rest    = drop 3 ints'

skip3 :: [Int] -> [Triangle]
skip3 ints =
  take3 ([a | (i, a) <- ints', i `mod` 3 == 0 ] ++
         [b | (i, b) <- ints', i `mod` 3 == 1 ] ++
         [c | (i, c) <- ints', i `mod` 3 == 2 ])
  where
    ints' = zip [0..] ints

trianglesFromString :: String -> ([Int] -> [Triangle]) -> [Triangle]
trianglesFromString s fn =
  fn (intsFromStr s)

legal (a, b, c) = a + b > c && a + c > b && c + b > a

