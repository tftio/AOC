module Main where

import Day06
import Day07

main :: IO ()
main = do
  let bank = map (\s -> read s :: Int) (words "0 5 10 0 11 14 13 4 11 8 8 7 1 4 12 11")
  print (solveOne bank)
  print (solveTwo bank)

