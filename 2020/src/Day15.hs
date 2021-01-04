module Day15 where

import qualified Data.IntMap as M

puzzleInput :: [Int]
puzzleInput = [7,12,1,0,16,2]

testInput :: [Int]
testInput = [0,3,6]

go (i, map, v) = (i + 1, M.insert v i map, v')
  where
    v' = case M.lookup v map of
      Nothing -> 0
      Just x  -> i - x

solve reps = (\(_, _, x) -> x). until (\(i, _, _) -> i == reps) go . 
  (\l -> (length l, M.fromList (zip (init l) [1..]), last l))
