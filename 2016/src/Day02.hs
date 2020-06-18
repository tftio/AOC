-- day 2
-- test data: ULL RRDDD LURDL UUUUD

module Day02 where

testData = ["ULL", "RRDDD", "LURDL", "UUUUD"]

data Direction = U | D | L | R deriving Show
type Button = Int
type Moves = [Direction]
type TransitionTable = [[Int]]

firstTransitionTable = [[1, 4, 1, 2],
                        [2, 5, 1, 3],
                        [3, 6, 2, 3],
                        [1, 7, 4, 5],
                        [2, 8, 4, 6],
                        [3, 9, 5, 6],
                        [4, 7, 7, 8],
                        [5, 8, 7, 9],
                        [6, 9, 8, 9]]

secondTransitionTable = [[1,3,1,1],
                         [2,6,2,3],
                         [1,7,2,4],
                         [4,8,3,4],
                         [5,5,5,6],
                         [2,10,5,7],
                         [3,11,6,8],
                         [4,12,7,9],
                         [9,9,8,9],
                         [6,11,10,10],
                         [7,13,10,12],
                         [8,12,11,12],
                         [11,13,13,13]]

moveOverButtons :: TransitionTable -> Button -> Direction -> Button
moveOverButtons t b d =
  line !! (idx d)
  where
    line = t !! (b - 1)
    idx :: Direction -> Int
    idx d' = case d' of
      U -> 0
      D -> 1
      L -> 2
      R -> 3

-- input is one set per line
readData :: String -> Moves
readData input =
  map charToDirection input
  where
    charToDirection :: Char -> Direction
    charToDirection s =
      case s of
        'U' -> U
        'D' -> D
        'R' -> R
        'L' -> L
        _   -> error "Invalid data"

readDataFile :: String -> [Moves]
readDataFile fp =
  map readData (lines fp)

solve :: TransitionTable -> [Moves] -> [Button]
solve table moves =
  aux [] 5 moves
  where
    aux acc start moves =
      case moves of
        [] -> reverse acc
        m:ms -> aux (b:acc) b ms
          where
            b = foldl (moveOverButtons table) start m
