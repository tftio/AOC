module Day03 where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List

nextSmallestOddRoot n =
  if n' `mod` 2 == 1 then n' else n' - 1
  where
    n' = floor (sqrt (fromIntegral n))

sideLength 1 = 1
sideLength n =
  if (n' * n') >= n then n' else n' + 2
  where
    n' = nextSmallestOddRoot n

ringNum n = (sideLength n - 1) `div` 2

distance 1 = 0
distance n = ((n - (s * s)) `mod` (ringNum n)) + ringNum n
  where
    s = nextSmallestOddRoot n

data Direction = U | D | L | R deriving Show

move (d, (x, y)) =
  (d', (x', y'))
  where
    d' = nextDirection (d, (x', y'))
    (x', y') = case d of
      U -> (x, y + 1)
      D -> (x, y - 1)
      L -> (x - 1, y)
      R -> (x + 1, y)
    shouldTurn (x, y) = if x > 0 && y < 0 then abs x == (abs y) + 1 else abs x == abs y
    turn direction =
      case direction of
        U -> L
        L -> D
        D -> R
        R -> U
    nextDirection (d, p) = if shouldTurn p then turn d else d

solutionTwo n =
  head (filter (\(_, s) -> s > n) (Data.List.sortBy (\(_, s) (_, s') -> compare s s') (Map.toList (foldl addPoint (createMap 200) (getCoordinates 200)))))
  where
    getCoordinates ct =
      map (\(_, (x, y)) -> (x, y))
      (reverse
       (foldl (\(n:ns) i -> (move n):n:ns)
        [(U, (1, 0)), (R, (0, 0))]
        [1..(ct - 2)]))
    addPoint m pt = Map.insert pt (sumAroundPoint m pt) m
      where
        sumAroundPoint (x, y) = sum (map (\c -> Map.findWithDefault 0 c m) [(x', y') | x' <- [x - 1..x+1], y' <- [y-1..y+1]])
    createMap ct = Map.insert (0, 0) 1 (Map.fromList (map (\c -> (c, 0)) (getCoordinates ct)))



