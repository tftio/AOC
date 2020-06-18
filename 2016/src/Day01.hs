{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Day01 where

import qualified Text.Printf as P
import qualified Debug.Trace as T
import qualified Data.List   as L
import qualified Data.Set    as Set

data Direction = N | S | E | W deriving Show
data Turn = L | R deriving Show
type Distance = Int
type Point = (Int, Int)

type Move = (Turn, Distance)
type Location = (Point, Direction)

initialPoint = (0, 0)
initialLocation = (initialPoint, N)

pointToStr :: Point -> String
pointToStr (x, y) = P.printf "(%i,%i)" x y

moveToStr :: Move -> String
moveToStr (t, d) =
  P.printf "%s%i" (show t) d

locationToStr :: Location -> String
locationToStr (p, d) =
  P.printf "%s facing %s (final distance %i)" (pointToStr p) (show d) distance
  where
    distance = manhattanDistance p initialPoint

manhattanDistance :: Point -> Point -> Int
manhattanDistance (x, y) (x', y') = abs (x - x') + abs (y - y')

moveOfStr :: String -> Move
moveOfStr m =
  let r = [head m]
      d = read (tail m) :: Int
  in
  case r of
    "R" -> (R, d)
    "L" -> (L, d)
    _   -> error "Invalid data"

executeMove :: Location -> Move -> Location
executeMove ((x, y), direction) (turn, distance) =
  let direction' = directionFromTurn direction turn
      point'     = case direction' of
        N -> (x, (y + distance))
        S -> (x, (y - distance))
        E -> ((x + distance), y)
        W -> ((x - distance), y)
  in
    (point', direction')
  where
    idxOfDirection :: Direction -> Int
    idxOfDirection d = case d of
      N -> 0
      E -> 1
      S -> 2
      W -> 3
    directionOfIdx :: Int -> Direction
    directionOfIdx i = case i of
      0 -> N
      1 -> E
      2 -> S
      3 -> W
      _ -> error "invalid idx"
    magnitudeOfTurn :: Turn -> Int
    magnitudeOfTurn i = case i of
      R -> 1
      L -> -1
    directionFromTurn :: Direction -> Turn -> Direction
    directionFromTurn d t =
      let i = idxOfDirection d
          i' = (i + (magnitudeOfTurn t)) `mod` 4
      in
        directionOfIdx i'

visited :: Point -> Point -> [Point]
visited (x, y) (x', y') =
  let p =  if x == x' then
             if y < y' then
               [(x, i) | i <- [y..y']]
             else
               reverse [(x, i) | i <- [y'..y]]
           else
             if x < x' then
               [(i, y) | i <- [x..x']]
             else
               reverse [(i, y) | i <- [x'..x]]
  in
    tail p

walkTheLine :: Set.Set Point -> Point -> Point -> (Set.Set Point, Maybe Point)
walkTheLine s p p' =
  let allPs = visited p p'
  in
    walkTheLine' s (tail allPs)
  where
    walkTheLine' s ps = case ps of
                          [] -> (s, Nothing)
                          p:ps' -> if Set.member p s then (s, Just p)
                                   else walkTheLine' (Set.union (Set.singleton p) s) ps'
find :: [Move] -> Maybe (Point, Int)
find moves =
  let startSet = Set.empty in
    case (find' startSet initialLocation moves) of
      (_, Just p) -> Just (p, (manhattanDistance p initialPoint))
      (_, Nothing) -> Nothing
  where
    find' set location@(point, _) moves =
      case moves of
        [] -> (set, Nothing) -- the end
        m:ms -> let location'@(point', _) = executeMove location m -- ok, we have something
                in
                  case (walkTheLine set point point') of
                    (s, Just p)  -> (s, Just p)
                    (s, Nothing) -> find' s location' ms

getPath :: [Move] -> [Point]
getPath moves =
  getPath' [] initialLocation moves
  where
    getPath' path location moves' | T.trace ((show path) ++ " " ++ (locationToStr location)) False = undefined
    getPath' path location@(point, _) moves' =
      case moves' of
        [] -> path
        m:ms -> let location'@(point', _) = executeMove location m in
                   getPath' (path ++ (visited point point')) location' ms

moves = map moveOfStr ["R5", "R4", "R2", "L3", "R1", "R1", "L4", "L5", "R3", "L1", "L1", "R4", "L2", "R1", "R4", "R4", "L2", "L2", "R4", "L4", "R1", "R3", "L3", "L1", "L2", "R1", "R5", "L5", "L1", "L1", "R3", "R5", "L1", "R4", "L5", "R5", "R1", "L185", "R4", "L1", "R51", "R3", "L2", "R78", "R1", "L4", "R188", "R1", "L5", "R5", "R2", "R3", "L5", "R3", "R4", "L1", "R2", "R2", "L4", "L4", "L5", "R5", "R4", "L4", "R2", "L5", "R2", "L1", "L4", "R4", "L4", "R2", "L3", "L4", "R2", "L3", "R3", "R2", "L2", "L3", "R4", "R3", "R1", "L4", "L2", "L5", "R4", "R4", "L1", "R1", "L5", "L1", "R3", "R1", "L2", "R1", "R1", "R3", "L4", "L1", "L3", "R2", "R4", "R2", "L2", "R1", "L5", "R3", "L3", "R3", "L1", "R4", "L3", "L3", "R4", "L2", "L1", "L3", "R2", "R3", "L2", "L1", "R4", "L3", "L5", "L2", "L4", "R1", "L4", "L4", "R3", "R5", "L4", "L1", "L1", "R4", "L2", "R5", "R1", "R1", "R2", "R1", "R5", "L1", "L3", "L5", "R2"]
testMoves = map moveOfStr ["R8", "R4", "R4", "R8"]
finalLocation = foldl executeMove initialLocation moves
