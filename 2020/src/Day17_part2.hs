{-# LANGUAGE OverloadedStrings #-}

module Day17_part2 where

import Data.List (groupBy, intercalate, sortOn)
import qualified Data.Map as M
import Data.Maybe

type Point = (Int, Int, Int, Int)

type Size = (Int, Int, Int, Int)

type Space = M.Map Point Int

day17Input :: String
day17Input = "......##\n####.#..\n.##....#\n.##.#..#\n........\n.#.#.###\n#.##....\n####.#.."

testInput :: String
testInput = ".#.\n..#\n###"

testSpace :: Space
testSpace = parseSpace testInput

day17Space :: Space
day17Space = parseSpace day17Input

findAdjacentPoints :: Point -> [Point]
findAdjacentPoints (x, y, z, w) =
  filter (/= (x, y, z, w)) [(a, b, c, d) | a <- [x -1 .. x + 1], b <- [y -1 .. y + 1], c <- [z -1 .. z + 1], d <- [w - 1 .. w + 1]]

parseSpace :: String -> Space
parseSpace str =
  foldr M.union M.empty (zipWith (\y s -> M.fromList (parseLine y s)) [0 ..] . lines $ str)
  where
    parseLine y = zipWith (\x c -> ((x, y, 0, 0), if c == '.' then 0 else 1)) [0 ..]

sizeOfSpace :: Space -> Size
sizeOfSpace s =
  ( 1 + abs minX + abs maxX,
    1 + abs minY + abs maxY,
    1 + abs minZ + abs maxZ,
    1 + abs minW + abs maxW
  )
  where
    (minX, minY, minZ, minW) = boundsOfSpace min s
    (maxX, maxY, maxZ, maxW) = boundsOfSpace max s

boundsOfSpace :: (Int -> Int -> Int) -> Space -> Size
boundsOfSpace f s = (x', y', z', w')
  where
    (x', y', z', w') =
      foldr (\(x, y, z, w) (xm, ym, zm, wm) -> (f x xm, f y ym, f z zm, f w wm)) (fst . head . M.toList $ s) $ M.keys s

test :: Bool
test =
  (2, 2, 0, 0) == boundsOfSpace max testSpace
    && (0, 0, 0, 0) == boundsOfSpace min testSpace

--     && spaceToString testSpace == testInput

changeState :: Space -> Space
changeState space =
  M.mapWithKey flipPoint (enclosingCube space)
  where
    flipPoint pt v =
      case (v, c) of
        (0, 3) -> 1
        (1, 3) -> 1
        (1, 2) -> 1
        _ -> 0
      where
        c = findAdjacentValues space pt

enclosingCube space =
  M.unionWith max space $
    M.fromList [((x, y, z, w), 0) | x <- [minX - 1 .. maxX + 1], y <- [minY - 1 .. maxY + 1], z <- [minZ - 1 .. maxZ + 1], w <- [minW - 1 .. maxW + 1]]
  where
    (maxX, maxY, maxZ, maxW) = boundsOfSpace max space
    (minX, minY, minZ, minW) = boundsOfSpace min space

findAdjacentValues :: Space -> Point -> Int
findAdjacentValues space point = sum $ mapMaybe (space M.!?) (findAdjacentPoints point)

countActive = M.foldr (+) 0

solve02 =
  length . filter (\(_, v) -> v == 1) . M.toList . go 6
  where
    go 0 s = s
    go c s = go (c - 1) (changeState s)
