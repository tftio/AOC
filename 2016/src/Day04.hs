module Day04 (first, second) where

import qualified Data.List.Split as S
import Data.List
import Data.Char

type Name = String
type Checksum = String
type SectorID = Int
type Room = (Name, SectorID, Checksum)

parseRoomFromString :: String -> Room
parseRoomFromString str =
  let str' = reverse str
      [_, checksum, str''] = S.splitOneOf "[]" str'
      id:name = S.splitOn "-" str''
      name' = intercalate "-" name
  in
  ((reverse name'), read (reverse id), (reverse checksum))

roomIsValid :: Room -> Bool
roomIsValid (name, _, checksum) = (computeChecksum name) == checksum

computeChecksum =
  let
    count = (map (\xs@(x:_) -> (x, length xs)) . group . sort)
    sorter = sortBy (\(_, a) (_, b) -> compare b a)
    flatten = map (\(c, _) -> c)
    removeDupes = nub in
  (take 5) . removeDupes . flatten . sorter . count . filter isLetter

shiftLetter :: Int -> Char -> Char
shiftLetter ct c =
  case c of
    '-' -> ' '
    _  -> chr ((((ord c - 97) + ct) `mod` 26) + 97)

decode :: Room -> String
decode (name, sectorId, checksum) =
  map (shiftLetter sectorId) name

first :: [String] -> Int
first lines = foldl (\sum (_, sectorId, _) -> sum + sectorId) 0 (filter roomIsValid (map parseRoomFromString lines))

second :: [String] -> String
second lines = intercalate "\n" (map (\r@(_, id, _) -> ((decode r) ++ " " ++ (show id))) (filter roomIsValid (map parseRoomFromString lines)))

