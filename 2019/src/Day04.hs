module Day04 where
import Data.List (sort, group)

isNotDescending x = sort x == x
hasDouble = any (>=2) . map length . group
hasOneExactlyDoubledDigit = elem 2 . map length . group

day04 =
  let ws = filter isNotDescending . map show $ [272091..815432] in
  (length . filter hasDouble $ ws, length . filter hasOneExactlyDoubledDigit $ ws)
