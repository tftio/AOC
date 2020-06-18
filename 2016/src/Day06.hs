module Day06 where

import qualified Data.Text as Text
import Data.List

type T = Text.Text

inputToLongText :: T -> T
inputToLongText = Text.concat . Text.lines

getMostPopularChar sortRoutine =
  head . head . (sortBy sortRoutine) . group . sort

transform routine t =
  Text.concat (map (\idx -> getMostPopularChar routine ([c | (i, c) <- pairs, i `mod` 8 == idx])) [0..7])
  where pairs = zip [0..] (Text.chunksOf 1 (inputToLongText (Text.pack t)))

partOne = transform (\a b -> compare (length b) (length a))
partTwo = transform (\a b -> compare (length a) (length b))
