-- | 

module Day08 where

import qualified Data.Vector.Unboxed as V
import Data.Char

type Pixels = V.Vector Int

type Image = ((Int, Int), Int, Pixels) 

checksum :: Image -> Bool
checksum ((x, y), layers, d) =
  x * y * layers == V.length d

imageSize :: Image -> Int
imageSize ((x, y), l, _) = x * y * l

newImage :: (Int, Int) -> [Int] -> Image
newImage (x, y) ps = 
  let ps' = V.fromList ps in
    ((x, y), V.length ps' `div` (x * y), ps')

-- layer is 0 based
getPixelAt :: (Int, Int) -> Int -> Image -> Int
getPixelAt (x, y) l ((x', y'), _, pixels) = pixels V.! ((x' * y' * l) + (x * y))
  
getLayerAt :: Image -> Int -> Pixels
getLayerAt ((x, y), _, pixels) layer =
  V.slice (layer * x * y) (x * y) pixels

stringToPixels :: [Char] -> [Int]
stringToPixels = map digitToInt

testImage = newImage (3, 2) (stringToPixels "123456789012")


