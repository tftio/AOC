{-# LANGUAGE OverloadedStrings #-}

module Day08 where

import Day05 (replaceAt)

import qualified Data.Text as Text

data Argument  = Rect (Int, Int) | ShiftX Int Int | ShiftY Int Int deriving Show

type Coord  = (Int, Int)
type Size   = (Int, Int)
type Rect   = (Coord, Coord)
type Screen = (Size, [Bool])

emptyScreen :: Int -> Int -> Screen
emptyScreen x y =
  ((x, y), map (\_ -> False) [0..(x*y) - 1])

translateXY :: Screen -> Coord -> Int
translateXY ((width, height), _) (x, y)  = (y * width) + x

translateToXY :: Screen -> Int -> Coord
translateToXY ((width, _), _) p =
  let y = p `mod` width in
    (p - (width * y), y)

replacePoint :: Screen -> Coord -> Bool -> Screen
replacePoint screen@((w, h), bits) c v =
  ((w, h), replaceAt bits (translateXY screen c) v)

rotate :: Screen -> Argument -> Screen
rotate screen rotation =
  case rotation of
    ShiftX column dist -> screen
    ShiftY row    dist -> screen
    _                  -> error "invalid call to 'rotate'"

setRect :: Screen -> Rect -> Screen
setRect screen@((w, h), bits) r = screen

textToInt :: Text.Text -> Int
textToInt = read . Text.unpack

listToLists :: Int -> [a] -> [[a]]
listToLists len bits =
  reverse (aux [] bits)
  where
    aux acc [] = acc
    aux acc ls = aux ((take len ls):acc) (drop len ls)

lineTo :: Text.Text -> Argument
lineTo text =
  let arg:rest = Text.words text in
    case arg of
      "rect" ->
        let [x,y] = map textToInt (Text.splitOn "x" (head rest)) in
          Rect (x, y)
      "rotate" ->
        let [dir, pick, _, size] = rest
            [axis, pick'] = Text.splitOn "=" pick
            size' = textToInt size
        in
          case (dir, axis) of
            ("column", "x") -> ShiftX (textToInt pick') size'
            ("row",    "y") -> ShiftY (textToInt pick') size'
            _               -> error ("Invalid input " ++ (show text))
      _ -> error ("Invalid input " ++ (show text))

transform :: Screen -> Argument -> Screen
transform screen arg =
  case arg of
    Rect (x, y) -> setRect screen ((0, 0), (x, y))
    _           -> rotate screen arg

screenToText :: Screen -> Text.Text
screenToText ((w, h), bits) =
  Text.intercalate "\n" (map formatter enumerated)
  where
    enumerated = zip [0..] bits
    formatter (i, v) =
      let c = if v then "#" else " "
          n = if (i - 1) `mod` w == 0 then "\n" else Text.empty
      in
        Text.concat [c,n]
