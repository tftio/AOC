module Day13 where
import qualified Data.Map        as Map
import qualified Data.List       as List
import Data.Function (on)

data TrackPiece = Horizontal | Vertical | FS | BS | Intersection | Empty | Unknown Char deriving (Show)
data Direction  = North | South | East | West | Collided deriving (Show)
data NextTurn   = LeftTurn | StraightAhead | RightTurn deriving (Show)

type Point = (Integer, Integer)
type PreviousPoint = Point
type CartInfo = (Direction, NextTurn) 

type CartMap  = Map.Map Point CartInfo
type TrackMap = Map.Map Point TrackPiece

parseLines :: [String] -> (TrackMap, CartMap)
parseLines ls =
  foldr combiner (Map.empty, Map.empty) (zip [0..] ls)
  where
    combiner (y, l) (tm, cl) = (Map.union tm tm', Map.union cl cl') 
      where
      (tm', cl') = parseLine y l

    parseLine y line =
      foldr (\(x, c) (tm, cl) ->
               let pt = (x, y) in
                 case c of '-'  -> ((Map.insert pt Horizontal tm), cl)
                           '|'  -> ((Map.insert pt Vertical tm), cl)
                           '/'  -> ((Map.insert pt FS tm), cl)
                           '\\' -> ((Map.insert pt BS tm), cl)
                           '+'  -> ((Map.insert pt Intersection tm), cl)

                           '>'  -> ((Map.insert pt Horizontal tm), (Map.insert pt (East, LeftTurn) cl))
                           '<'  -> ((Map.insert pt Horizontal tm), (Map.insert pt (West, LeftTurn) cl))
                           '^'  -> ((Map.insert pt Vertical tm), (Map.insert pt (North, LeftTurn) cl))
                           'v'  -> ((Map.insert pt Vertical tm), (Map.insert pt (South, LeftTurn) cl))
                           ' '  -> ((Map.insert pt Empty tm), cl)
                           c    -> ((Map.insert pt (Unknown c) tm), cl))

      (Map.empty, Map.empty)
      (zip [0..] line)

paintMap :: TrackMap -> CartMap -> [String]
paintMap tm cm = 
  map (\l -> map snd l) (map (\y -> (filter (cmp y) pieces)) [0..maxY])
  where
    cmp y' = (\((_, y), _) -> y == y')
    (_, maxY) = mapSize tm
    pieces = Map.toAscList (Map.union cm' tm')
    cm' = Map.fromList (map (\(p, ci) -> (p, cartInfoToChar ci)) (Map.toAscList cm))
    tm' = Map.fromList (map (\(p, tp) -> (p, trackPieceToChar tp)) (Map.toAscList tm))
    
    trackPieceToChar t = case t of
      Horizontal   -> '-'
      Vertical     -> '|'
      FS            -> '/'
      BS            -> '\\'
      Intersection -> '+'
      Empty        -> ' '
      Unknown c    -> c

    cartInfoToChar (d, _) = case d of
      North -> '^'
      South -> 'v'
      East  -> '>'
      West  -> '<'
      Collided -> 'X'

testDataStr :: [[String]]
testDataStr = [["/->-\\        ", "|   |  /----\\", "| /-+--+-\\  |", "| | |  | v  |", "\\-+-/  \\-+--/", "  \\------/   "],
               ["/-->\\        ", "|   |  /----\\", "| /-+--+-\\  |", "| | |  | |  |", "\\-+-/  \\->--/", "  \\------/   "],
               ["/---v        ", "|   |  /----\\", "| /-+--+-\\  |", "| | |  | |  |", "\\-+-/  \\-+>-/", "  \\------/   "],
               ["/---\\        ", "|   v  /----\\", "| /-+--+-\\  |", "| | |  | |  |", "\\-+-/  \\-+->/", "  \\------/   "],
               ["/---\\        ", "|   |  /----\\", "| /->--+-\\  |", "| | |  | |  |", "\\-+-/  \\-+--^", "  \\------/   "],
               ["/---\\        ", "|   |  /----\\", "| /-+>-+-\\  |", "| | |  | |  ^", "\\-+-/  \\-+--/", "  \\------/   "],
               ["/---\\        ", "|   |  /----\\", "| /-+->+-\\  ^", "| | |  | |  |", "\\-+-/  \\-+--/", "  \\------/   "],
               ["/---\\        ", "|   |  /----<", "| /-+-->-\\  |", "| | |  | |  |", "\\-+-/  \\-+--/", "  \\------/   "],
               ["/---\\        ", "|   |  /---<\\", "| /-+--+>\\  |", "| | |  | |  |", "\\-+-/  \\-+--/", "  \\------/   "],
               ["/---\\        ", "|   |  /--<-\\", "| /-+--+-v  |", "| | |  | |  |", "\\-+-/  \\-+--/", "  \\------/   "],
               ["/---\\        ", "|   |  /-<--\\", "| /-+--+-\\  |", "| | |  | v  |", "\\-+-/  \\-+--/", "  \\------/   "],
               ["/---\\        ", "|   |  /<---\\", "| /-+--+-\\  |", "| | |  | |  |", "\\-+-/  \\-<--/", "  \\------/   "],
               ["/---\\        ", "|   |  v----\\", "| /-+--+-\\  |", "| | |  | |  |", "\\-+-/  \\<+--/", "  \\------/   "],
               ["/---\\        ", "|   |  /----\\", "| /-+--v-\\  |", "| | |  | |  |", "\\-+-/  ^-+--/", "  \\------/   "],
               ["/---\\        ", "|   |  /----\\", "| /-+--+-\\  |", "| | |  X |  |", "\\-+-/  \\-+--/", "  \\------/   "]]

testData = map parseLines testDataStr
  
testPainting :: [String] -> Bool
testPainting ss =
  (paintMap tm cm) == ss
  where
    (tm, cm) = parseLines ss

carts = (List.sortBy f . Map.toList . snd . head) testData
  where f = (\(p, _) (p', _) -> pointSorter p p')
      
mapSize tm =
  (maximum xs, maximum ys)
  where
    (xs, ys) = unzip (map fst (Map.toAscList tm))

pointSorter (x, y) (x', y') = compare (y, x) (y', x')

advanceCart :: (Point, CartInfo) -> TrackMap -> (Point, CartInfo)
advanceCart (pt@(x, y), ci@(d, nt)) tm =
  (nextPoint, ci')
  where
    nextPoint = (x + x', y + y')
      where (x', y') = case d of
              East -> (1, 0)
              West -> (-1, 0)
              North -> (0, -1) -- note crap (inverted maps)
              South -> (0, 1)
    nextPiece = tm Map.! nextPoint
    handleBS = case d of
      East -> South
      West -> North
      South -> East
      North -> West

    handleFS = case d of
      East -> North
      West -> South
      South -> West
      North -> East

    handleIntersection = case (d, nt) of
      (East, LeftTurn) -> (North, StraightAhead)
      (West, LeftTurn) -> (South, StraightAhead)
      (North, LeftTurn) -> (West, StraightAhead)
      (South, LeftTurn) -> (East, StraightAhead)

      (East, StraightAhead) -> (East, RightTurn)
      (West, StraightAhead) -> (West, RightTurn)
      (North, StraightAhead) -> (North, RightTurn)
      (South, StraightAhead) -> (South, RightTurn)

      (East, RightTurn) -> (South, LeftTurn)
      (West, RightTurn) -> (North, LeftTurn)
      (North, RightTurn) -> (East, LeftTurn)
      (South, RightTurn) -> (West, LeftTurn)
      
    ci' = case nextPiece of
            Horizontal   -> ci
            Vertical     -> ci
            BS           -> (handleBS, nt)
            FS           -> (handleFS, nt)
            Intersection -> handleIntersection
            _            -> error (List.intercalate ", " ["illegitmate piece state: ", show pt, show ci])

performTick :: TrackMap -> CartMap -> CartMap
performTick tm cm =
  foldr advancer Map.empty carts
  where
    carts = List.sortBy (\(p, _) (p', _) -> pointSorter p p') (Map.toAscList cm)
    advancer cart@(p, _) cartMap =
      if (Map.member newPoint otherCarts) || (Map.null otherCarts) then error (show newPoint)
      else
        Map.insert newPoint cartInfo cartMap
      where
        (newPoint, cartInfo) = advanceCart cart tm
        otherCarts = Map.fromList (filter (\(p', _) -> p /= p') carts)

findFirstCollision tm cm =
  let cm' = performTick tm cm
  in findFirstCollision tm cm'

performNTicks i tm cm =
  p i tm cm
  where
    p 0 _ r = r
    p n tm tr = p (n - 1) tm (performTick tm tr)

testStartingMap = (fst . head) testData

testTicks = map (\(t, c) -> performTick t c) testData
testMaps  = [head testDataStr] ++ (map (\c -> paintMap testStartingMap c) testTicks)
