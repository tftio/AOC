-- |

module Day12 where
import           Data.Char
import           Data.List
import           Data.List.Split
import qualified Data.Map        as M

testInput = "initial state: #..#.#..##......###...###\n\n...## => #\n..#.. => #\n.#... => #\n.#.#. => #\n.#.## => #\n.##.. => #\n.#### => #\n#.#.# => #\n#.### => #\n##.#. => #\n##.## => #\n###.. => #\n###.# => #\n####. => #\n"

day01Input = "initial state: #.......##.###.#.#..##..##..#.#.###..###..##.#.#..##....#####..##.#.....########....#....##.#..##...\n..... => .\n#.... => .\n..### => .\n##..# => #\n.###. => #\n...## => .\n#.#.. => .\n..##. => .\n##.#. => #\n..#.. => .\n.#... => #\n##.## => .\n....# => .\n.#.#. => .\n#..#. => #\n#.### => .\n.##.# => #\n.#### => .\n.#..# => .\n####. => #\n#...# => #\n.#.## => #\n#..## => .\n..#.# => #\n#.##. => .\n###.. => .\n##### => #\n###.# => #\n...#. => #\n#.#.# => #\n.##.. => .\n##... => #"

type Pot   = (Int, Bool)
type State = [Pot]
type Rule  = ([Bool], Bool)

type Store = M.Map State

parseInput :: [Char] -> (State, [Rule])
parseInput input = (zip [0..] (strToBools init), (toRules rs))
  where
    [init]:rs = filter (\l -> length l > 0) (map (split (dropInitBlank . dropFinalBlank . condense . dropDelims $ oneOf "initialstate:=> ")) (lines input))
    strToBools = map (\c -> c == '#')
    toRule (rule:[result]:[]) = (rule, result)
    toRules = map (toRule . map strToBools)

visualizeStates ss = do
  putStrLn (intercalate "\n" ([header] ++ body)) where
    body = map (\(s, i) -> (padStr (show i)) ++ (visualizeState s)) (zip ss [0..])
    header = map (\i -> if (i `mod` 10 == 0) then intToDigit (i `div` 10) else '-') [-4..max]
    max = maximum (map length ss)
    padStr s | length s == 4 = s
    padStr s = take 4 $ s ++ repeat ' '
    visualizeState s = map (\(_, b) -> if b then '#' else '.') s


chunkState :: State -> [State]
chunkState s = map (\i -> fill 5 (slice (i - 2) (i + 3) s')) [2..(length s') + 1]
  where
    s' = padState s
    fill n l | length l == n = l
    fill n l = take n $ l ++ (zip [max..] (repeat False)) where max = 1 + fst (last l)
    slice b e = take (e - b) . drop b
    padState s = frontPad ++ s
      where
        from f t= zip [f..t] (repeat False)
        frontPad = case s of
          (i, True):_                      -> from (i - 4) (i - 1)
          (i, _):(_, True):_               -> from (i - 3) (i - 1)
          (i, _):(_, _):(_, True):_        -> from (i - 2) (i - 1)
          (i, _):(_, _):(_, _):(_, True):_ -> from (i - 1) (i - 1)
          _                                -> []

simulate :: State -> [Rule] -> State
simulate s r =
  map chunker (chunkState s)
  where
    chunker c | hasPlant (map snd c) r = (fst (c !! 2), True)
    chunker c = (fst (c !! 2), False)
    hasPlant chunk ((rule, result):_) | chunk == rule = result
    hasPlant chunk (_:rules)          = hasPlant chunk rules
    hasPlant _ []                     = False



(s, r) = parseInput testInput
go prev@state rules num = aux prev state rules num where
  aux _ s _ 0 = s
  aux p s r n = if (simulate s r) == s then s else aux s (simulate s r) r (n - 1)

-- cycleLength :: [Rule] -> State -> Int
cycleLength rules state max =
  aux state M.empty 1
  where
    aux state store i | M.member state store = (M.lookup state store, i)
    aux state store i | i < max =
      aux (simulate state rules) (M.insert state i store) (i + 1)
    aux _ _ i | i == max = (Nothing, i)

solve02 i = cycleLength r s i where
  (s, r) = parseInput day01Input
