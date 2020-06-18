module Day05 where

solve f input =
  foldr jumps 0 (0, input)
  where
    finished (o, js) = o < 0 || o >= length js
    incrementAtOffset (o, js) = prev ++ [f e] ++ next
      where (prev, (e:next)) = splitAt o js
    jumps i sum =
      case jump i of
        Nothing -> sum
        Just (o, js) -> jumps (o, js) (sum + 1)
    jump i@(o, js) =
      if finished i then
        Nothing
      else
        Just (o', js')
      where
        o'  = o + js !! o
        js' = incrementAtOffset i

-- main = do
--   let inputFromStr = (map (\s -> read s :: Int)) . lines
--   input <- readFile "/Users/jamesblack/Projects/AOC-2017/data/Day05.txt"
--   print (solve (\e -> e + 1) (0, inputFromStr input))
--   print (solve (\e -> if e >= 3 then e - 1 else e + 1) (0, inputFromStr input))
