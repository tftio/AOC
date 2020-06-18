module Day09 where

import Data.Maybe

scoreGroup :: [Char] -> Int
scoreGroup =
  aux 0 0 False
  where
    aux s _ _ [] = s
    aux sum level state (c:cs) =
      case (state, c) of
        (True, '!')  -> (case cs of []      -> sum
                                    (_:cs') -> aux sum level state cs')
        (True, '>')  -> aux sum level False cs
        (True, _)    -> aux sum level True cs
        (False, '<') -> aux sum level True cs
        (_, '{')     -> aux sum (level + 1) False cs
        (_, '}')     -> aux (sum + level) (level - 1) state cs
        (_, _)       -> aux sum level state cs

scoreGroup' :: [Char] -> Int
scoreGroup' =
  aux 0 False
  where
    aux ct _ [] = ct
    aux ct state (c:cs) = 
      case (state, c) of
        (True, '!') -> (case cs of []      -> ct
                                   (_:cs') -> aux ct state cs')
        (True, '>') -> aux ct False cs
        (True, _)   -> aux (ct + 1) True cs
        (_, '<')    -> aux ct True cs
        (_, _)      -> aux ct state cs

main = do
  i <- readFile "/Users/jamesblack/Projects/AOC-2017/data/Day09.txt"
  print(scoreGroup i)
  print(scoreGroup' i)
