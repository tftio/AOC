module Day06 where

import qualified Data.Map as M
import qualified Data.Set as S

solveOne input =
  s input 1 (S.fromList [input])
  where
    s bank sum seen =
      if S.member bank' seen then
        sum + 1
      else
        s bank' (sum + 1) (S.insert bank seen)
      where
        bank' = redistribute bank

redistribute bank =
  map (\(_, v) -> v) (M.toList (foldr update nums [idx + 1..idx + max]))
  where
    (idx, max) = foldr (\a@(_, v) b@(_, v') -> if v' > v then b else a) (0, head bank) (zip [0..] bank)
    bank' = p ++ [0] ++ a where (p, (_:a)) = splitAt idx bank
    l  = length bank
    update i m =
      M.insert (i `mod` l) ((M.findWithDefault 0 (i `mod` l) m) + 1) m
    nums = M.insert idx 0 (M.fromList (zip [0..] bank'))


solveTwo input =
  s input 1 False (S.fromList [input])
  where
    s bank sum seenOnce seen =
      if seenOnce then
        if S.member bank' seen then
          sum
        else
          s bank' (sum + 1) seenOnce (S.insert bank seen)
      else
        if S.member bank' seen then
          s bank' 1 True (S.fromList [bank'])
        else
          s bank' (sum + 1) False (S.insert bank seen)
     where
       bank' = redistribute bank

