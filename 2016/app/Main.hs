{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Day08
import Data.List

main :: IO ()
main = do
  a <- readFile "data/08.txt"
  putStrLn "Day 08"
