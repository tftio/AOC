{-# LANGUAGE OverloadedStrings #-}

module Day05 where

import Data.Char (isDigit, digitToInt)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B (fromStrict)
import Data.Text.Encoding as E (decodeUtf8, encodeUtf8)
import Data.Digest.Pure.MD5 (md5)
import Data.Int

import qualified Debug.Trace as R (trace)

type Checksum = T.Text

getChecksum :: T.Text -> Checksum
getChecksum = T.pack . show . md5 . B.fromStrict . E.encodeUtf8

getDerivedPassphrase :: T.Text -> Int -> T.Text
getDerivedPassphrase base idx = T.append base (T.pack (show idx))

okChecksum :: Checksum -> Bool
okChecksum = (== "00000") . (T.take 5)

findPassword :: Int -> T.Text -> T.Text
findPassword len seed =
  findPassword' T.empty 0
  where
    findPassword' :: T.Text -> Int -> T.Text
    findPassword' acc idx
      | T.length acc == len = acc
      | otherwise =
        let cs = (getChecksum (getDerivedPassphrase seed idx)) in
          case okChecksum cs of
            True  -> findPassword' (T.snoc acc (T.index cs 5)) (idx + 1)
            False -> findPassword' acc (idx + 1)

replaceAt :: [a] -> Int -> a -> [a]
replaceAt orig idx new =
  let (before, _:after) = splitAt idx orig in
    before ++ [new] ++ after

positionOk :: Char -> [Char] -> Bool
positionOk idx positions
  | isDigit idx =
      let pos = digitToInt idx in
        pos >= 0 && pos < 8 && (positions !! pos) == '_'
  | otherwise = False

findPassword2 :: Int -> T.Text -> [Char]
findPassword2 len seed =
  findPassword2' ['_', '_', '_', '_', '_', '_', '_', '_'] 0
  where
    findPassword2' :: [Char] -> Int -> [Char]
    findPassword2' positions idx
      | length (filter (not . (== '_')) positions) == len = positions
      | otherwise =
        let cs = (getChecksum (getDerivedPassphrase seed idx))
            pos = T.index cs 5
            char = T.index cs 6
            posOk = positionOk pos positions
            csOk = okChecksum cs
        in
          if csOk && posOk then
            findPassword2' (replaceAt positions (digitToInt pos) char) (idx + 1)
          else
            findPassword2' positions (idx + 1)
