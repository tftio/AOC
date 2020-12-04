{-# LANGUAGE OverloadedStrings #-}

module Day04 where

import Data.List.Split
import Text.Read
import Text.Regex.TDFA

testStr = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in"



-- byr (Birth Year)
-- iyr (Issue Year)
-- eyr (Expiration Year)
-- hgt (Height)
-- hcl (Hair Color)
-- ecl (Eye Color)
-- pid (Passport ID)
-- cid (Country ID)

parseInput = map words . splitOn "\n\n"

hasA vf s = (== 1) . length . filter (\p -> vf p && (take 3 p == s))

isValidPassport p =
  foldl (&&) True (map (\f -> f p) fs) 
  where
    fs = map (hasA (const True)) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

solve01 = length . filter (== True) . map isValidPassport . parseInput

solution = do
  s <- readFile "data/Day04.txt"
  print (solve01 s)

isInt s =
  case readMaybe s :: Int of Nothing -> False
                             _       -> True

validateField p =
  case k of "byr" -> checkYear 1920 2020 v
            "iyr" -> checkYear 2010 2020 v
            "eyr" -> checkYear 2020 2030 v
            "hgt" -> checkHeight v
            "hcl" -> checkColour v
            "ecl" -> checkEyeColour v
            "pid" -> checkPID v
            _     -> True
  where
    k = take 3 p
    v = drop 4 p
    v' = read v :: Int
    checkYear from to y = isInt y && length y == 4 && v' >= from && v' <= to
    checkHeight v =
      (units == "in" || units == "cm") && read ()

data PassportField = BYR Int | IYR Int | EYR Int |
                     HGT Int | HCL String | ECL String |
                     PID Int | CID Int
                   deriving Show

checkField = True

parseField p =
  c
  where
    c = case k of "byr" -> BYR vInt
                  "iyr" -> IYR vInt
                  "eyr" -> EYR vInt
                  "hgt" -> HGT vInt
                  "hcl" -> HCL v
                  "ecl" -> ECL v
                  "pid" -> PID vInt
                  "cid" -> CID vInt
                  _     -> error ("Invalid field " ++ p)
    k = take 3 p
    v = drop 4 p
    vInt = read v :: Int

data Passport = Passport { birthYear :: Int
                         , issueYear :: Int
                         , expiryYear :: Int
                         , height :: Int
                         , eyeColor :: String
                         , passportId :: Int
                         , countryId :: Maybe Int } deriving Show
                
