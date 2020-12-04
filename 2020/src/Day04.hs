{-# LANGUAGE FlexibleContexts #-}
module Day04 where

import Data.List.Split
import Text.Read
import Text.Regex.TDFA 

testStr :: String
testStr = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in"

testInvalidStr :: String
testInvalidStr = "eyr:1972 cid:100\nhcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926\n\niyr:2019\nhcl:#602927 eyr:1967 hgt:170cm\necl:grn pid:012533040 byr:1946\n\nhcl:dab227 iyr:2012\necl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277\n\nhgt:59cm ecl:zzz\neyr:2038 hcl:74454a iyr:2023\npid:3556412378 byr:2007"

testValidStr :: String
testValidStr   = "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\nhcl:#623a2f\n\neyr:2029 ecl:blu cid:129 byr:1989\niyr:2014 pid:896056539 hcl:#a97842 hgt:165cm\n\nhcl:#888785\nhgt:164cm byr:2001 iyr:2015 cid:88\npid:545766238 ecl:hzl\neyr:2022\n\niyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"

parseInput :: String -> [[String]]
parseInput = map words . splitOn "\n\n"

isValidPassport :: (String -> Bool) -> [String] -> Bool
isValidPassport vf p =
  all (\f -> f p) fs && all vf p
  where
    fs = map hasA ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
    hasA s = (== 1) . length . filter (\p -> take 3 p == s)

validateField :: String -> Bool
validateField p =
  case k of
    "byr" -> checkYear 1920 2002 v
    "iyr" -> checkYear 2010 2020 v
    "eyr" -> checkYear 2020 2030 v
    "hgt" -> checkHeight v
    "hcl" -> checkHairColour v
    "ecl" -> checkEyeColour v
    "pid" -> checkPID v
    "cid" -> True
    _     -> False
    where
      k = take 3 p
      v = drop 4 p
      isInt s = case readMaybe s :: Maybe Int of Nothing -> False
                                                 _       -> True
      checkYear f t s = isInt s && length s == 4 && s' >= f && s' <= t
        where s' = read s :: Int
      checkHeight s = correctFormat && validSize
        where
          isValidHeight = "^[0-9]+(cm|in)$" :: String
          correctFormat = s =~ isValidHeight :: Bool
          validSize = (units == "cm" && sz >= 150 && sz <= 193) || (units == "in" && sz >= 59  && sz <= 76)
  
          units = reverse . take 2 . reverse $ s
          sz    = read (reverse . drop 2 . reverse $ s) :: Int

      checkHairColour s = s =~ "^#[a-f0-9]{6}$" :: Bool
      checkEyeColour s  = s =~ "^(amb|blu|brn|gry|grn|hzl|oth)$" :: Bool
      checkPID s = length s == 9 && isInt s

solve01 :: String -> Int
solve01 = length . filter (== True) . map (isValidPassport (const True)) . parseInput

solve02 :: String -> Int
solve02 = length . filter (== True) . map (isValidPassport validateField) . parseInput

solution :: IO ()
solution = do
  s <- readFile "data/Day04.txt"
  print (solve01 s)
  print (solve02 s)

