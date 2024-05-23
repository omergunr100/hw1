module IdValidation where

padZeros :: [Integer] -> [Integer]
padZeros x = if (length x) < 9 then (padZeros (0:x)) else x

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | n < 10 = [n]
  | otherwise = padZeros ((toDigits (div n 10)) ++ [mod n 10])

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (xe:xo:xs) = xe : (xo * 2) : (doubleEveryOther xs)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sum (toDigits x)) + (sumDigits xs)

validate :: Integer -> Bool
validate n = (mod (sumDigits (doubleEveryOther (toDigits n))) 10) == 0