-- 1)
reverseExceptFirst :: String -> String
reverseExceptFirst "" = ""
reverseExceptFirst (x:xs) = [x] ++ reverse xs

shiftString :: String -> String
shiftString "" = ""
shiftString str = reverseExceptFirst (reverse str)

-- 2)
isShiftedHelper :: String -> String -> Int -> Bool
isShiftedHelper "" "" _ = True
isShiftedHelper "" _ _ = False
isShiftedHelper _ "" _ = False
isShiftedHelper _ _ 0 = False
isShiftedHelper s1 s2 n = s1 == s2 || isShiftedHelper s1 (shiftString s2) (n - 1)

isShifted :: String -> String -> Bool
isShifted s1 s2 = length s1 == length s2 && isShiftedHelper s1 s2 (length s1)

-- 3)
stupidListOp :: [Int] -> [Int]
stupidListOp [] = []
stupidListOp (x:xs) = (replicate x x) ++ stupidListOp xs

-- 4)
pascal :: Int -> Int -> Int
pascal r 0 = 1
pascal r c = if r == c then 1 else ((pascal (r - 1) (c - 1)) + (pascal (r - 1) c))

pascalList :: [(Int, Int)] -> [Int]
pascalList [] = []
pascalList ((r, c):xs) = [pascal r c] ++ pascalList xs

zipLists :: [Int] -> [Int] -> [(Int, Int)]
zipLists [] _ = []
zipLists _ [] = []
zipLists (x:xs) (y:ys) = [(x, y)] ++ zipLists xs ys

pascalLine :: Int -> [Int]
pascalLine r = pascalList (zipLists (replicate (r + 1) r) [0..r])

-- 5)
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

-- 6)
rowLengthChecker :: Matrix t -> Int -> Bool
rowLengthChecker [] _ = True
rowLengthChecker (r:rs) n = length r == n && rowLengthChecker rs n
rowLengthChecker _ _ = False

is_square :: Matrix t -> Bool
is_square [] = True
is_square (r:rs) = (length r == (length rs + 1)) && (rowLengthChecker rs (length r))