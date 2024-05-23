module IsSquare where

type Matrix t = [[t]]

rowLengthChecker :: Matrix t -> Int -> Bool
rowLengthChecker [] _ = True
rowLengthChecker (r:rs) n = length r == n && rowLengthChecker rs n
rowLengthChecker _ _ = False

is_square :: Matrix t -> Bool
is_square [] = True
is_square (r:rs) = (length r == (length rs + 1)) && (rowLengthChecker rs (length r))
