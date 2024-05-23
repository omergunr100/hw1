module ShiftString where

reverseExceptFirst :: String -> String
reverseExceptFirst "" = ""
reverseExceptFirst (x:xs) = [x] ++ reverse xs

shiftString :: String -> String
shiftString "" = ""
shiftString str = reverseExceptFirst (reverse str)