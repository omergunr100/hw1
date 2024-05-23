module IsShifted where
import ShiftString

isShiftedHelper :: String -> String -> Int -> Bool
isShiftedHelper "" "" _ = True
isShiftedHelper "" _ _ = False
isShiftedHelper _ "" _ = False
isShiftedHelper _ _ 0 = False
isShiftedHelper s1 s2 n = s1 == s2 || isShiftedHelper s1 (shiftString s2) (n - 1)

isShifted :: String -> String -> Bool
isShifted s1 s2 = length s1 == length s2 && isShiftedHelper s1 s2 (length s1)