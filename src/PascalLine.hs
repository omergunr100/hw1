module PascalLine where

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