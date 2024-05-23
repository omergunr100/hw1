module StupidListOp where

stupidListOp :: [Int] -> [Int]
stupidListOp [] = []
stupidListOp (x:xs) = (replicate x x) ++ stupidListOp xs
