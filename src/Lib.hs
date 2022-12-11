module Lib
    ( 
    batchSolve
    ) where


batchSolve :: [Int] -> Int -> Int -> Int -> [Int]
batchSolve candidates base mod_ res = [x | x <- candidates, singleSolve x base mod_ res]
    
singleSolve :: Int -> Int -> Int -> Int -> Bool
singleSolve candidate base mod_ res = base ^ candidate `mod` mod_ == res
