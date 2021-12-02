module Day1 where

import System.IO
import Data.List (transpose)

solvePart1 :: [Int] -> Int
solvePart1 nums = sum $ map fromEnum $ zipWith (<) nums (tail nums)

solvePart2 :: [Int] -> Int
solvePart2 nums = solvePart1 $ map sum $ transpose [nums, tail nums, (tail . tail) nums]

main :: IO()
main = do 
    handle <- openFile "day_1.txt" ReadMode 
    content <- hGetContents handle
    let numbers = map (\s -> read s :: Int) $ words content
    print(solvePart1 numbers)
    print(solvePart2 numbers)
