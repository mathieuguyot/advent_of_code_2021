module Day3 where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.Char (digitToInt)
import Data.List (foldl', transpose)

getMostCommonBit :: String -> Int
getMostCommonBit binNum
    | length (filter (=='1') binNum) >= length (filter (=='0') binNum) = 1
    | otherwise                                                        = 0

getLeastCommonBit :: String -> Int
getLeastCommonBit binNum
    | length (filter (=='1') binNum) >= length (filter (=='0') binNum) = 0
    | otherwise                                                        = 1

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

solve_part_2 :: [String] ->  Int -> (String -> Int) -> String
solve_part_2  [a] _ _ = a
solve_part_2 nums index commonBitFunc = solve_part_2 (slv nums index commonBitFunc) (index+1) commonBitFunc
    where slv :: [String] -> Int -> (String -> Int) -> [String]
          slv nums index commonBitFunc = do
            let transposed = transpose nums
            let commonBit = commonBitFunc (transposed !! index)
            filter (\num -> [num !! index] == show commonBit) nums

main :: IO()
main = do
    handle <- openFile "day_3.txt" ReadMode
    content <- hGetContents handle
    let binNums = transpose $ words content
    let gamma_rate = toDec $ foldr ((\a b -> show a ++ b) . getMostCommonBit) "" binNums
    let epsilon_rate = toDec $ foldr ((\a b -> show a ++ b) . getLeastCommonBit) "" binNums
    let oxygen_rate = toDec $ solve_part_2 (words content) 0 getMostCommonBit
    let  co2_date = toDec $ solve_part_2 (words content) 0 getLeastCommonBit
    print $ gamma_rate * epsilon_rate
    print $ oxygen_rate * co2_date
