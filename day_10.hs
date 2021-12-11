module Day1 where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.List

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

checkInvalidLine :: String -> String -> Int
checkInvalidLine [] _          = 0
checkInvalidLine (c:cd) sck
    | c `elem` ['(','<','{','['] = checkInvalidLine cd (c : sck)
    | c == ')' && (head sck) /= '(' = 3
    | c == ']' && (head sck) /= '[' = 57
    | c == '}' && (head sck) /= '{' = 1197
    | c == '>' && (head sck) /= '<' = 25137
    | otherwise = checkInvalidLine cd (tail sck)

comres :: String -> Int -> Int
comres [] n = n
comres (c:cd) n
    | c == '(' = comres cd ((n * 5) + 1)
    | c == '[' = comres cd ((n * 5) + 2)
    | c == '{' = comres cd ((n * 5) + 3)
    | c == '<' = comres cd ((n * 5) + 4)

part2 :: String -> String -> Int
part2 [][]          = 0
part2 [] sck = comres sck 0
part2 (c:cd) sck
    | c `elem` ['(','<','{','['] = part2 cd (c : sck)
    | c == ')' && (head sck) /= '(' = 0
    | c == ']' && (head sck) /= '[' = 0
    | c == '}' && (head sck) /= '{' = 0
    | c == '>' && (head sck) /= '<' = 0
    | otherwise = part2 cd (tail sck)

main :: IO()
main = do
    handle <- openFile "day_10.txt" ReadMode
    content <- hGetContents handle
    let lines = wordsWhen (== '\n') content
    let invalidSums =  map (\e -> checkInvalidLine e "") lines
    let part2Sums = sort $ filter (/= 0) $ map (\e -> part2 e "") lines
    print invalidSums
    print $ sum $ invalidSums
    print $ part2Sums
    print $ part2Sums !! ((length part2Sums) `div` 2)
