module Day9 where

import System.IO
import Data.List

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

notIn :: Eq a => [a] -> [a] -> [a]
notIn [] _ = []
notIn (x:xs) ys
    | x `elem` ys = notIn xs ys
    | otherwise = x : notIn xs ys

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

type Position = (Int, Int)
type Value = Int
type Cell = (Position, Value)

isAdjacent :: Cell -> Cell -> Bool
isAdjacent ((x, y), _) ((x2, y2), _)
    | (x2 - 1 == x || x2 + 1 == x) && y == y2 = True
    | (y2 - 1 == y || y2 + 1 == y) && x == x2 = True
    | otherwise                               = False

getAdjacentPoints :: Cell -> [Cell] ->  [Cell]
getAdjacentPoints _ [] = []
getAdjacentPoints p hm = filter (\e -> isAdjacent p e) hm

isLowPoint :: Cell -> [Cell] -> Bool
isLowPoint ((x, y), v) heatMap = do
    let adjacentPoints = getAdjacentPoints ((x, y), v) heatMap
    null $ filter (\((x3, y3), v3) -> v3 <= v) adjacentPoints

parseMap :: [String] -> [Cell]
parseMap map = parse' map 0
    where parse' :: [String] -> Int -> [Cell]
          parse' [] _         = []
          parse' (x:xs) index = [((index, y), (read [x !! y] :: Int)) | y <- [0..((length x)-1)]] ++ parse' xs (index+1)

getRiskLevel :: Cell -> Int
getRiskLevel ((x, y), v) = v + 1

getAdjacentPointsHigherThan :: Cell ->  [Cell] ->  [Cell]
getAdjacentPointsHigherThan ((x, y), v) heatMap = do
    let adjacentPoints = getAdjacentPoints ((x, y), v) heatMap
    filter (\((x3, y3), v3) -> v3 > v && v3 /= 9) adjacentPoints

visitP :: Cell -> [Cell] -> [Cell] -> [Cell]
visitP point visited hm 
    | point `elem` visited = visited
    | otherwise = do
        let hap = getAdjacentPointsHigherThan point (notIn hm visited)
        let one = if length hap >= 1 then visitP (hap !! 0) (point : visited) hm else point : visited
        let two = if length hap >= 2 then visitP (hap !! 1) one hm else one
        let three = if length hap >= 3 then visitP (hap !! 2) two hm else two
        if length hap >= 4 then visitP (hap !! 3) three hm else three


main :: IO()
main = do
    handle <- openFile "day_9.txt" ReadMode
    content <- hGetContents handle
    let heatMap = parseMap $ wordsWhen (== '\n') content
    let lowPoints = filter (\e -> isLowPoint e heatMap) heatMap
    print $ sum $ map getRiskLevel $ lowPoints
    print $ product $ lastN 3 $ sort $ map length $ map (\e -> visitP e [] heatMap) lowPoints
