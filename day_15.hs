module Main where

import System.IO
import Data.List

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
getAdjacentPoints p hm = filter (isAdjacent p) hm

initTentativeDistance :: Cell -> [Cell] -> [Cell]
initTentativeDistance (p, v) = map (\(p2, _) -> if p == p2 then (p2, 0) else (p2, 1000000000))

checkAdjacent :: Int -> Cell -> [Cell] -> [Cell]
checkAdjacent n cur distances = do
    let newDist = snd cur + n
    let oldDist = snd $ head $ filter (\(p, _) -> p == fst cur) distances
    let newDistances = map (\(p, v) -> if p == fst cur then (p, newDist) else (p, v)) distances
    if newDist < oldDist then newDistances else distances

getNextCurr :: [Position] -> [Cell] -> Position -> [Cell] -> Cell
getNextCurr [] _ cur nodes = head $ filter (\(p, _) -> p == cur) nodes
getNextCurr (u: us) distances cur nodes = do
    let curV =  head $ filter (\(p, _) -> p == cur) distances
    let candidateV =  head $ filter (\(p, _) -> p == u) distances
    if snd candidateV < snd curV then (getNextCurr us distances (fst candidateV) nodes) else (getNextCurr us distances (fst curV) nodes)

dijkstra :: Cell -> [Cell] -> [Cell] -> [Position] -> IO()
dijkstra cur nodes distances unvisited = do
    let unvisitedAdjNodes = filter (\(p, _) -> p `elem` unvisited) $ getAdjacentPoints cur nodes
    let curDistance = snd $ head $ filter (\(p, _) -> p == fst cur) distances
    let nD1 = if length unvisitedAdjNodes >= 1 then checkAdjacent curDistance (unvisitedAdjNodes !! 0) distances else distances
    let nD2 = if length unvisitedAdjNodes >= 2 then checkAdjacent curDistance (unvisitedAdjNodes !! 1) nD1 else nD1
    let nD3 = if length unvisitedAdjNodes >= 3 then checkAdjacent curDistance (unvisitedAdjNodes !! 2) nD2 else nD2
    let nD4 = if length unvisitedAdjNodes >= 4 then checkAdjacent curDistance (unvisitedAdjNodes !! 3) nD3 else nD3
    let newUnvisited = filter (\p -> p /= fst cur) unvisited
    let newCurrent = getNextCurr (tail newUnvisited) nD4 (head newUnvisited) nodes
    if null newUnvisited then print nD4 else do 
        print $ length newUnvisited
        dijkstra newCurrent nodes nD4 newUnvisited

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

parseMap :: [String] -> [Cell]
parseMap map = parse' map 0
    where parse' :: [String] -> Int -> [Cell]
          parse' [] _         = []
          parse' (x:xs) index = [((index, y), read [x !! y] :: Int) | y <- [0..(length x-1)]] ++ parse' xs (index+1)

main :: IO()
main = do
    handle <- openFile "day_15.txt" ReadMode
    content <- hGetContents handle
    let nodes = parseMap $ wordsWhen (== '\n') content
    dijkstra (head nodes) nodes (initTentativeDistance (head nodes) nodes) (map fst nodes)