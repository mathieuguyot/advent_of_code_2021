module Main where

import System.IO
import Data.List
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace

type Position = (Int, Int)
type Value = Int
type Cell = (Position, Value)
type CellMap = Map.Map Position Value
type UnvisitedSet = Set.Set Position

getAdjacentPoints :: Position -> CellMap -> [Position]
getAdjacentPoints (x, y) nodes = do
    let p1 = [(x + 1, y) | Map.member (x + 1, y) nodes]
    let p2 = [(x - 1, y) | Map.member (x - 1, y) nodes]
    let p3 = [(x, y + 1) | Map.member (x, y + 1) nodes]
    let p4 = [(x, y - 1) | Map.member (x, y - 1) nodes]
    p1 ++ p2 ++ p3 ++ p4

initTentativeDistance :: Cell -> [Cell] -> [Cell]
initTentativeDistance (p, v) cells = map (\(p2, _) -> if p == p2 then (p2, 0) else (p2, 1000000000)) cells

insertInOrder :: Cell -> [Cell] -> [Cell]
insertInOrder cell [] = [cell]
insertInOrder cell (d:ds) = if snd d < snd cell then d : insertInOrder cell ds else cell : d : ds

first :: Position -> [Cell] -> Cell
first pos (a:as) = if pos == fst a then a else first pos as

checkAdjacent :: Int -> Cell -> [Cell] -> [Cell]
checkAdjacent n cur distances = do
    let newDist = snd cur + n
    let oldDist = snd $ first (fst cur) distances
    let newDistances = insertInOrder (fst cur, newDist) $ filter (\(p, v) -> p /= fst cur) distances
    if newDist < oldDist then newDistances else distances

dijkstra :: Cell -> CellMap -> [Cell] -> UnvisitedSet -> IO()
dijkstra cur nodes distances unvisited = do
    let unvisitedAdjNodes = getAdjacentPoints (fst cur) nodes
    let curDistance = snd $ head distances
    let d = tail distances ++ [head distances]
    let nD1 = if length unvisitedAdjNodes >= 1 then checkAdjacent curDistance (unvisitedAdjNodes !! 0, nodes Map.! (unvisitedAdjNodes !! 0)) d else d
    let nD2 = if length unvisitedAdjNodes >= 2 then checkAdjacent curDistance (unvisitedAdjNodes !! 1, nodes Map.! (unvisitedAdjNodes !! 1)) nD1 else nD1
    let nD3 = if length unvisitedAdjNodes >= 3 then checkAdjacent curDistance (unvisitedAdjNodes !! 2, nodes Map.! (unvisitedAdjNodes !! 2)) nD2 else nD2
    let nD4 = if length unvisitedAdjNodes >= 4 then checkAdjacent curDistance (unvisitedAdjNodes !! 3, nodes Map.! (unvisitedAdjNodes !! 3)) nD3 else nD3
    let newUnvisited = Set.delete (fst cur) unvisited
    let newCurrent =  head nD4
    print $ length newUnvisited
    if fst cur == (49,49) then print curDistance else if null newUnvisited then print nD4 else dijkstra newCurrent nodes nD4 newUnvisited

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

parseNodes :: [String] -> [Cell]
parseNodes map = parse' map 0
    where parse' :: [String] -> Int -> [Cell]
          parse' [] _         = []
          parse' (x:xs) index = [((index, y), read [x !! y] :: Int) | y <- [0..(length x-1)]] ++ parse' xs (index+1)

createHugeList :: [Cell]-> [Int] -> [(Int, Int)] -> [Cell]
createHugeList _ [] [] = []
createHugeList nodes (o:ov) ((ovPosX, ovPosY): ovps) = (map (\((x,y),v) -> ((ovPosX + x, ovPosY + y), if (v + o) <= 9 then v + o else (v+o) - 9)) nodes) ++ createHugeList nodes ov ovps

main :: IO()
main = do
    handle <- openFile "day_15.txt" ReadMode
    content <- hGetContents handle
    let nodesList = parseNodes $ wordsWhen (== '\n') content
    let p2Indexes = [0, 1, 2, 3, 4, 1, 2, 3, 4, 5, 2, 3, 4, 5, 6, 3, 4, 5, 6, 7, 4, 5, 6, 7, 8]
    let p2Overflows = [(x,y) | x <- [0, 100,200,300,400], y <- [0, 100,200,300,400]]
    let hugeNodeList = createHugeList nodesList p2Indexes p2Overflows
    let hugeNodeMap = Map.fromList hugeNodeList
    let nodesMap = Map.fromList nodesList
    --print hugeNodeList
    --dijkstra (head nodesList) nodesMap (initTentativeDistance (head nodesList) nodesList) (Set.fromList $ map fst nodesList)
    dijkstra (head hugeNodeList) hugeNodeMap (initTentativeDistance (head hugeNodeList) hugeNodeList) (Set.fromList $ map fst hugeNodeList)