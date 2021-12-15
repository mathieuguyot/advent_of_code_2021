module Main where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.List
import Data.Char

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

getTargets :: String -> [(String, String)] -> [String]
getTargets str nodes = map snd (filter (\(a, _) -> str == a) nodes) ++ map fst (filter (\(_, a) -> str == a) nodes)

callVisit :: [String] -> [String] -> [(String, String)] -> [[String]]
callVisit [] _ _ = []
callVisit ("end":xs) path graph = (path ++ ["end"]) : callVisit xs path graph
callVisit (x:xs) path graph = visit x path graph ++ callVisit xs path graph

visit :: String -> [String] -> [(String, String)] -> [[String]]
visit current path graph = do
    let newPath = path ++ [current]
    let targets = getTargets current graph
    let targetsUpper = filter (isUpper . head) targets
    let targetsLower = filter (`notElem` newPath) $ filter (isLower . head) targets
    let finalTargets = targetsUpper ++ targetsLower
    callVisit finalTargets newPath graph

callVisit2 :: [String] -> [String] -> [(String, String)] -> [[String]]
callVisit2 [] _ _ = []
callVisit2 ("end":xs) path graph = (path ++ ["end"]) : callVisit2 xs path graph
callVisit2 (x:xs) path graph = visit2 x path graph ++ callVisit2 xs path graph

checkPath :: [String] -> Bool
checkPath path = do
    let startOnce = length (filter (=="start") path) == 1
    let lengths = map length $group $ sort $ filter (isLower . head) path
    length (filter (==2) lengths) <= 1 && not (any (>2) lengths) && startOnce

visit2 :: String -> [String] -> [(String, String)] -> [[String]]
visit2 current path graph = do
    let newPath = path ++ [current]
    let targets = getTargets current graph
    let targetsUpper = filter (isUpper . head) targets
    let targetsLower = filter (isLower . head) targets
    let finalTargets = targetsUpper ++ targetsLower
    if checkPath newPath then callVisit2 finalTargets newPath graph else []

main :: IO()
main = do
    handle <- openFile "day_12.txt" ReadMode
    content <- hGetContents handle
    let graph = map (\e -> (head $ wordsWhen (=='-') e, last $ wordsWhen (=='-') e)) $ wordsWhen (=='\n') content
    print $ length $ visit "start" [] graph
    print $ length $ visit2 "start" [] graph
