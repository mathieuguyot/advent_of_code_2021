module Main where

import System.IO
import Data.List

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

type Rule = (Char, Char, Char)
type Occurence = ((Char, Char), Int)

stepO :: Occurence -> [Rule] -> [Occurence]
stepO ((a, b), n) rules = do
    let filteredRules = filter (\(a2, b2, _) -> a2 == a && b2 == b) rules
    if null filteredRules then [((a, b), n)] else do
        let (_, _, c) = head filteredRules
        [((a, c), n), ((c, b), n)]

step2 :: [Occurence] -> [Rule] -> [Occurence]
step2 os rules = groupOccurences $ concatMap (`stepO` rules) os

step2N :: [Occurence] -> [Rule] -> Int -> [Occurence]
step2N os _ 0 = os
step2N os r n = step2N (step2 os r) r (n-1)

groupOccurences :: [Occurence] -> [Occurence]
groupOccurences [] = []
groupOccurences (((a, b), n):os) = do
    let os' = filter (\((a2, b2), _) -> a /= a2 || b /= b2) os
    let same = filter (\((a2, b2), _) -> a == a2 && b == b2) os
    let count = sum (map snd same) + n
    ((a,b), count) : groupOccurences os'

groupLetters :: [Occurence] -> [(Char, Int)]
groupLetters [] = []
groupLetters (((a, _), n):os) = do
    let os' = filter (\((a2, _), _) -> a /= a2) os
    let same = filter (\((a2, _), _) -> a == a2) os
    let count = sum (map snd same) + n
    (a, count) : groupLetters os'

parseRules :: [String] -> [Rule]
parseRules []     = []
parseRules (x:xs) = do
    let ws = wordsWhen (==' ') x
    (head (head ws), last (head ws), head $ last ws) : parseRules xs

main :: IO()
main = do
    handle <- openFile "day_14.txt" ReadMode
    content <- hGetContents handle
    let cont = wordsWhen (=='\n') content
    let str = head cont
    let rules = parseRules $ tail cont
    let occurences = map (\e -> (head e, length e)) (group $ sort $ zip str $ tail str) ++ [((last str, '#'), 1)]
    let s10 = sort $ map snd $ groupLetters $ step2N occurences rules 10
    print $ last s10 - head s10
    let s40 = sort $ map snd $ groupLetters $ step2N occurences rules 40
    print $ last s40 - head s40
