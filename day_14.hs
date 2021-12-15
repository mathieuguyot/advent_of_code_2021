module Main where

import System.IO
import Data.List

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

type Rule = (Char, Char, Char)

thrd :: Rule -> Char
thrd (_, _, v) = v

parseRules :: [String] -> [Rule]
parseRules []     = []
parseRules (x:xs) = do
    let ws = wordsWhen (==' ') x
    (head (head ws), last (head ws), head $ last ws) : parseRules xs

getRule :: Char -> Char -> [Rule] -> String
getRule a b rules = do
    let fRules = filter (\(a2,b2,_) -> a == a2 && b == b2) rules
    [thrd (head fRules) | not $ null rules]

step :: String -> [Rule] -> String
step str rules = concat $ zipWith (\a b -> a : getRule a b rules) str (tail str) ++ [[last str]]

stepN :: String -> [Rule] -> Int -> IO()
stepN str _ 0 = do
    print 0
    let sums = sort $ map length $ group $ sort str
    print 0
    print (maximum sums - minimum sums)
stepN str rules n = do 
    let sums = sort $ map length $ group $ sort str
    print n
    print (maximum sums - minimum sums)
    stepN (step str rules) rules (n-1)

main :: IO()
main = do
    handle <- openFile "day_14.txt" ReadMode
    content <- hGetContents handle
    let cont = wordsWhen (=='\n') content
    let str = head cont
    let rules = parseRules $ tail cont
    let s10 = stepN str rules 10
    let s40 = stepN str rules 40
    s10
    s40
