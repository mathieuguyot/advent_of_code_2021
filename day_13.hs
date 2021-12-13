module Main where

import System.IO
import Data.List

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

type Position = (Int, Int)
data Fold = AlongX Int | AlongY Int deriving (Eq, Show)

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

foldAtX :: Int -> [Position] -> [Position]
foldAtX n pos = do
    let toMove = filter (\(x, y) -> x > n) pos
    let newPos = filter (\(x, y) -> x < n) pos ++ map (\(x, y) -> (abs (x - n*2) ,y)) toMove
    rmdups newPos

foldAtY :: Int -> [Position] -> [Position]
foldAtY n pos = do
    let toMove = filter (\(x, y) -> y > n) pos
    let newPos = filter (\(x, y) -> y < n) pos ++ map (\(x, y) -> (x ,abs (y - n*2))) toMove
    rmdups newPos

parseFile :: [String] -> ([Position], [Fold]) -> ([Position], [Fold])
parseFile [] (ps, fs) = (ps, fs)
parseFile (x:xs) (ps, fs)
    | head x == 'f' && x !! 11 == 'x' = parseFile xs (ps, fs ++ [AlongX (read (wordsWhen (=='=') x !! 1) :: Int)])
    | head x == 'f' && x !! 11 == 'y' = parseFile xs (ps, fs ++[AlongY (read (wordsWhen (=='=') x !! 1) :: Int)])
    | otherwise                       = do
        let nums = wordsWhen (==',') x
        let pos = (read (head nums) :: Int, read (nums !! 1) :: Int)
        parseFile xs (pos : ps, fs)

run :: ([Position], [Fold]) -> [Position]
run (p, []) = p
run (p, f) = run $ step (p, f)

step :: ([Position], [Fold]) -> ([Position], [Fold])
step (p, []) = (p, [])
step (p, AlongX n:xs) = (foldAtX n p, xs)
step (p, AlongY n:xs) = (foldAtY n p, xs)

mySplit :: Int -> [a] -> [[a]]
mySplit n [] = []
mySplit n xs = take n xs : mySplit n (drop n xs)

myPrint :: [String] -> IO()
myPrint [] = print ""
myPrint (x:xs) = do
    print x
    myPrint xs

printPos :: [Position] -> IO()
printPos pos = do
    let maxX = maximum (map fst pos)
    let maxY = maximum (map snd pos)
    myPrint $ mySplit (maxX+1) [w | y <- [0..maxY], x <- [0..maxX], let w = if (x, y) `elem` pos then '#' else '.']

main :: IO()
main = do
    handle <- openFile "day_13.txt" ReadMode
    content <- hGetContents handle
    let sheet = parseFile (wordsWhen (=='\n') content) ([], [])
    let finalSheet = run sheet
    let maxX = maximum (map fst finalSheet)
    let maxY = maximum (map snd finalSheet)
    print $ length $ fst $ step sheet
    printPos finalSheet
