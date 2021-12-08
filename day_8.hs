
module Day8 where

import System.IO
import Data.List

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

type Top = Bool
type Middle = Bool
type Bottom = Bool
type TopLeft = Bool
type TopRight = Bool
type BottomLeft = Bool
type BottomRight = Bool
type Segment7 = (Top, Middle, Bottom, TopLeft, TopRight, BottomLeft, BottomRight)
type Segment7Solver = (Char, Char, Char, Char, Char, Char, Char)

segment7ToNumber :: Segment7 -> Int
segment7ToNumber (True, False, True, True, True, True, True) = 0
segment7ToNumber (False, False, False, False, True, False, True) = 1
segment7ToNumber (True, True, True, False, True, True, False) = 2
segment7ToNumber (True, True, True, False, True, False, True) = 3
segment7ToNumber (False, True, False, True, True, False, True) = 4
segment7ToNumber (True, True, True, True, False, False, True) = 5
segment7ToNumber (True, True, True, True, False, True, True) = 6
segment7ToNumber (True, False, False, False, True, False, True) = 7
segment7ToNumber (True, True, True, True, True, True, True) = 8
segment7ToNumber (True, True, True, True, True, False, True) = 9
segment7ToNumber _ = -1

notIn :: Eq a => [a] -> [a] -> [a]
notIn [] _ = []
notIn (x:xs) ys
    | x `elem` ys = notIn xs ys
    | otherwise = x : notIn xs ys

segment7str :: Segment7Solver -> String
segment7str (a, b, c, d, e, f, g) = [a, b, c, d, e, f, g]

findTop :: [String] -> Segment7Solver
findTop list = (head (notIn (head (filter (\e -> length e == 3) list)) (head (filter (\e -> length e == 2) list))), ' ', ' ', ' ', ' ', ' ', ' ')

findBottom :: [String] -> Segment7Solver -> Segment7Solver
findBottom list solver = do
    let (a, _, _, _, _, _, _) = solver
    let four = head (filter (\e -> length e == 4) list)
    let sixNums = map (\e -> notIn e (segment7str solver)) (filter (\e -> length e == 6) list)
    let intersects = filter (null . notIn four) sixNums
    let bottom = head (notIn (head intersects) four)
    (a, ' ', bottom, ' ', ' ', ' ', ' ')

findTopRight :: [String] -> Segment7Solver -> Segment7Solver
findTopRight list solver = do
    let (a, _, c, _, _, _, _) = solver
    let one = head (filter (\e -> length e == 2) list)
    let sixNums = map (\e -> notIn e (segment7str solver)) (filter (\e -> length e == 6) list)
    let intersects = head (filter (not . null . notIn one) sixNums)
    let topRight = head (notIn one intersects)
    (a, ' ', c, ' ', topRight, ' ', ' ')

findBottomRight :: [String] -> Segment7Solver -> Segment7Solver
findBottomRight list solver = do
    let (a, _, c, _, e, _, _) = solver
    let one = head (filter (\e -> length e == 2) list)
    let bottomRight = head (notIn one (segment7str solver))
    (a, ' ', c, ' ', e, ' ', bottomRight)

findMiddle :: [String] -> Segment7Solver -> Segment7Solver
findMiddle list solver = do
    let (a, _, c, _, e, _, g) = solver
    let fiveNums = map (\e -> notIn e (segment7str solver)) (filter (\e -> length e == 5) list)
    let middle = head (head (filter (\e -> length e == 1) fiveNums))
    (a, middle, c, ' ', e, ' ', g)

findTopLeft :: [String] -> Segment7Solver -> Segment7Solver
findTopLeft list solver = do
    let (a, b, c, _, e, _, g) = solver
    let sixNums = map (\e -> notIn e (segment7str solver)) (filter (\e -> length e == 6) list)
    let topLeft = head (head (filter (\e -> length e == 1) sixNums))
    (a, b, c, topLeft, e, ' ', g)

findBottomLeft :: [String] -> Segment7Solver -> Segment7Solver
findBottomLeft list solver = do
    let (a, b, c, d, e, _, g) = solver
    let bottomLeft = head (head (map (\e -> notIn e (segment7str solver)) (filter (\e -> length e == 7) list)))
    (a, b, c, d, e, bottomLeft, g)

computeSegment7Solver :: [String] -> Segment7Solver
computeSegment7Solver strs = do
    let top = findTop strs
    let bottom = findBottom strs top
    let topRight = findTopRight strs bottom
    let bottomRight = findBottomRight strs topRight
    let middle = findMiddle strs bottomRight
    let topLeft = findTopLeft strs middle
    findBottomLeft strs topLeft

computeNumber :: Segment7Solver -> String -> Int
computeNumber (a, b, c, d, e, f, g) num = segment7ToNumber (a `elem` num, b `elem` num, c `elem` num, d `elem` num, e `elem` num, f `elem` num, g `elem` num)

countEasyWords :: ([String], [String]) -> Int
countEasyWords (_, []) = 0
countEasyWords (a, w:xs)
    | length w == 2 || length w == 3 || length w == 4 || length w == 7 = 1 + countEasyWords (a, xs)
    | otherwise = countEasyWords (a, xs)

solvePart2 :: ([String], [String]) -> Int
solvePart2 (strs, nums) = do
    let solver = computeSegment7Solver strs
    let n = map (computeNumber solver) nums
    head n * 1000 + n !! 1 * 100 + n !! 2 * 10 + n !! 3

main :: IO()
main = do
    handle <- openFile "day_8.txt" ReadMode
    content <- hGetContents handle
    let entries = map ((\[a, b] -> (words a, words b)) . wordsWhen (=='|')) (wordsWhen (=='\n') content)
    print $ sum $ map countEasyWords entries
    print $ sum $ map solvePart2 entries