{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day4 where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

gridHSum :: [Int] -> Int -> Int
gridHSum grid index = sum $ map (grid !!) [index*5..index*5+4]

gridVSum :: [Int] -> Int -> Int
gridVSum grid index = sum $ map (grid !!) [index, 5 + index, 10 + index, 15 + index, 20 + index]

verifyList :: [Int] -> Bool
verifyList grid = elem (-5) (map (gridHSum grid) [0..4] ++ map (gridVSum grid) [0..4])

applyBingoList :: [Int] -> [Int] -> [Int]
applyBingoList grid nums = map (\x -> if x `elem` nums then -1 else x) grid

groupBingoGrids :: [[Int]] -> [[Int]]
groupBingoGrids (g1:g2:g3:g4:g5:xs) = (g1 ++ g2 ++ g3 ++ g4 ++ g5) : groupBingoGrids xs
groupBingoGrids [] = []

turn :: [[Int]] -> [Int] -> [Int]
turn [] _ = []
turn (g:gs) nums = if verifyList $ applyBingoList g nums then g else turn gs nums

game :: [[Int]] -> [Int] -> [Int] -> ([Int], [Int])
game grids (n:ns) curNums = if not $ null $ turn grids (n : curNums) then (turn grids (n : curNums), n : curNums) else game grids ns (n : curNums)

turnP2 :: [[Int]] -> [Int] -> [[Int]]
turnP2 [] _ = []
turnP2 (g:gs) nums = if verifyList $ applyBingoList g nums then turnP2 gs nums else g : turnP2 gs nums

gameP2 :: [[Int]] -> [Int] -> [Int] -> ([Int], [Int])
gameP2 grids (n:ns) curNums = if null (turnP2 grids (n : curNums)) then (head (turnP2 grids curNums), n : curNums) else gameP2 grids ns (n : curNums)

computeScore :: ([Int], [Int]) -> Int
computeScore (grid, nums) = sum (filter (/= -1) $ applyBingoList grid nums) * head nums

main :: IO()
main = do
    handle <- openFile "day_4.txt" ReadMode
    content <- hGetContents handle
    let lines = wordsWhen (=='\n') content
    let bingoNumbers = map (\x -> read x :: Int) $ wordsWhen (==',') $ head lines
    let grids = groupBingoGrids $ map (map (\ x -> read x :: Int) . words) (tail lines)
    print $ computeScore $ game grids bingoNumbers []
    print $ computeScore $ gameP2 grids bingoNumbers []