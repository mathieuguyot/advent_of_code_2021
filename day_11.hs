module Main where

import System.IO
import Data.List

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'


type Position = (Int, Int)
type Cell = (Position, Int, Int)

isAdjacent :: Cell -> Cell -> Bool
isAdjacent ((x, y), _, _) ((x2, y2), _, _)
    | x `elem` [x2 - 1, x2 + 1, x2] && y `elem` [y2 - 1, y2 + 1, y2] && (x2, y2) /= (x, y) = True
    | otherwise                                                                              = False

replace :: Position -> Cell -> [Cell] -> [Cell]
replace p newC = map (\(p2, v, f) -> if p2 == p then newC else (p2, v, f))

step :: [Cell] -> [Cell]
step world = do
    let newWorld = map (\(p, v, f) -> (p, v+1, f)) world
    let willFlash = filter (\(_, v, f) -> v > 9) newWorld
    fst $ step' willFlash newWorld []
    where step' :: [Cell] -> [Cell] -> [Position] -> ([Cell], [Position])
          step' [] world flashed = (world, flashed)
          step' (x:xs) world flashed = do
            let (w', f') = visit x flashed world
            step' xs w' f'

getAtPos :: Position -> [Cell] -> Cell
getAtPos p world = head $ filter (\(p2, _, _) -> p2 == p) world

visit :: Cell -> [Position] -> [Cell] -> ([Cell], [Position])
visit (p, v, f) flashed world
    | p `elem` flashed = (world, flashed)
    | v < 9 = (replace p (p, v+1, f) world, flashed)
    | otherwise = do
        let pos = map (\(p, _, _) -> p) world
        let (x, y) = p
        let (w1, f1) = if (x + 1, y) `elem` pos then visit (getAtPos (x + 1, y) world) (p : flashed) world else (world, p: flashed)
        let (w2, f2) = if (x - 1, y) `elem` pos then visit (getAtPos (x - 1, y) w1) f1 w1 else (w1, f1)
        let (w3, f3) = if (x, y + 1) `elem` pos then visit (getAtPos (x, y + 1) w2) f2 w2 else (w2, f2)
        let (w4, f4) = if (x, y - 1) `elem` pos then visit (getAtPos (x, y - 1) w3) f3 w3 else (w3, f3)
        let (w5, f5) = if (x + 1, y + 1) `elem` pos then visit (getAtPos (x + 1, y + 1) w4) f4 w4 else (w4, f4)
        let (w6, f6) = if (x - 1, y - 1) `elem` pos then visit (getAtPos (x - 1, y - 1) w5) f5 w5 else (w5, f5)
        let (w7, f7) = if (x + 1, y - 1) `elem` pos then visit (getAtPos (x + 1, y - 1) w6) f6 w6 else (w6, f6)
        let (w8, f8) = if (x - 1, y + 1) `elem` pos then visit (getAtPos (x - 1, y + 1) w7) f7 w7 else (w7, f7)
        (replace p (p, 0, f+1) w8, f8)

parseStrWorld :: [String] -> [Cell]
parseStrWorld world = parse' world 0
    where parse' :: [String] -> Int -> [Cell]
          parse' [] _ = []
          parse' (x:xs) index = [((index, y), read [x !! y]:: Int, 0) | y <- [0..(length x - 1)]] ++ parse' xs (index + 1)

stepN :: [Cell] -> Int -> [Cell]
stepN world 0 = world
stepN world n = stepN (step world) (n-1)

findAllFlash :: [Cell] -> Int -> Int
findAllFlash world n 
    | sum (map (\(_, v, _) -> v) world) == 0 = n
    | otherwise                              = findAllFlash (step world) n+1

main :: IO()
main = do
    handle <- openFile "day_11.txt" ReadMode
    content <- hGetContents handle
    let world = parseStrWorld $ wordsWhen (=='\n') content
    let steppedWorld = stepN world 100
    let flashedCount = sum $ map (\(p, v, f) -> f) steppedWorld
    print flashedCount
    print $ findAllFlash world 0
