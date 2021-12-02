module Day2 where

import System.IO
import Data.List (transpose)

data Command = Forward Int | Down Int | Up Int | Unknown deriving(Show)

solvePart1 :: [Command] -> Int
solvePart1 commands = solve commands (0, 0)
    where
        solve :: [Command] -> (Int, Int) -> Int
        solve (Forward n:xs) (hp, d) = solve xs (hp + n, d)
        solve (Down n:xs) (hp, d)    = solve xs (hp, d+n)
        solve (Up n:xs) (hp, d)      = solve xs (hp, d-n)
        solve (Unknown:xs) (hp, d)   = solve xs (hp, d)
        solve [] (hp, d)             = hp * d

solvePart2 :: [Command] -> Int
solvePart2 commands = solve commands (0, 0, 0)
    where
        solve :: [Command] -> (Int, Int, Int) -> Int
        solve (Forward n:xs) (hp, d, aim) = solve xs (hp + n, d + (aim * n), aim)
        solve (Down n:xs) (hp, d, aim)    = solve xs (hp, d, aim+n)
        solve (Up n:xs) (hp, d, aim)      = solve xs (hp, d, aim-n)
        solve (Unknown:xs) (hp, d, aim)   = solve xs (hp, d, aim)
        solve [] (hp, d, _)               = hp * d

parseFile :: String -> [Command]
parseFile str = parseWords $ words str
    where
        parseWords :: [String] -> [Command]
        parseWords []       = []
        parseWords (c:n:xs) = parseCommand c n : parseWords xs
        parseWords _        = []

parseCommand :: String -> String -> Command
parseCommand command num 
    | command == "forward" = Forward (read num :: Int)
    | command == "down"    = Down (read num :: Int)
    | command == "up"      = Up (read num :: Int)
    | otherwise            = Unknown

main :: IO()
main = do 
    handle <- openFile "day_2.txt" ReadMode 
    content <- hGetContents handle
    let commands = parseFile content
    print(solvePart1 commands)
    print(solvePart2 commands)
