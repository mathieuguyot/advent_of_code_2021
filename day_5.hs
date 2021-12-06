{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day5 where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.List (sort, group)
import Data.Text (replace, pack, unpack, filter)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

pushPos :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
pushPos (inX, inY) (0, 0) = [(inX, inY)]
pushPos (inX, inY) (x, y) = (inX + x, inY + y) : 
    pushPos (inX, inY) 
            (if x < 0 then x + 1 else if x > 0 then x - 1 else x, 
             if y < 0 then y + 1 else if y > 0 then y - 1 else y)

computePositions :: [Int] -> [(Int, Int)]
computePositions [x1,y1,x2,y2] = pushPos (x1, y1) (x2-x1, y2-y1)

computeHVPositions :: [Int] -> [(Int, Int)]
computeHVPositions [x1,y1,x2,y2]
    | x2 - x1 /= 0 && y2 - y1 /= 0 = []
    | otherwise = pushPos (x1, y1) (x2-x1, y2-y1)

main :: IO()
main = do
    handle <- openFile "day_5.txt" ReadMode
    content <- hGetContents handle
    let vents = map
          (map (\ x -> read x :: Int) . wordsWhen (== ','))
          (wordsWhen (== '\n') $ unpack $ replace " -> " "," $ pack content)
    let hvPos = concatMap computeHVPositions vents
    let pos = concatMap computePositions vents
    print $ length $ Prelude.filter (>1) $ map length $ group $ sort hvPos
    print $ length $ Prelude.filter (>1) $ map length $ group $ sort pos
