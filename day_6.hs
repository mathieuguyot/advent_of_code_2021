{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.List (sort, group)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

updateState :: [Int] -> [Int] 
updateState [i0, i1, i2, i3, i4, i5, i6, i7, i8] = [i1, i2, i3, i4, i5, i6, i7 + i0, i8, i0]

updateStateXTimes :: [Int] -> Int -> [Int]
updateStateXTimes l index
    | index > 1  = updateStateXTimes (updateState l) (index-1)
    | otherwise  = l

main :: IO()
main = do
    handle <- openFile "day_6.txt" ReadMode
    content <- hGetContents handle
    let initial_list = map (\x -> read x :: Int) $ wordsWhen (==',') content
    let initial_state = map length ((group . sort) initial_list) ++ [0, 0, 0, 0]
    print(sum $ updateStateXTimes initial_state 80)
    print(sum $ updateStateXTimes initial_state 256)
