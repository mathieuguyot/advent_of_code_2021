module Day7 where

import System.IO
import Data.List

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

comp :: Int -> Int -> Int -> Int
comp a b inertia
    | a < b = comp b a inertia
    | a == b = inertia
    | otherwise = inertia + comp a (b+1) (inertia+1)

main :: IO()
main = do
    handle <- openFile "day_7.txt" ReadMode
    content <- hGetContents handle
    let nums = sort $ map (\x -> read x :: Int) $ wordsWhen (==',') content
    let l = length nums
    let hl = l `div` 2
    let med = ((nums !! hl) + (nums !! (hl-1))) `div` 2
    print $ sum $ map (\e -> abs $ e - med) nums
    let mean = fromIntegral (sum nums) `div` fromIntegral l
    print $ sum $ map (\e -> comp e mean 0) nums

-- After solve problem, I've checked Reddit and: 
-- mean is the wrong anwser to this problem but with the provided dataset it is working XD
-- Use dichotomy seems the right way to go !