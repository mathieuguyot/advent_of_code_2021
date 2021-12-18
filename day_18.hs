module Main where

import Data.Char
import System.IO
import Data.List
import Debug.Trace
import Text.Html (tag)

data NestedList = Regular Int | Nested [NestedList] deriving (Show, Read, Eq)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

data UpdTag = TagLeft Int | TagRight Int | TagNone deriving (Show, Read, Eq)

isTagRight :: UpdTag -> Bool
isTagRight (TagRight _) = True
isTagRight _            = False

applyLeftTag :: NestedList -> Int -> (Int, UpdTag) -> (NestedList, Int)
applyLeftTag (Regular a) lvc (targetLvc, TagLeft addV)
    | lvc == targetLvc - 1 = (Regular (a + addV), lvc+1)
    | otherwise            = (Regular a, lvc+1)
applyLeftTag (Nested childs) lvc (targetLvc, addV) = do
    let (newChilds, newLvc) = applyLeftTag' childs [] lvc (targetLvc, addV)
    (Nested newChilds, newLvc)
    where applyLeftTag' :: [NestedList] -> [NestedList] -> Int -> (Int, UpdTag) -> ([NestedList], Int)
          applyLeftTag' [] acc lvc (targetLvc, addV) = (acc, lvc)
          applyLeftTag' (n:nl) acc lvc (targetLvc, addV) = do
              let (newN, newLvc) = applyLeftTag n lvc (targetLvc, addV)
              applyLeftTag' nl (acc ++ [newN]) newLvc (targetLvc, addV)

-- LORD GOD
applyExplosion :: NestedList -> Int -> UpdTag -> Int -> (NestedList, UpdTag, Int) -- list rightR leftR visitedCount
applyExplosion (Regular a) index TagNone lvc = (Regular a, TagNone, lvc+1)
applyExplosion (Regular a) index (TagLeft n) lvc = (Regular a, TagLeft n, lvc)
applyExplosion (Regular a) index (TagRight 0) lvc = (Regular a, TagRight 0, lvc+1)
applyExplosion (Regular a) index (TagRight n) lvc = (Regular (a + n), TagRight 0, lvc+1)
applyExplosion (Nested (Nested [Regular a, Regular b]: Nested [Regular c, Regular d]: xs)) (3) TagNone lvc = (Nested [Regular 0, Nested $ [Regular (b+c), Regular d] ++ xs], TagLeft a, lvc)
applyExplosion (Nested (Regular a: Nested [Regular b, Regular c]: xs)) (3) TagNone lvc = (Nested $ [Regular (a+b), Regular 0] ++ xs, TagRight c, lvc)
applyExplosion (Nested (Nested [Regular b, Regular c]: Regular a: xs)) (3) TagNone lvc = (Nested $ [Regular 0, Regular (a+c)] ++ xs, TagLeft b, lvc)
applyExplosion (Nested childs) index tag lvc = do
    let (newChilds, newTag, newLvc) = split' childs [] index tag lvc
    (Nested newChilds, newTag, newLvc)
    where split' :: [NestedList] -> [NestedList] -> Int -> UpdTag -> Int -> ([NestedList], UpdTag, Int)
          split' [] acc index tag lvc = (acc, tag, lvc)
          split' (n:nl) acc index tag lvc = do
              let (newN, newTag, newLvc) = applyExplosion n (index+1) tag lvc
              split' nl (acc ++ [newN]) index newTag newLvc

completeApplyExplosion :: NestedList -> (NestedList, Bool)
completeApplyExplosion l = do
    let (l2, tag, lvc) = applyExplosion l 0 TagNone 0
    if tag == TagNone then (l, False) else if isTagRight tag then (l2, True) else do
        let (l3, _) = applyLeftTag l2 0 (lvc, tag)
        (l3, True)

applySplit :: NestedList -> Bool -> (NestedList, Bool)
applySplit (Regular a) ap -- ap : Already processed
    | a >= 10 && not ap = (Nested [Regular (a `div` 2), Regular $ a `div` 2 + if even a then 0 else 1], True)
    | otherwise = (Regular a, ap)
applySplit (Nested childs) ap = do
    let (newChilds, newAp) = split' childs [] ap
    (Nested newChilds, newAp)
    where split' :: [NestedList] -> [NestedList]  -> Bool -> ([NestedList], Bool)
          split' [] acc ap = (acc, ap)
          split' (n:nl) acc ap = do
            let (newN, newAp) = applySplit n ap
            split' nl (acc ++ [newN]) newAp

computeMagnitude :: NestedList -> Int
computeMagnitude (Regular a) = a
computeMagnitude (Nested [Regular a, Regular b]) = 3*a + 2*b
computeMagnitude (Nested childs) = do
    let nums = map computeMagnitude childs
    3 * head nums + 2 * nums !! 1


strToNested :: String -> NestedList
strToNested str = read (concatMap (\e -> if e == '[' then "Nested [" else if isDigit e then "Regular " ++ [e] else [e]) str) :: NestedList

reduceAll :: [NestedList] -> NestedList
reduceAll [a,b] = reduce (Nested [a,b])
reduceAll (a:b:xs) = reduceAll (reduce (Nested [a,b]) : xs)

reduce :: NestedList -> NestedList
reduce l = do
    let (l2, wasExploded) = completeApplyExplosion l
    let (l3, wasSplit) = applySplit l False
    if wasExploded then reduce l2 else if wasSplit then reduce l3 else l

main :: IO ()
main = do
    handle <- openFile "day_18.txt" ReadMode
    content <- hGetContents handle
    let snailfishList = map strToNested (wordsWhen (== '\n') content)
    print $ computeMagnitude $ reduceAll snailfishList
    print $ maximum $ map (computeMagnitude . reduceAll) (filter (\[x, y] -> x /= y) [[x, y] | x <- snailfishList, y <- snailfishList])