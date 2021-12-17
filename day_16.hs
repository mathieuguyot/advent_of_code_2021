module Main where

import System.IO (openFile, hGetContents, IOMode(ReadMode))
import Data.Char (digitToInt)
import Data.List (foldl', transpose)
import Debug.Trace

hexCharToBin :: Char -> String
hexCharToBin '0' = "0000"
hexCharToBin '1' = "0001"
hexCharToBin '2' = "0010"
hexCharToBin '3' = "0011"
hexCharToBin '4' = "0100"
hexCharToBin '5' = "0101"
hexCharToBin '6' = "0110"
hexCharToBin '7' = "0111"
hexCharToBin '8' = "1000"
hexCharToBin '9' = "1001"
hexCharToBin 'A' = "1010"
hexCharToBin 'B' = "1011"
hexCharToBin 'C' = "1100"
hexCharToBin 'D' = "1101"
hexCharToBin 'E' = "1110"
hexCharToBin 'F' = "1111"

data Packet = Literal Int Int Int | Operator Int Int [Packet] deriving (Show)

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

extractN :: Int -> String -> (String, String)
extractN n str = (slice 0 (n-1) str, slice n (length str) str)

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

parseLitteralValues :: String -> String -> Int -> (String, String, Int)
parseLitteralValues str strAlreadyRead alreadyRead = do
    let (strNum, str2) = extractN 5 str
    if head strNum == '1' then parseLitteralValues str2 (strAlreadyRead ++ tail strNum) (alreadyRead+5) else (strAlreadyRead ++ tail strNum, str2, alreadyRead+5)

parseFrame :: String -> (String, Packet)
parseFrame str = do
    let (strVersion, str2) = extractN 3 str
    let (strTypeId, str3) = extractN 3 str2
    let v = toDec strVersion
    let tId = toDec strTypeId
    if toDec strTypeId == 4 then parseFrameLiteralValue str3 v tId else parseOperatorPacket str3 v tId

parseFrameLiteralValue :: String -> Int -> Int -> (String, Packet)
parseFrameLiteralValue str version typeId = do
    let (num, str1, read) = parseLitteralValues str "" 0
    (str1, Literal (toDec num) version typeId)

parseOperatorPacket :: String -> Int -> Int -> (String, Packet)
parseOperatorPacket str version typeId = do
    let (operatorType, str1) = extractN 1 str
    if head operatorType == '0' then parseOperator0Packet str1 version typeId else parseOperator1Packet str1 version typeId

parseOperator0Packet :: String -> Int -> Int -> (String, Packet)
parseOperator0Packet str version typeId = do
    let (packetSize, str2) = extractN 15 str
    let (str3, packets) = parse' str2 str2 (toDec packetSize) []
    (str3, Operator version typeId packets)
    where parse' :: String -> String -> Int -> [Packet] -> (String, [Packet])
          parse' str originalStr toDecode acc 
            | length originalStr - length str == toDecode = (str, acc)
            | otherwise = do
                let (str2, packet) = parseFrame str
                parse' str2 originalStr toDecode (acc ++ [packet])

parseOperator1Packet :: String -> Int -> Int -> (String, Packet)
parseOperator1Packet str version typeId = do
    let (packetNum, str2) = extractN 11 str
    let (str3, packets) = parse' str2 (toDec packetNum) []
    (str3, Operator version typeId packets)
    where parse' :: String -> Int -> [Packet] -> (String, [Packet])
          parse' str n acc
            | n == 0 = (str, acc)
            | otherwise = do
                let (str2, packet) = parseFrame str
                parse' str2 (n-1) (acc ++ [packet]) 

sumVersions :: Packet -> Int
sumVersions (Literal _ v _) = v
sumVersions (Operator v _ packets) = v + (sum $ map sumVersions packets)

solve :: Packet -> Int
solve (Literal v _ _) = v
solve (Operator v 0 packets) = sum $ map solve packets
solve (Operator v 1 packets) = product $ map solve packets
solve (Operator v 2 packets) = minimum $ map solve packets
solve (Operator v 3 packets) = maximum $ map solve packets
solve (Operator v 5 packets) = if solve (packets !! 0) > solve (packets !! 1) then 1 else 0 
solve (Operator v 6 packets) = if solve (packets !! 0) < solve (packets !! 1) then 1 else 0 
solve (Operator v 7 packets) = if solve (packets !! 0) == solve (packets !! 1) then 1 else 0 

main :: IO()
main = do
    let strHex = "020D74FCE27E600A78020200DC298F1070401C8EF1F21A4D6394F9F48F4C1C00E3003500C74602F0080B1720298C400B7002540095003DC00F601B98806351003D004F66011148039450025C00B2007024717AFB5FBC11A7E73AF60F660094E5793A4E811C0123CECED79104ECED791380069D2522B96A53A81286B18263F75A300526246F60094A6651429ADB3B0068937BCF31A009ADB4C289C9C66526014CB33CB81CB3649B849911803B2EB1327F3CFC60094B01CBB4B80351E66E26B2DD0530070401C82D182080803D1C627C330004320C43789C40192D002F93566A9AFE5967372B378001F525DDDCF0C010A00D440010E84D10A2D0803D1761045C9EA9D9802FE00ACF1448844E9C30078723101912594FEE9C9A548D57A5B8B04012F6002092845284D3301A8951C8C008973D30046136001B705A79BD400B9ECCFD30E3004E62BD56B004E465D911C8CBB2258B06009D802C00087C628C71C4001088C113E27C6B10064C01E86F042181002131EE26C5D20043E34C798246009E80293F9E530052A4910A7E87240195CC7C6340129A967EF9352CFDF0802059210972C977094281007664E206CD57292201349AA4943554D91C9CCBADB80232C6927DE5E92D7A10463005A4657D4597002BC9AF51A24A54B7B33A73E2CE005CBFB3B4A30052801F69DB4B08F3B6961024AD4B43E6B319AA020020F15E4B46E40282CCDBF8CA56802600084C788CB088401A8911C20ECC436C2401CED0048325CC7A7F8CAA912AC72B7024007F24B1F789C0F9EC8810090D801AB8803D11E34C3B00043E27C6989B2C52A01348E24B53531291C4FF4884C9C2C10401B8C9D2D875A0072E6FB75E92AC205CA0154CE7398FB0053DAC3F43295519C9AE080250E657410600BC9EAD9CA56001BF3CEF07A5194C013E00542462332DA4295680"
    let strBin = concatMap hexCharToBin strHex
    print $ sumVersions $ snd $ parseFrame strBin
    print $ solve $ snd $ parseFrame strBin
