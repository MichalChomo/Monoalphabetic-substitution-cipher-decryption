-- FLP functional project subs-cipher
-- xchomo01, Michal Chomo
module FreqMap where

import Data.Char
import qualified Data.Map as Map

-- Type representing the whole frequency database
type FreqMap = Map.Map String Float

-- Perform a frequency analysis on the cyphertext
freqAnalysis :: String -> FreqMap
freqAnalysis cyphertext = Map.map ((100 *) . (/ sum (Map.elems m))) m
    where m = getOccurences cyphertext

-- Count occurences of characters in the cyphertext and store them in map
getOccurences :: String -> FreqMap
getOccurences [] = Map.fromList []
getOccurences (x:xs)
    | Map.member [x] (getOccurences xs) = Map.adjust (1 +) [x] (getOccurences xs)
    | otherwise = Map.insert [x] 1.0 (getOccurences (filter isAlpha xs))

getFreqMap :: String -> FreqMap
getFreqMap dbString = Map.fromList $ map getPair (filter isLetter' (lines dbString))

getFreqMapDigram :: String -> FreqMap
getFreqMapDigram dbString = Map.fromList $ map getPair (filter isDigram (lines dbString))

getFreqMapTrigram :: String -> FreqMap
getFreqMapTrigram dbString = Map.fromList $ map getPair (filter isTrigram (lines dbString))

getPair :: String -> (String, Float)
getPair line = (head (words line), read (last (words line)) :: Float)

isLetter' :: String -> Bool
isLetter' line
    | length (head (words line))  == 1 = True
    | otherwise = False

isDigram :: String -> Bool
isDigram line
    | length (head (words line))  == 2 = True
    | otherwise = False

isTrigram :: String -> Bool
isTrigram line
    | length (head (words line))  == 3 = True
    | otherwise = False
