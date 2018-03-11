-- FLP functional project subs-cipher
-- xchomo01, Michal Chomo
module FreqMap where

import Data.List
import Data.Ord

-- Type representing the whole frequency database
type FreqMap = [(String, Float)]

-- Compute frequency from count of occurences of letters/digrams/trigrams
freqAnalysis :: FreqMap -> FreqMap
freqAnalysis occurenceMap = sortByValue $
    map (\x -> (fst x, 100 * snd x / sumValues occurenceMap)) occurenceMap

-- Perform frequency analysis on letters in string
freqAnalysisLetter :: String -> FreqMap
freqAnalysisLetter ciphertext = freqAnalysis $ getOccurences ciphertext

-- Perform frequency analysis on digrams in string
freqAnalysisDigram :: String -> FreqMap
freqAnalysisDigram ciphertext = freqAnalysis $ getOccurencesGram 2 ciphertext

-- Perform frequency analysis on trigrams in string
freqAnalysisTrigram :: String -> FreqMap
freqAnalysisTrigram ciphertext = freqAnalysis $ getOccurencesGram 3 ciphertext

-- Count occurences of characters in text and store them in map
getOccurences :: String -> FreqMap
getOccurences [] = []
getOccurences text = map (\x -> ([head x], fromIntegral $ length x)) $
    group . sort $ text

-- Count occurences of n-grams in text and store them in FreqMap
getOccurencesGram :: Int -> String -> FreqMap
getOccurencesGram _ [] = []
getOccurencesGram n text = go $ chunksOf n text ++ chunksOf n (tail text)
    where getGramCount gram = length . filter (== gram)
          removeGram gram = filter (/= gram)
          go [] = []
          go l = (head l, fromIntegral (getGramCount (head l) l)) :
            go (removeGram (head l) l)

-- Parse frequency map from string and store it to FreqMap
-- len determines if frequency map is of letters/digrams/trigrams
parseFreqMap :: Int -> String -> FreqMap
parseFreqMap len dbString = map getPair $ filter (isLen len) $ lines dbString

-- Parse frequency map of letters
parseFreqMapLetter :: String -> FreqMap
parseFreqMapLetter = parseFreqMap 1

-- Parse frequency map of digrams
parseFreqMapDigram :: String -> FreqMap
parseFreqMapDigram = parseFreqMap 2

-- Parse frequency map of trigrams
parseFreqMapTrigram :: String -> FreqMap
parseFreqMapTrigram = parseFreqMap 3

-- Get pair of letter/digram/trigram and its frequency
getPair :: String -> (String, Float)
getPair line = (head (words line), read (last (words line)) :: Float)

-- Check if line from frequency db contains string of len characters
isLen :: Int -> String -> Bool
isLen len line
    | length (head $ words line) == len = True
    | otherwise = False

-- Sort list of key/value pairs by value
sortByValue :: Ord v => [(k, v)] -> [(k, v)]
sortByValue = sortBy $ flip $ comparing snd

-- Divide the list into chunks of n elements
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l
    | n > 0 = take n l : chunksOf n (drop n l)
    | otherwise = chunksOf 1 l

-- Return sum of all values in the occurence map
sumValues :: FreqMap -> Float
sumValues occurenceMap = sum $ map snd occurenceMap
