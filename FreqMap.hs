-- FLP functional project subs-cipher
-- xchomo01, Michal Chomo
module FreqMap where

import Data.List
import Data.Ord

-- Type representing the whole frequency database
type FreqMap = [(String, Float)]

-- Perform a frequency analysis on the ciphertext
freqAnalysis :: FreqMap -> FreqMap
freqAnalysis occurenceMap = sortByValue $
    map (\x -> (fst x, 100 * snd x / sumValues occurenceMap)) occurenceMap

freqAnalysisLetter :: String -> FreqMap
freqAnalysisLetter ciphertext = freqAnalysis $ getOccurences ciphertext

freqAnalysisDigram :: String -> FreqMap
freqAnalysisDigram ciphertext = freqAnalysis $ getOccurencesDigram ciphertext

freqAnalysisTrigram :: String -> FreqMap
freqAnalysisTrigram ciphertext = freqAnalysis $ getOccurencesTrigram ciphertext

-- Count occurences of characters in text and store them in map
getOccurences :: String -> FreqMap
getOccurences [] = []
getOccurences text = map (\x -> ([head x], fromIntegral $ length x)) $
    group . sort $ text

getOccurencesDigram :: String -> FreqMap
getOccurencesDigram [] = []
getOccurencesDigram text = getOccurencesGram 2 text

getOccurencesTrigram :: String -> FreqMap
getOccurencesTrigram [] = []
getOccurencesTrigram text = getOccurencesGram 3 text

-- Count occurences of n-grams in text and store them in map
getOccurencesGram :: Int -> String -> FreqMap
getOccurencesGram _ [] = []
getOccurencesGram n text = go $ chunksOf n text ++ chunksOf n (tail text)
    where getGramCount gram = length . filter (== gram)
          removeGram gram = filter (/= gram)
          go [] = []
          go l = (head l, fromIntegral (getGramCount (head l) l)) :
            go (removeGram (head l) l)

parseFreqMap :: Int -> String -> FreqMap
parseFreqMap len dbString = map getPair $ filter (isLen len) $ lines dbString

parseFreqMapLetter :: String -> FreqMap
parseFreqMapLetter = parseFreqMap 1

parseFreqMapDigram :: String -> FreqMap
parseFreqMapDigram = parseFreqMap 2

parseFreqMapTrigram :: String -> FreqMap
parseFreqMapTrigram = parseFreqMap 3

getPair :: String -> (String, Float)
getPair line = (head (words line), read (last (words line)) :: Float)

isLen :: Int -> String -> Bool
isLen len line
    | length (head $ words line) == len = True
    | otherwise = False

sortByValue :: Ord v => [(k, v)] -> [(k, v)]
sortByValue = sortBy $ flip $ comparing snd

-- Divide the list into chunks of n elements
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l
    | n > 0 = take n l : chunksOf n (drop n l)
    | otherwise = chunksOf 1 l

-- Return sum of all values in frequency map
sumValues :: FreqMap -> Float
sumValues l = sum $ map snd l
