-- FLP functional project subs-cipher
-- xchomo01, Michal Chomo
module KeyUtils where

import Data.List
import Data.Maybe
import qualified Data.Map as Map
import FreqMap

type KeyMapChar = [(Char, Char)]
type KeyMapString = [(String, String)]

-- Get string in format:
-- plaintext alphabet
-- key
formatKey :: KeyMapChar -> String
formatKey key = snd (keysValues key) ++ "\n" ++ fst (keysValues key)
    where keysValues = unzip . reverse . sortByValue

-- Substitute letters in text according to key
applyKey :: String -> KeyMapChar -> String
applyKey ciphertext key = map getValChar ciphertext
    where getValChar x = fromMaybe '?' $ lookup x key

-- Ensure that one cipher character maps to exactly one plain text character
getFinalKey :: String -> String -> KeyMapChar
getFinalKey db ciphertext = get (getCombinedKey db ciphertext) [] []
    where get [] xs _ = xs
          get (x:xs) final used = get xs ((head (fst x), findNotUsed used (snd x)) : final) (used ++ [findNotUsed used (snd x)])
          findNotUsed [] vals = head vals
          findNotUsed _ [] = '-'
          findNotUsed used (x:xs)
              | x `elem` used = findNotUsed used xs
              | otherwise = x

getCombinedKey :: String -> String -> KeyMapString
--getCombinedKey db ciphertext = map (\(x, y) -> (x, getMostFrequentChar y)) $ combineKeys (combineKeys (getKeyMapString (freqAnalysisLetter ciphertext) (parseFreqMapLetter db)) (removeDuplicatesFromDigramKey (getKeyMapString (freqAnalysisDigram ciphertext) (parseFreqMapDigram db)))) (removeDuplicatesFromTrigramKey (getKeyMapString (freqAnalysisTrigram ciphertext) (parseFreqMapTrigram db)))
getCombinedKey db ciphertext = combineKeys (combineKeys (getKeyMapString (freqAnalysisLetter ciphertext) (parseFreqMapLetter db)) (removeDuplicatesFromDigramKey (getKeyMapString (freqAnalysisDigram ciphertext) (parseFreqMapDigram db)))) (removeDuplicatesFromTrigramKey (getKeyMapString (freqAnalysisTrigram ciphertext) (parseFreqMapTrigram db)))

combineKeys :: KeyMapString -> KeyMapString -> KeyMapString
--combineKeys key gramKey = Map.toList $ Map.union (Map.fromList gramKey) (Map.fromList key)
combineKeys key gramKey = combine key gramKey
    where combine k [] = k
          combine k ((x, y):xs) = combine (map (replace (x, y)) k) xs
          replace (a, b) (c, d)
            | a == c = (a, b)
            | otherwise = (c, d)

-- Remove duplicate keys, keep the ones closest to the start of the list
removeDuplicatesFromDigramKey :: KeyMapString -> KeyMapString
removeDuplicatesFromDigramKey key = nubTuple $ getKeyFromDigram key

removeDuplicatesFromTrigramKey :: KeyMapString -> KeyMapString
removeDuplicatesFromTrigramKey key = nubTuple $ getKeyFromTrigram key

nubTuple :: KeyMapString -> KeyMapString
nubTuple key = nubBy isKeyEq key
    where
        isKeyEq (a, _) (c, _)
          | a == c = True
          | otherwise = False

getKeyMapString :: FreqMap -> FreqMap -> KeyMapString
--getKeyMapString fa db = zip (map fst (addMissingLetters fa)) (map fst db)
getKeyMapString fa db = zip (map fst fa) (map fst db)

-- Add missing letters with zero frequency to the frequency map
addMissingLetters :: FreqMap -> FreqMap
addMissingLetters fa = sortByValue $ Map.toList $ Map.union
                        (Map.fromList fa) (Map.fromList getZeroFreqMap)
    where getZeroFreqMap = map (\x -> ([x], 0.0)) ['a'..'z']

getKeyFromDigram :: KeyMapString -> KeyMapString
getKeyFromDigram [] = []
getKeyFromDigram ((x, y):xs) = [(take 1 x, take 1 y), (drop 1 x, drop 1 y)]  ++ getKeyFromDigram xs

getKeyFromTrigram :: KeyMapString -> KeyMapString
getKeyFromTrigram [] = []
getKeyFromTrigram ((x, y):xs) = [(take 1 x, take 1 y), (drop 1 (take 2 x), drop 1 (take 2 y)), (drop 2 x, drop 2 y)]  ++ getKeyFromTrigram xs
