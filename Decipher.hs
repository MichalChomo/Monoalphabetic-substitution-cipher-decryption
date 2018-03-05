-- FLP functional project subs-cipher
-- xchomo01, Michal Chomo
module Decipher where

import qualified Data.Map as Map
import FreqMap

decipher :: String -> String -> (String, String)
--decipher db ciphertext = (show (getFinalKey db ciphertext), show (applyKey ciphertext (getCombinedKey db ciphertext)))
decipher db ciphertext = ((getKeyString (getFinalKey db ciphertext)), show (applyKey ciphertext (getFinalKey db ciphertext)))

getKeyString :: [(Char, Char)] -> String
getKeyString key = snd (keysValues key) ++ "\n" ++ fst (keysValues key)
    where keysValues = unzip . reverse . sortByValue

-- Substitute letters in text according to key
applyKey :: String -> [(Char, Char)] -> String
applyKey ciphertext key = map getValChar ciphertext
    where getValChar x = Map.findWithDefault [] x (Map.fromList key)

getFinalKey :: String -> String -> [(Char, Char)]
getFinalKey db ciphertext = get (getCombinedKey db ciphertext) [] []
    where get [] xs _ = xs
          get (x:xs) final used = get xs ((getKeyChar x, if getValChar x `elem` used then '-' else getValChar x) : final) (used ++ [getValChar x])
          getKeyChar (a, _) = head a
          getValChar (_, b) = head b

getCombinedKey :: String -> String -> [(String, String)]
--getCombinedKey db ciphertext = map (\(x, y) -> (x, getMostFrequentChar y)) $ combineKeys (combineKeys (getKey (freqAnalysisLetter ciphertext) (parseFreqMapLetter db)) (removeDuplicatesFromDigramKey (getKey (freqAnalysisDigram ciphertext) (parseFreqMapDigram db)))) (removeDuplicatesFromTrigramKey (getKey (freqAnalysisTrigram ciphertext) (parseFreqMapTrigram db)))
getCombinedKey db ciphertext = combineKeys (combineKeys (getKey (freqAnalysisLetter ciphertext) (parseFreqMapLetter db)) (removeDuplicatesFromDigramKey (getKey (freqAnalysisDigram ciphertext) (parseFreqMapDigram db)))) (removeDuplicatesFromTrigramKey (getKey (freqAnalysisTrigram ciphertext) (parseFreqMapTrigram db)))

combineKeys :: [(String, String)] -> [(String, String)] -> [(String, String)]
--combineKeys key gramKey = Map.toList $ Map.union (Map.fromList gramKey) (Map.fromList key)
combineKeys key gramKey = Map.toList $ Map.unionWith (++) (Map.fromList gramKey) (Map.fromList key)

-- Remove duplicate keys, keep the ones closest to the start of the list
removeDuplicatesFromDigramKey :: [(String, String)] -> [(String, String)]
removeDuplicatesFromDigramKey l = Map.toList $ Map.fromList $ reverse $ getKeyFromDigram l
--removeDuplicatesFromDigramKey l = Map.toList $ Map.fromListWith (++) $ reverse $ getKeyFromDigram l

removeDuplicatesFromTrigramKey :: [(String, String)] -> [(String, String)]
removeDuplicatesFromTrigramKey l = Map.toList $ Map.fromList $ reverse $ getKeyFromTrigram l
--removeDuplicatesFromTrigramKey l = Map.toList $ Map.fromListWith (++) $ reverse $ getKeyFromTrigram l

getKey :: FreqMap -> FreqMap -> [(String, String)]
getKey fa db = zip (map fst (addMissingLetters fa)) (map fst db)

-- Add missing letters to the frequency map
addMissingLetters :: FreqMap -> FreqMap
addMissingLetters fa = sortByValue $ Map.toList $ Map.union
                        (Map.fromList fa) (Map.fromList getZeroFreqMap)
    where getZeroFreqMap = map (\x -> ([x], 0.0)) ['a'..'z']

getKeyFromDigram :: [(String, String)] -> [(String, String)]
getKeyFromDigram [] = []
getKeyFromDigram ((x, y):xs) = [(take 1 x, take 1 y), (drop 1 x, drop 1 y)]  ++ getKeyFromDigram xs

getKeyFromTrigram :: [(String, String)] -> [(String, String)]
getKeyFromTrigram [] = []
getKeyFromTrigram ((x, y):xs) = [(take 1 x, take 1 y), (drop 1 (take 2 x), drop 1 (take 2 y)), (drop 2 x, drop 2 y)]  ++ getKeyFromTrigram xs
