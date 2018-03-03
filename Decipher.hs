-- FLP functional project subs-cipher
-- xchomo01, Michal Chomo
module Decipher where

import qualified Data.Map as Map
import FreqMap

decipher :: String -> String -> (String, String)
decipher db ciphertext = (show (getFinalKey db ciphertext), show (applyKey ciphertext (getFinalKey db ciphertext)))

-- Substitute letters in text according to key
applyKey :: String -> [(String, String)] -> String
applyKey ciphertext key =
    map (\x -> head (Map.findWithDefault [] [x] (Map.fromList key))) ciphertext

getFinalKey :: String -> String -> [(String, String)]
getFinalKey db ciphertext = combineKeys (combineKeys (getKey (freqAnalysisLetter ciphertext) (parseFreqMapLetter db)) (removeDuplicatesFromKey (getKey (freqAnalysisDigram ciphertext) (parseFreqMapDigram db)))) (removeDuplicatesFromTrigramKey (getKey (freqAnalysisTrigram ciphertext) (parseFreqMapTrigram db)))

getKey :: FreqMap -> FreqMap -> [(String, String)]
getKey fa db = zip (map fst fa) (map fst db)

combineKeys :: [(String, String)] -> [(String, String)] -> [(String, String)]
combineKeys key gramKey = Map.toList $ Map.union (Map.fromList gramKey) (Map.fromList key)

-- Remove duplicate keys, keep the ones closest to the start of the list
removeDuplicatesFromKey :: [(String, String)] -> [(String, String)]
removeDuplicatesFromKey l = Map.toList $ Map.fromList $ reverse $ getKeyFromDigram l

removeDuplicatesFromTrigramKey :: [(String, String)] -> [(String, String)]
removeDuplicatesFromTrigramKey l = Map.toList $ Map.fromList $ reverse $ getKeyFromTrigram l

getKeyFromDigram :: [(String, String)] -> [(String, String)]
getKeyFromDigram [] = []
getKeyFromDigram ((x, y):xs) = [(take 1 x, take 1 y), (drop 1 x, drop 1 y)]  ++ getKeyFromDigram xs

getKeyFromTrigram :: [(String, String)] -> [(String, String)]
getKeyFromTrigram [] = []
getKeyFromTrigram ((x, y):xs) = [(take 1 x, take 1 y), (drop 1 (take 2 x), drop 1 (take 2 y)), (drop 2 x, drop 2 y)]  ++ getKeyFromTrigram xs
