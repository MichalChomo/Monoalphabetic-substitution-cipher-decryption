-- FLP functional project subs-cipher
-- xchomo01, Michal Chomo
module KeyUtils where

import Data.List
import Data.Maybe
import qualified Data.Map as Map
import FreqMap

-- Resulting mapping of ciphertext letters to plaintext letters, one to one
type KeyMapChar = [(Char, Char)]
-- Mapping of ciphertext letters to plaintext letters, String because of
-- digrams and trigrams
type KeyMapString = [(String, String)]

-- Get key in format:
-- plaintext alphabet
-- ciphertext letters
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

-- Get key that is composed of keys from frequency analysis of letters, digrams
-- and trigrams
getCombinedKey :: String -> String -> KeyMapString
getCombinedKey db ciphertext =
    combineKeys
    (combineKeys
        (getKeyMapString (addMissingLetters (freqAnalysisLetter ciphertext)) (parseFreqMapLetter db))
        (nubTuple (getKeyFromDigram (getKeyMapString (freqAnalysisDigram ciphertext) (parseFreqMapDigram db)))))
    (nubTuple (getKeyFromTrigram (getKeyMapString (freqAnalysisTrigram ciphertext) (parseFreqMapTrigram db))))

-- Combine keys, gramKey has priority, so if ciphertext letter is the same in
-- key and gramKey, plaintext letter from gramKey is used
combineKeys :: KeyMapString -> KeyMapString -> KeyMapString
combineKeys key gramKey = combine key gramKey
    where combine k [] = k
          combine k ((x, y):xs) = combine (map (replace (x, y)) k) xs
          replace (a, b) (c, d)
            | a == c = (a, b)
            | otherwise = (c, d)

-- Remove tuples with duplicate keys, keep the one closest to the start of the list
nubTuple :: KeyMapString -> KeyMapString
nubTuple key = nubBy isKeyEq key
    where
        isKeyEq (a, _) (c, _)
          | a == c = True
          | otherwise = False

-- Get mapping of ciphertext letters/digrams/trigrams to plaintext letters/
-- digrams/trigrams from two frequency maps
getKeyMapString :: FreqMap -> FreqMap -> KeyMapString
getKeyMapString fa db = zip (map fst fa) (map fst db)

-- Add missing letters with zero frequency to the frequency map
addMissingLetters :: FreqMap -> FreqMap
addMissingLetters fa = sortByValue $ Map.toList $ Map.union
                        (Map.fromList fa) (Map.fromList getZeroFreqMap)
    where getZeroFreqMap = map (\x -> ([x], 0.0)) ['a'..'z']

-- Get mapping of one letter to one letter instead of digram to digram
getKeyFromDigram :: KeyMapString -> KeyMapString
getKeyFromDigram [] = []
getKeyFromDigram ((x, y):xs) = [(take 1 x, take 1 y), (drop 1 x, drop 1 y)]  ++ getKeyFromDigram xs

-- Get mapping of one letter to one letter instead of trigram to trigram
getKeyFromTrigram :: KeyMapString -> KeyMapString
getKeyFromTrigram [] = []
getKeyFromTrigram ((x, y):xs) =
    [(take 1 x, take 1 y), (drop 1 (take 2 x), drop 1 (take 2 y)),
    (drop 2 x, drop 2 y)]  ++ getKeyFromTrigram xs
