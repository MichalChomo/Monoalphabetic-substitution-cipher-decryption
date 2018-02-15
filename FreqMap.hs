-- FLP functional project subs-cipher
-- xchomo01, Michal Chomo
module FreqMap where

import qualified Data.Map as Map

-- Type representing one row from the frequency database
--data FreqPair = FreqPair String Float deriving (Show)

-- Type representing the whole frequency database
type FreqMap = Map.Map String Float

createFreqMap :: String -> FreqMap
createFreqMap dbString = Map.fromList $ map getPair (lines dbString)

getPair :: String -> (String, Float)
getPair line = (head (words line), read (last (words line)) :: Float)
