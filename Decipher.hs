-- FLP functional project subs-cipher
-- xchomo01, Michal Chomo
module Decipher where

import FreqMap

decipher :: String -> String -> (String, String)
decipher db input = (show (getFreqMap db), show (freqAnalysis input))

