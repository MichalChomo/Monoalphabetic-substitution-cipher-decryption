-- FLP functional project subs-cipher
-- xchomo01, Michal Chomo
module Decipher where

import FreqMap

decipher :: String -> String -> (String, String)
decipher db input = (show (createFreqMap db), "text" ++ input)

