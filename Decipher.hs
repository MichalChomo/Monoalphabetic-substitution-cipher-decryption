-- FLP functional project subs-cipher
-- xchomo01, Michal Chomo
module Decipher where

decipher :: String -> String -> (String, String)
decipher db input = ("key", "text" ++ input)

