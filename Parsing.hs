-- FLP functional project subs-cipher
-- xchomo01, Michal Chomo
module Parsing where

getDbFilename :: [String] -> String
getDbFilename args
    | head (getOneBeforeLast args) == '-' = last args
    | otherwise = getOneBeforeLast args

getOneBeforeLast :: [String] -> String
getOneBeforeLast args = args !! (length args - 2)

getInputFilename :: [String] -> String
getInputFilename = last
