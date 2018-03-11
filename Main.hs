-- FLP functional project subs-cipher
-- xchomo01, Michal Chomo
import System.Environment
import Control.Monad
import Data.Char
import KeyUtils
import FreqMap

usage :: String
usage = "./subs-cipher [-k] [-t] frequency_database [ciphertext]"

main :: IO ()
main = do
    args <- getArgs
    -- Check count of arguments
    if length args < 2
        then error usage
        else if length args > 4
                then error usage
                else process args

-- Process arguments and print the result
process :: [String] -> IO ()
process args = do
    -- Parse cmd line options
    let isText = "-t" `elem` args
    let isKey = "-k" `elem` args
    let dbFilename = getDbFilename args
    let ciphertextFilename = last args
    db <- readFile dbFilename
    ciphertext <-
        if dbFilename == ciphertextFilename
            -- If filenames are equal, there is no ciphertext file, read from stdin
            then getLine
            else readFile ciphertextFilename
    -- Output results
    let ct = filter isLetter ciphertext
    when isKey (putStrLn $ "Key:\n" ++ formatKey (getFinalKey db ct))
    --when isKey (putStrLn $ "Key:\n" ++ show (getCombinedKey db ct))
    when isText (putStrLn $ "Deciphered text:\n" ++ show (applyKey ct (getFinalKey db ct)))

-- Get name of the file with frequency database
getDbFilename :: [String] -> String
getDbFilename args
    | head (getOneBeforeLast args) == '-' = last args
    | otherwise = getOneBeforeLast args

-- Get element before the last element in list
getOneBeforeLast :: [String] -> String
getOneBeforeLast args = args !! (length args - 2)
