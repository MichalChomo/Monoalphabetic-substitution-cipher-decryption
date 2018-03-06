-- FLP functional project subs-cipher
-- xchomo01, Michal Chomo
import System.Environment
import Control.Monad
import Parsing
import KeyUtils

main :: IO ()
main = do
    args <- getArgs
    -- Check count of arguments
    if length args < 2
        then error "Too few arguments"
        else if length args > 4
                then error "Too many arguments"
                else process args

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
    --when isKey (putStrLn $ "Key:\n" ++ getKeyString (getFinalKey db ciphertext))
    when isKey (putStrLn $ "Key:\n" ++ show (getFinalKey db ciphertext))
    when isText (putStrLn $ "Deciphered text:\n" ++ show (applyKey ciphertext (getFinalKey db ciphertext)))
