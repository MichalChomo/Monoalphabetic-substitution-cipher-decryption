-- FLP functional project subs-cipher
-- xchomo01, Michal Chomo
import System.Environment
import Control.Monad
import Data.Char
import Parsing
import Decipher

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
    dbFileContents <- readFile dbFilename
    ciphertextFileContents <-
        if dbFilename == ciphertextFilename
            -- If filenames are equal, there is no ciphertext file, read from stdin
            then getLine
            else readFile ciphertextFilename
    let (key, text) = decipher dbFileContents $ filter isLetter ciphertextFileContents
    -- Output results
    when isKey (putStrLn $ "Key:\n" ++ key)
    when isText (putStrLn $ "Deciphered text:\n" ++ text)
