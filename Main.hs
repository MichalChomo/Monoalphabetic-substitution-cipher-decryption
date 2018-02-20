-- FLP functional project subs-cipher
-- xchomo01, Michal Chomo
import System.Environment
import Control.Monad
import Parsing
import Decipher

main :: IO ()
main = do
    args <- getArgs
    -- Check count of arguments
    if length args < 2
        then putStrLn "Too few arguments"
        else if length args > 4
                then putStrLn "Too many arguments"
                else process args

process :: [String] -> IO ()
process args = do
    -- Parse cmd line options
    let isText = "-t" `elem` args
    let isKey = "-k" `elem` args
    let dbFilename = getDbFilename args
    let cyphertextFilename = last args
    dbFileContents <- readFile dbFilename
    cyphertextFileContents <-
        if dbFilename == cyphertextFilename
            -- If filenames are equal, there is no cyphertext file, read from stdin
            then getLine
            else readFile cyphertextFilename
    let (key, text) = decipher dbFileContents cyphertextFileContents
    -- Output results
    when isKey (putStrLn $ "Key: " ++ key)
    when isText (putStrLn $ "Deciphered text: " ++ text)
