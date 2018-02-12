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
    if (length args) < 2
        then putStrLn "Too few arguments"
        else if (length args) > 4
                then putStrLn "Too many arguments"
                else process args

process :: [String] -> IO ()
process args = do
    -- Parse cmd line options
    let isText = isOption "-t" args
    let isKey = isOption "-k" args
    let dbFilename = getDbFilename args
    let inputFilename = getInputFilename args
    dbFileContents <- readFile dbFilename
    inputFileContents <-
        if dbFilename == inputFilename
            -- If filenames are equal, there is no input file, read from stdin
            then getLine
            else readFile inputFilename
    let (key, text) = decipher dbFileContents inputFileContents
    -- Output results
    when isKey (putStrLn $ "Key: " ++ key)
    when isText (putStr $ "Deciphered text: " ++ text)
