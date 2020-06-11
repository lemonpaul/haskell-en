import System.IO  
import Control.Monad
import System.Environment

main :: IO()
main = do
    args <- getArgs
    let word = if length args > 0
               then args !! 0
               else ""
    handle <- openFile "en.dic" ReadMode
    contents <- hGetContents handle
    putStrLn $ tranlsate word (lines contents)
    hClose handle

tranlsate :: String -> [String] -> String
tranlsate word [] = "Translation is not found."
tranlsate word (l:ls) = if words l !! 0 == word
                        then l
                        else tranlsate word ls
