module Main where

import En
import Spell
import System.IO
import System.IO.Silently
import System.Environment
import Text.Regex.PCRE.Heavy
import Data.List
import Data.Maybe

main :: IO()
main = do
    args <- getArgs
    let definition = if length args > 1
                         then intercalate " " args
                         else if length args > 0
                              then args !! 0
                              else ""
    if definition == ""
    then do
        progName <- getProgName
        putStrLn $ "Usage: " ++ progName ++ " <definition>"
    else if definition =~ regexp ("^.*[ёа-яА-Я].*$")
    then do
        putStrLn definition
        findRussianWord definition
    else do
        handleList <- openFile "list.dic" ReadMode
        contentsList <- hGetContents handleList
        correction <- capture_ $ correct definition
        let index = indexOf correction (lines contentsList)
        if isJust index
        then do
            handleDict <- openFile "en.dic" ReadMode
            contentsDict <- hGetContents handleDict
            let string = lines contentsDict !! fromJust index
            putStrLn correction
            parse $ drop (length correction  + 1) string
            hClose handleDict
        else do
            putStrLn "Translation not found"
        hClose handleList