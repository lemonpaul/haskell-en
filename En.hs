{-# LANGUAGE OverloadedStrings #-}

import System.IO  
import Control.Monad
import System.Environment
import Data.List
import Data.Maybe
import Text.Regex.PCRE.Heavy
import Data.ByteString.UTF8 (fromString)
import Data.Either (rights)
import Data.Text (replace, pack, unpack)

regexp :: String -> Regex
regexp string = rights [compileM (fromString string) []] !! 0

escape :: String -> String
escape string = unpack $ replace "." "\\." $ replace ")" "\\)" $ pack string

fromArray :: [String] -> String
fromArray array = "(" ++ escape (intercalate "|" array) ++ ")"

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
        else do
            handle <- openFile "en.dic" ReadMode
            contents <- hGetContents handle
            let translationLine = tranlsateEn definition (lines contents)
            if translationLine /= ""
                then do
                    let translation = drop (length definition + 1) translationLine
                    parse translation
                else
                    putStrLn "Translation is not found."
            hClose handle

tranlsateEn :: String -> [String] -> String
tranlsateEn definition [] = ""
tranlsateEn definition (l:ls) = if isPrefixOf definition l
                                        then l
                                        else tranlsateEn definition ls

tranlsateRu :: String -> [String] -> String
tranlsateRu definition [] = ""
tranlsateRu definition (l:ls) = if l =~ (regexp definition)
                                        then l
                                        else tranlsateRu definition ls

parse :: String -> IO()
parse string = do
    let romanArray = ["I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"]
    let romanRegex = regexp $ fromArray romanArray
    let arabicDotArray = map (++ ".") $ map show $ take 6 $ iterate (+1) 1
    let arabicDotRegexArray = map escape arabicDotArray
    let arabicDotRegex = regexp $ fromArray arabicDotArray
    let speechPartArray = ["noun", "porn.", "v.", "adj.", "adv.", "prep.", "cj.", "interj.", "predic.", "num."]
    let speechPartString = fromArray speechPartArray
    let speechPartRegex = regexp speechPartString
    let arabicBracketArray = map (++ ")") $ map show $ take 34 $ iterate (+1) 1
    let arabicBracketRegexArray = map escape arabicBracketArray
    let arabicBracketRegex = regexp $ fromArray arabicBracketArray
    let beginFrom = words string !! 0
    if beginFrom =~ romanRegex :: Bool
    then do
        let index = fromMaybe (-1) $ findIndex (\a -> (beginFrom == a)) romanArray
        if (index == length romanArray - 1) || (index == -1)
        then do
            putStrLn beginFrom
            parse $ drop (length beginFrom + 1) string
        else do
            let regex = regexp ("^" ++ (romanArray !! index) ++ " .*?(" ++ (romanArray !! (index + 1)) ++ ".*)$")
            if string =~ regex :: Bool
            then do
                let part = (\[(_, a)] -> a !! 0) (scan regex string :: [(String, [String])])
                parse $ take (length string - length part - 1) string
                parse part
            else do
                putStrLn beginFrom
                parse $ drop (length beginFrom + 1) string
    else if beginFrom =~ arabicDotRegex :: Bool
    then do
        let index = fromMaybe (-1) $ findIndex (\a -> (beginFrom == a)) arabicDotArray
        if (index == length arabicDotArray - 1) || (index == -1)
        then do
            putStrLn beginFrom
            parse $ drop (length beginFrom + 1) string
        else do
            let regex = regexp ("^" ++ (arabicDotRegexArray !! index) ++ " .*?(" ++ (arabicDotRegexArray !! (index + 1)) ++ ".*)$")
            if string =~ regex :: Bool
            then do
                let part = (\[(_, a)] -> a !! 0) (scan regex string :: [(String, [String])])
                parse $ take (length string - length part - 1) string
                parse part
            else do
                putStrLn beginFrom
                parse $ drop (length beginFrom + 1) string
    else if beginFrom =~ speechPartRegex :: Bool
    then do
        let prevBeginFrom = beginFrom
        let beginFrom = if string =~ regexp (speechPartString ++ ";") :: Bool
                        then (\[(a, _)] -> a) (scan (regexp (speechPartString ++ "(; " ++ speechPartString ++ ")*")) string :: [(String, [String])])
                        else prevBeginFrom
        putStrLn beginFrom
        parse $ drop (length beginFrom + 1) string
    else if beginFrom =~ arabicBracketRegex :: Bool
    then do
        let index = fromMaybe (-1) $ findIndex (\a -> (beginFrom == a)) arabicBracketArray
        if (index == length arabicBracketArray - 1) || (index == -1)
        then do
            putStrLn beginFrom
            parse $ drop (length beginFrom + 1) string
        else do
            let regex = regexp ("^" ++ (arabicBracketRegexArray !! index) ++ " .*?(" ++ (arabicBracketRegexArray !! (index + 1)) ++ ".*)$")
            if string =~ regex :: Bool
            then do
                let part = (\[(_, a)] -> a !! 0) (scan regex string :: [(String, [String])])
                parse $ take (length string - length part - 1) string
                parse part
            else do
                putStrLn beginFrom
                parse $ drop (length beginFrom + 1) string
    else
        putStrLn string
