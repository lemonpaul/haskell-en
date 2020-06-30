{-# LANGUAGE OverloadedStrings #-}

module En where

import System.IO
import Control.Monad
import System.Environment
import Data.List
import Data.Maybe
import Text.Regex.PCRE.Heavy
import Data.ByteString.UTF8 (fromString)
import Data.Either (rights)
import Data.Text (replace, pack, unpack)

romanArray = ["I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"]
romanRegex = regexp ("^" ++ fromArray romanArray)

arabicDotArray = map (++ ".") $ map show $ take 6 $ iterate (+1) 1
arabicDotRegex = regexp ("^" ++ fromArray arabicDotArray)

arabicBracketArray = map (++ ")") $ map show $ take 34 $ iterate (+1) 1
arabicBracketRegex = regexp ("^" ++ fromArray arabicBracketArray)

cyrillicBracketArray = map (++ ")") $ group "абвгдежзиклмнопрстуфхцчшщэ"
cyrillicBracketRegex = regexp ("^" ++ fromArray cyrillicBracketArray)

langArray = ["anc.-gr.", "arab.", "chin.", "fr.", "germ.", "greek", "indian", "irish", "it.", "lat.", "roman", "rus.", "s.-afr.", "scot.", "span.", "turk."]
langString = fromArray langArray
langRegex = regexp ("^" ++ langString)

speechPartArray = ["noun", "pron.", "v.", "adj.", "adv.", "prep.", "cj.", "interj.", "predic.", "num.", "suf.", "pref.", "artic."]
speechPartString = fromArray speechPartArray
speechPartRegex = regexp ("^" ++ speechPartString)

pastFormString = "(past and past part\\.( [a-z'-]+,)*( [a-z'-]+)|past part\\.( [a-z'-]+,)*( [a-z'-]+)|past sg\\. [a-z'-]+, pl\\. [a-z'-]+|past( [a-z'-]+,)*( [a-z'-]+))?"
pastFormRegex = regexp ("^" ++ pastFormString ++ ";? ")

usuString = "(usu\\. (past part\\.|pres\\. part\\.|refl\\. or pass\\.|refl\\.|pass\\.|pl\\.|mil\\.|amer\\.|predic\\.|imp\\.|neg\\.|collect\\.|joc\\.|fig\\.|disapprov\\.|disdain\\.|abbr\\.( [a-z]\\.)+|([a-z]+,? )*[a-z]+))"
usuRegex = regexp ("^" ++ usuString)

alsoString = "(also (sandal wood|a pair of crutches|as sg\\.|fig\\.|tech\\.|geol\\.|refl\\.|leg\\.|gram\\.|astr\\.|joc\\.|pl\\.|sg\\.|mil\\.|physiol\\.|no\\.|iron\\.|phys\\.|zool\\.|philos\\.|math\\.|[a-zA-Z-]+))"
alsoRegex = regexp ("^" ++ alsoString)

oftString = "(oft\\.( [a-z]+\\.?)*)"
oftRegex = regexp ("^" ++ oftString)

plString = "(pl\\.( of [a-z]+| as sg\\.| and sg\\.| invar\\.|( [A-Za-z'-]+,?)*))"
plRegex = regexp ("^" ++ plString)

abbrString = "abbr\\.(( of( [A-Za-z+-]+,?)+)|( [0-9A-Za-z]+)\\.?)?"
abbrRegex = regexp ("^" ++ abbrString)

emptyRegex = regexp ("^$")

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
            let translationLine = translateEn definition (lines contents)
            if translationLine /= ""
                then do
                    let translation = drop (length definition + 1) translationLine
                    parse translation
                else
                    putStrLn "Translation is not found."
            hClose handle

regexp :: String -> Regex
regexp string = rights [compileM (fromString string) []] !! 0

escape :: String -> String
escape string = unpack $ replace "." "\\." $ replace ")" "\\)" $ pack string

fromArray :: [String] -> String
fromArray array = "(" ++ escape (intercalate "|" array) ++ ")"

translateEn :: String -> [String] -> String
translateEn definition [] = ""
translateEn definition (l:ls) = if isPrefixOf definition l
                                        then l
                                        else translateEn definition ls

translateRu :: String -> [String] -> String
translateRu definition [] = ""
translateRu definition (l:ls) = if l =~ (regexp definition)
                                        then l
                                        else translateRu definition ls

parseNumeric :: [String] -> String -> IO()
parseNumeric array string = do
    let begin = words string !! 0
    let regexArray = map escape array
    let index = fromMaybe (-1) $ findIndex (\a -> (begin == a)) array
    if (index == length array - 1) || (index == -1)
    then do
        putStrLn begin
        parse $ drop (length begin + 1) string
    else do
        let regex = regexp ("^" ++ (regexArray !! index) ++ " .*?(" ++ (regexArray !! (index + 1)) ++ " .*)$")
        if string =~ regex :: Bool
        then do
            let part = (\[(_, a)] -> a !! 0) (scan regex string :: [(String, [String])])
            parse $ take (length string - length part - 1) string
            parse part
        else do
            putStrLn begin
            parse $ drop (length begin + 1) string

parseWord :: String -> String -> IO()
parseWord word string = do
    let beginFrom = if string =~ regexp ("^" ++ word ++ "; ?") :: Bool
                    then (\[(a, _)] -> a) (scan (regexp ("^" ++ word ++ "; ?")) string :: [(String, [String])])
                    else (\[(a, _)] -> a) (scan (regexp ("^" ++ word ++ "( |$)")) string :: [(String, [String])])
    putStrLn beginFrom
    parse $ drop (length beginFrom) string

parseWords :: String -> String -> IO()
parseWords word string = do
    let beginFrom = if string =~ regexp ("^" ++ word ++ ";") :: Bool
                    then (\[(a, _)] -> a) (scan (regexp ("^((" ++ word ++ "; )*" ++ word ++ ";?)")) string :: [(String, [String])])
                    else (\[(a, _)] -> a) (scan (regexp ("^(" ++ word ++ ";?)")) string :: [(String, [String])])
    putStrLn beginFrom
    parse $ drop (length beginFrom + 1) string

parse :: String -> IO()
parse string = do
    if string =~ romanRegex :: Bool
    then do
        parseNumeric romanArray string
    else if string =~ langRegex :: Bool
    then do
        parseWords langString string
    else if string =~ arabicDotRegex :: Bool
    then do
        parseNumeric arabicDotArray string
    else if string =~ speechPartRegex :: Bool
    then do
        parseWords speechPartString string
    else if string =~ usuRegex :: Bool
    then do
        parseWord usuString string
    else if string =~ oftRegex :: Bool
    then do
        parseWord oftString string
    else if string =~ plRegex :: Bool
    then do
        parseWord plString string
    else if string =~ pastFormRegex :: Bool
    then do
        parseWords pastFormString string
    else if string =~ alsoRegex :: Bool
    then do
        parseWord alsoString string
    else if string =~ abbrRegex :: Bool
    then do
        parseWord abbrString string
    else if string =~ arabicBracketRegex :: Bool
    then do
        parseNumeric arabicBracketArray string
    else if string =~ cyrillicBracketRegex :: Bool
    then do
        parseNumeric cyrillicBracketArray string        
    else if not (string =~ emptyRegex)
    then do
        putStrLn string
    else
        return ()