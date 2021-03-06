{-# LANGUAGE OverloadedStrings #-}

module En where

import System.IO
import System.IO.Silently
import Control.Monad
import System.Environment
import Data.List
import Data.Maybe
import Text.Regex.PCRE.Heavy
import Data.ByteString.UTF8 (fromString)
import Data.Either (rights)
import Data.Text (replace, pack, unpack, splitOn, breakOn)
import Spell

romanArray = ["I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"]
romanRegex = regexp ("^" ++ fromArray romanArray ++ " ")

arabicDotArray = map (++ ".") $ map show $ take 6 $ iterate (+1) 1
arabicDotRegex = regexp ("^" ++ fromArray arabicDotArray)

arabicBracketArray = map (++ ")") $ map show $ take 34 $ iterate (+1) 1
arabicBracketRegex = regexp ("^" ++ fromArray arabicBracketArray)

cyrillicBracketArray = map (++ ")") $ group "абвгдежзиклмнопрстуфхцчшщэ"
cyrillicBracketRegex = regexp ("^" ++ fromArray cyrillicBracketArray)

listString = "(?: " ++ fromArray romanArray ++ ")?(?: " ++ fromArray arabicDotArray ++ ")?(?: " ++ 
             fromArray arabicBracketArray ++ ")?(?: " ++ fromArray cyrillicBracketArray ++ ")?"

langArray = ["anc.-gr.", "arab.", "chin.", "fr.", "germ.", "greek", "indian", "irish", "it.", "jap.", "lat.", 
             "persian", "pol.", "roman", "rus.", "s.-afr.", "sanskr.", "scot.", "span.", "turk."]
langString = fromArray langArray

speechPartArray = ["noun", "pron.", "v.", "adj.", "adv.", "prep.", "cj.", "interj.", "predic.", "num.", "suf.", 
                   "pref.", "artic."]
speechPartString = fromArray speechPartArray

pastFormString = "(past and past part\\.( [a-z'-]+,)*( [a-z'-]+)|past part\\.( [a-z'-]+,)*( [a-z'-]+)|past sg\\. [a-z'-]+, pl\\. [a-z'-]+|past( [a-z'-]+,)*( [a-z'-]+))"

usuString = "(usu\\. (past part\\.|pres\\. part\\.|refl\\. or pass\\.|refl\\.|pass\\.|pl\\.|mil\\.|amer\\.|predic\\.|imp\\.|neg\\.|collect\\.|joc\\.|fig\\.|disapprov\\.|disdain\\.|abbr\\.( [a-z]\\.)+|([a-z]+,? )*[a-z]+))"

alsoString = "(also (sandal wood|a pair of crutches|as sg\\.|fig\\.|tech\\.|geol\\.|refl\\.|leg\\.|gram\\.|astr\\.|joc\\.|pl\\.|sg\\.|mil\\.|physiol\\.|no\\.|iron\\.|phys\\.|zool\\.|philos\\.|math\\.|[a-zA-Z-]+))"

oftString = "(oft\\.( [a-z]+\\.?)*)"

plString = "(pl\\.( of [a-z]+| as sg\\.| and sg\\.| invar\\.|( [A-Za-z'-]+,?)*))"

abbrString = "(abbr\\.(( of( [A-Za-z+-]+,?)+)|( [0-9A-Za-z]+)\\.?)?)"

dimString = "(dim\\. of( [A-Za-z]+,?)*)"

superlString = "(superl\\.(( of( [a-z]+,?)+)|( [a-z]+,?)+))"

compString = "(comp\\.(( of( [a-z]+,?)+)|( [a-z]+,?)+)?)"

negString = "(neg\\.( of [a-z]+)?)"

fString = "(f\\. \\-[a-z']+)"

instString = "((wrong|euphem\\.) (inst\\. )?of [a-z]+)"

persString = "(pers\\.( obj\\. (of [a-z]+|invar\\.|[a-z]+))?)"

asString = "(as (adj\\.|adv\\.|noun|pl\\.|sg\\.))"

emphString = "(emph\\.( of [a-z]+)?)"

objString = "(obj\\. ((of [a-z]+)|[a-z]+))"

sgString = "(sg\\.( (and pl\\.|only))?)"

otherArray = ["account.", "acoust.", "aeron.", "affect.", "agric.", "amer.", "anat.", "anthrop.", 
              "arch.", "archaeol.", "archit.", "art", "artil.", "astr.", "attr.", "austral.", "bacter.", "bank.", 
              "bibl.", "bioch.", "biol.", "book.", "bot.", "br.", "card.", "cards", "chem.", "chess", "child.", 
              "cin.", "coll.", "collect.", "comm.", "comput.", "computer", "conj.", "constr.", "cul.", "demonstr.", 
              "dial.", "dipl.", "disapprov.", "disdain.", "eccl.", "econ.", "egypt.", "electr.", "esp.","ethnogr.", 
              "euphem.", "exch.", "fig.", "fin.", "forest.", "geod.", "geogr.", "geol.", "geom.", "gram.", "herald.", 
              "hist.", "holl.", "hung.", "hunt.", "hydr.", "imp.", "indef.", "inf.", "insur.", "inter.", "iron.", 
              "jargon", "joc.", "leg.", "ling.", "lit.", "logic", "math.", "mech.", "med.", "metal", "metal.", 
              "meteor.", "mil.", "min.", "mining", "mot.", "mus.", "myth.", "naut.", "north.", "norweg.", "obs.", 
              "opt.", "ord.", "paint.", "paleont.", "parl.", "pass.", "pejor.", "pf.", "pharm.", "philos.", "phon.", 
              "phot.", "phys.", "physiol.", "poet.", "polit.", "port.", "poss.", "prosody", "prov.", "psych.", 
              "radio", "railways", "rare", "recipr.", "refl.", "rel.", "relat.", "rhet.", "road", "rocket", "rude", 
              "scand.", "school", "sl.", "spec.", "sport", "spread", "stud.", "styl.", "surg.", "tech.", "telegr.", 
              "teleph.", "text.", "theatr.", "topogr.", "tv", "typ.", "univ.", "vers.", "vet.", "vulg.", "zool."]
otherString = fromArray otherArray

wordString = "(" ++ langString ++ "|" ++ speechPartString ++ "|" ++ pastFormString ++ "|" ++ usuString ++ "|" ++ 
             alsoString ++ "|" ++ oftString ++ "|" ++ plString ++ "|" ++ abbrString ++ "|" ++ dimString ++ "|" ++ 
             superlString ++ "|" ++ compString ++ "|" ++ negString ++ "|" ++ fString ++ "|" ++ instString ++ "|" ++
             persString ++ "|" ++ asString ++ "|" ++ emphString ++ "|" ++ objString ++ "|" ++ sgString ++ "|" ++
             otherString ++ ")"
wordRegex = regexp ("^" ++ wordString ++ "(;| |$)")

bracketsString = "(\\(.*?\\)|\\.*?\\])"
bracketsRegex = regexp ("^" ++ bracketsString ++ ";? ")

linkString = "(?:(?:'|-)?[a-zA-Z'./]+(?:-[a-zA-Z'./]+){0,3}(?: [a-zA-Z'./]+(?:-[a-zA-Z'./]+){0,3})*)(?: " ++ optionalFromArray romanArray ++ ")?(?: " ++
             optionalFromArray arabicDotArray ++ ")?(?: " ++ optionalFromArray arabicBracketArray ++ ")?(?: " ++
             optionalFromArray cyrillicBracketArray ++ ")?"

hyphenRegex = regexp ("^- ")

formString = "(see|past|Syn:|2nd|3rd sg\\.|f\\. of)"
formRegex = regexp ("^" ++ formString ++ " .+$")

russianString = "-?(?:\\d{1,4} |1/\\d{1,2} |\\d{1,3}-)?(?:[DSVXY]-)?['\"]?[ёа-яА-Я]"
russianRegex = regexp ("^" ++ russianString ++ ".*$")

englishString = "([a-zA-Z\\d()'.=/&!?]+(?:-[a-zA-Z\\d()'.=/&!?]+)*,?(?: [a-zA-Z\\d()'.=/&!?]+(?:-[a-zA-Z\\d()'.=/&!?]+)*,?)*;?(?:$| ))"
englishRegex = regexp ("^" ++ englishString ++ ".*$")

emptyRegex = regexp ("^$")

regexp :: String -> Regex
regexp string = rights [compileM (fromString string) []] !! 0

escape :: String -> String
escape string = unpack $ replace "." "\\." $ replace ")" "\\)" $ pack string

fromArray :: [String] -> String
fromArray array = "(" ++ escape (intercalate "|" array) ++ ")"

optionalFromArray :: [String] -> String
optionalFromArray array = "(?:" ++ escape (intercalate "|" array) ++ ")"

indexOf :: String -> [String] -> Maybe Int
indexOf string list = elemIndex string list

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
        let regex = regexp ("^" ++ (regexArray !! index) ++ " .*? (" ++ (regexArray !! (index + 1)) ++ " .*)$")
        if string =~ regex :: Bool
        then do
            let part = (\[(_, a)] -> a !! 0) (scan regex string :: [(String, [String])])
            parse $ take (length string - length part - 1) string
            parse part
        else do
            putStrLn begin
            parse $ drop (length begin + 1) string

parseWords :: String -> String -> IO()
parseWords word string = do
    let begin = ((\[(a, _)] -> a) (scan (regexp ("^" ++ word ++ ";?( " ++ word ++ ";?)*( |$)")) 
                                        string :: [(String, [String])]))
    putStrLn begin
    parse $ drop (length begin) string

parseBrackets :: String -> IO()
parseBrackets string = do
    let begin = ((\[(a, _)] -> a) (scan bracketsRegex string :: [(String, [String])]))
    putStrLn begin
    parse $ drop (length begin) string

numerateArray :: [[String]] -> [String]
numerateArray links = do
    let index = (minimum $ map length links) - 1
    if index > 0 && length links > 1
    then do
        let element = links !! 0 !! index
        if elem element romanArray
        then romanArray
        else if elem element arabicDotArray
        then arabicDotArray
        else if elem element arabicBracketArray
        then arabicBracketArray
        else cyrillicBracketArray
    else if length links == 1
    then repeat ""
    else romanArray

parseLink :: String -> IO()
parseLink string = do
    let regex = regexp ("(^= " ++ linkString ++ "(?:, " ++ linkString ++ ")*-?;?)(?: |$)(.+)?$")
    let begin = (\[(_, a)] -> a !! 0) (scan regex string :: [(String, [String])])
    let linkRegex = regexp ("^= (" ++ linkString ++ ")(?:, (" ++ linkString ++ "))?(?:, (" ++ linkString ++ "))?;?")
    let links = (\[(_, a)] -> a) (scan linkRegex begin)
    let splitedLinks = splitLinks links
    let numerator = numerateArray splitedLinks
    parseLinks numerator splitedLinks
    parse $ drop (length begin + 1) string

parseLinks :: [String] -> [[String]] -> IO()
parseLinks _ [] = return ()
parseLinks (n:ns) (l:ls) = do
    handleList <- openFile "list.dic" ReadMode
    contentsList <- hGetContents handleList
    let index = indexOf (l !! 0) (lines contentsList)
    if isJust index
    then do
        parse n
        parsePart (fromJust index) (head l) (tail l)
    else putStrLn ("= "++ (l !! 0))
    parseLinks ns ls
    hClose handleList

parsePart :: Int -> String -> [String] -> IO()
parsePart index definition part = do
    handleDict <- openFile "en.dic" ReadMode
    contentsDict <- hGetContents handleDict
    let string = lines contentsDict !! index
    let substring = drop (length definition + 1) string
    extractPart part substring
    hClose handleDict

extractPart :: [String] -> String -> IO()
extractPart [] string = parse string
extractPart (p:ps) string = do
    let array = if elem p romanArray
                then romanArray
                else if elem p arabicDotArray
                then arabicDotArray
                else if elem p arabicBracketArray
                then arabicBracketArray
                else cyrillicBracketArray
    let regexArray = map escape array
    parse $ (\[(_, a)] -> a !! 0) (scan (regexp ("^(.*?)" ++ escape (array !! 0) ++ ".*$")) string)
    let index = fromMaybe (-1) $ findIndex (\a -> (p == a)) array
    let part = if (index == length array - 1) || (index == -1)
               then do
                   drop (length p + 1) string
               else do
                   let regex = regexp ("^.*?" ++ (regexArray !! index) ++ " (.*?)(?: " ++ 
                                       (regexArray !! (index + 1)) ++ " .*)?$")
                   (\[(_, a)] -> a !! 0) (scan regex string)
    extractPart ps part

splitLinks :: [String] -> [[String]]
splitLinks [] = []
splitLinks (l:ls) = do
    let link = filter (\a -> a /= "") ((\[(_, a)] -> a) (scan (regexp ("^(.*?)" ++ listString ++ "$")) l))
    [link] ++ splitLinks ls

parseAbbr :: String -> IO()
parseAbbr string = do
    let regex = regexp ("^(.*?)( " ++ optionalFromArray arabicDotArray ++ ")? " ++ speechPartString)
    let begin = (\[(_, a)] -> a !! 0) (scan regex string :: [(String, [String])])
    parse begin
    parse $ drop (length begin + 1) string

parseEnglish :: String -> IO()
parseEnglish string = do
    let begin = (\[(_, a)] -> a !! 0) (scan englishRegex string :: [(String, [String])])
    putStrLn begin
    parse $ drop (length begin) string

parseRussian :: String -> IO()
parseRussian string = do
    if string =~ regexp "^.* \\[.*\\].*$"
    then do
        let begin = (\[(_, a)] -> a !! 0) (scan (regexp "^(.*?) \\[.*$") string)
        parse begin
        parse $ drop (length begin + 1) string
    else if string =~ regexp "^.* \\(.*\\).*$"
    then do
        let begin = (\[(_, a)] -> a !! 0) (scan (regexp "^(.*?) \\(.*$") string)
        parse begin
        parse $ drop (length begin + 1) string
    else if string =~ regexp "^.*; .*$"
    then do
        let begin = (\[(_, a)] -> a !! 0) (scan (regexp "^(.*?;) .*$") string)
        parse begin
        parse $ drop (length begin + 1) string
    else if string =~ regexp "^.* - .*$"
    then do
        let begin = (\[(_, a)] -> a !! 0) (scan (regexp "^(.*?) - .*$") string)
        parse begin
        parse $ drop (length begin + 3) string
    else if string =~ regexp "^.+ [a-zA-Z].*$"
    then do
        let begin = (\[(_, a)] -> a !! 0) (scan (regexp "^(.+?) [a-zA-Z].*$") string)
        parse begin
        parse $ drop (length begin + 1) string
    else
        putStrLn string

parse :: String -> IO()
parse string = do
    if string =~ romanRegex :: Bool
    then do
        parseNumeric romanArray string
    else if string =~ wordRegex :: Bool
    then do
        parseWords wordString string
    else if string =~ arabicDotRegex :: Bool
    then do
        parseNumeric arabicDotArray string
    else if string =~ arabicBracketRegex :: Bool
    then do
        parseNumeric arabicBracketArray string
    else if string =~ cyrillicBracketRegex :: Bool
    then do
        parseNumeric cyrillicBracketArray string
    else if string =~ bracketsRegex
    then do
        parseBrackets string
    else if string =~ regexp ("^= " ++ linkString)
    then do
        parseLink string
    else if string =~ regexp (" " ++ speechPartString ++ ";? ")
    then do
        parseAbbr string
    else if string =~ hyphenRegex
    then do
        parse $ drop 2 string
    else if string =~ formRegex
    then do
        putStrLn string
    else if string =~ russianRegex
    then do
        parseRussian string
    else if string =~ englishRegex
    then do
        parseEnglish string
    else if not (string =~ emptyRegex)
    then do
        putStrLn string
    else
        return ()

splitTranslation :: String -> [String]
splitTranslation string = do
    let withoutSemicolumn = replace (pack ";") (pack "") (pack string)
    let splited = splitOn (pack ", ") withoutSemicolumn
    map unpack splited

containTranslation :: String -> String -> String -> IO String
containTranslation word phrase translation = do
    text <- capture_ $ parse $ drop (length phrase + 1) translation
    let russianWords = filter (\a -> a =~ regexp ("^[^\\(].*[ёа-яА-Я].*")) (lines text)
    let words = concat $ map splitTranslation russianWords
    if any (== word) words
    then return $ phrase
    else return $ ""


filterTranslations :: String -> [String] -> [String] -> IO()
filterTranslations word list dictionary = do
    let translationArray = filter (\a -> a =~ regexp ("^.*" ++ word ++ ".*$")) dictionary
    let indexArray = map (\a -> fromJust $ elemIndex a dictionary) translationArray
    stringArray <- sequence $ map (\i -> containTranslation word (list !! i) (dictionary !! i)) indexArray
    putStrLn $ mconcat $ intersperse ", " $ filter (( /= 0) . length) stringArray


findRussianWord :: String -> IO()
findRussianWord string = do
    handleDict <- openFile "en.dic" ReadMode
    contentsDict <- hGetContents handleDict
    handleList <- openFile "list.dic" ReadMode
    contentsList <- hGetContents handleList
    filterTranslations string (lines contentsList) (lines contentsDict)
    hClose handleDict
    hClose handleList