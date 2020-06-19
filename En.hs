import System.IO  
import Control.Monad
import System.Environment
import Data.List
import Data.Maybe
import Text.Regex.PCRE.Heavy
import Data.ByteString.Char8 (pack)
import Data.Either (rights)


fromString :: String -> Regex
fromString string = rights [compileM (pack string) []] !! 0

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
            let translationLine = if definition =~ fromString "[а-я]"
                                    then error "TODO"
                                    else tranlsateEn definition (lines contents)
            let translation = drop (length definition + 1) translationLine
            parse translation
            hClose handle

tranlsateEn :: String -> [String] -> String
tranlsateEn definition [] = ""
tranlsateEn definition (l:ls) = if isPrefixOf definition l
                                        then l
                                        else tranlsateEn definition ls


parse :: String -> IO()
parse string = do
    let first = fromString "(I|II|III|IV|V|VI|VII|VIII|IX)"
    let firstArray = ["I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"]
    let second = fromString "(1\\.|2\\.|3\\.|4\\.|5\\.|6\\.)"
    let secondArray = ["1.", "2.", "3.", "4.", "5.", "6."]
    let secondRegexArray = ["1\\.", "2\\.", "3\\.", "4\\.", "5\\.", "6\\."]
    let thirdString = "(noun|pron\\.|v\\.|adj\\.|adv\\.|prep\\.|cj\\.|interj\\.|predic\\.|num\\.)"
    let fourth = fromString "[1-9]|[1-2]\\d|3[0-4]\\)"
    let fourthArray = ["1)", "2)", "3)", "4)", "5)", "6)", "7)", "8)", "9)", "10)", "11)", "12)", "13)", "14)", "15)", "16)", "17)", "18)", "19)", "20)", "21)", "22)", "23)", "24)", "25)", "26)", "27)", "28)", "29)", "30)", "31)", "32)", "33)", "34)"]
    let fourthRegexArray = ["1\\)", "2\\)", "3\\)", "4\\)", "5\\)", "6\\)", "7\\)", "8\\)", "9\\)", "10\\)", "11\\)", "12\\)", "13\\)", "14\\)", "15\\)", "16\\)", "17\\)", "18\\)", "19\\)", "20\\)", "21\\)", "22\\)", "23\\)", "24\\)", "25\\)", "26\\)", "27\\)", "\\28)", "\\29)", "\\30)", "\\31)", "\\32)", "\\33)", "\\34)"]
    let beginFrom = words string !! 0
    if beginFrom =~ first :: Bool
    then do
        let index = fromMaybe (-1) $ findIndex (\a -> (beginFrom == a)) firstArray
        if (index == length firstArray - 1) || (index == -1)
        then do
            putStrLn beginFrom
            parse $ drop (length beginFrom + 1) string
        else do
            let regex = fromString ("^" ++ (firstArray !! index) ++ "( +).*?(" ++ (firstArray !! (index + 1)) ++ ".*)$")
            if string =~ regex :: Bool
            then do
                let spaces = (\[(_, a)] -> a !! 0) (scan regex string :: [(String, [String])])
                let part = (\[(_, a)] -> a !! 1) (scan regex string :: [(String, [String])])
                parse $ take (length string - length part - 1) string
                parse part
            else do
                let regex = fromString ("^" ++ (firstArray !! index) ++ "( +).*$")
                let spaces = (\[(_, a)] -> a !! 0) (scan regex string :: [(String, [String])])
                putStrLn beginFrom
                parse $ drop (length beginFrom + length spaces) string
    else if beginFrom =~ second :: Bool
    then do
        let index = fromMaybe (-1) $ findIndex (\a -> (beginFrom == a)) secondArray
        if (index == length secondArray - 1) || (index == -1)
        then do
            putStrLn beginFrom
            parse $ drop (length beginFrom + 1) string
        else do
            let regex = fromString ("^" ++ (secondRegexArray !! index) ++ "( +).*?(" ++ (secondRegexArray !! (index + 1)) ++ ".*)$")
            if string =~ regex :: Bool
            then do
                let spaces = (\[(_, a)] -> a !! 0) (scan regex string :: [(String, [String])])
                let part = (\[(_, a)] -> a !! 1) (scan regex string :: [(String, [String])])
                parse $ take (length string - length part - 1) string
                parse part
            else do
                let regex = fromString ("^" ++ (secondRegexArray !! index) ++ "( +).*$")
                let spaces = (\[(_, a)] -> a !! 0) (scan regex string :: [(String, [String])])
                putStrLn beginFrom
                parse $ drop (length beginFrom + length spaces) string
    else if beginFrom =~ fromString thirdString :: Bool
    then do
        let prevBeginFrom = beginFrom
        let beginFrom = if string =~ fromString (thirdString ++ ";") :: Bool
                        then (\[(a, _)] -> a) (scan (fromString (thirdString ++ "(; +" ++ thirdString ++ ")*")) string :: [(String, [String])])
                        else prevBeginFrom
        let spaces = (\[(_, a)] -> a !! 0) (scan (fromString (beginFrom ++ "( +).*")) string :: [(String, [String])])
        putStrLn beginFrom
        parse $ drop (length beginFrom + length spaces) string
    else if beginFrom =~ fourth :: Bool
    then do
        let index = fromMaybe (-1) $ findIndex (\a -> (beginFrom == a)) fourthArray
        if (index == length fourthArray - 1) || (index == -1)
        then do
            putStrLn beginFrom
            parse $ drop (length beginFrom + 1) string
        else do
            let regex = fromString ("^" ++ (fourthRegexArray !! index) ++ "( +).*?(" ++ (fourthRegexArray !! (index + 1)) ++ ".*)$")
            if string =~ regex :: Bool
            then do
                let spaces = (\[(_, a)] -> a !! 0) (scan regex string :: [(String, [String])])
                let part = (\[(_, a)] -> a !! 1) (scan regex string :: [(String, [String])])
                parse $ take (length string - length part - 1) string
                parse part
            else do
                let regex = fromString ("^" ++ (fourthRegexArray !! index) ++ "( +).*$")
                let spaces = (\[(_, a)] -> a !! 0) (scan regex string :: [(String, [String])])
                putStrLn beginFrom
                parse $ drop (length beginFrom + length spaces) string
    else
        putStrLn string
