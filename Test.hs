import En (translateEn, parse)
import System.IO

translateArray :: [String] -> IO()
translateArray [] = do
    return ()
translateArray (x:xs) = do
    handle <- openFile "en.dic" ReadMode
    contents <- hGetContents handle
    let translationLine = translateEn x (lines contents)
    if translationLine /= ""
        then do
            putStrLn x
            let translation = drop (length x + 1) translationLine
            parse translation
        else
            putStrLn "Translation is not found."
    hClose handle
    translateArray xs

main :: IO()
main = do
    handle <- openFile "list.dic" ReadMode
    contents <- hGetContents handle
    let array = (lines contents)
    translateArray array
    hClose handle