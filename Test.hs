import En (parse)
import System.IO

translate :: [String] -> [String] -> [Int] -> IO()
translate list dict [] = do
    return ()
translate list dict (x:xs) = do
    putStrLn ("#" ++ (list !! x))
    parse $ drop (length (list !! x) + 1) (dict !! x)
    translate list dict xs

main :: IO()
main = do
    handleList <- openFile "list.dic" ReadMode
    contentList <- hGetContents handleList
    let list = (lines contentList)
    handleDict <- openFile "en.dic" ReadMode
    contentDict <- hGetContents handleDict
    let dict = (lines contentDict)
    translate list dict [0..(length list - 1)]
    hClose handleList
    hClose handleDict
