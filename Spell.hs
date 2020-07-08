module Spell where

import System.IO
import Data.List

edits :: String -> [String]
edits word = deletes ++ transposes ++ replaces ++ inserts
  where
    letters    = "abcdefghijklmnopqrstuvwxyz"
    splits     = [ splitAt i word                  | i <- [1 .. length word] ]
    deletes    = [ l ++ tail r                     | (l,r) <- splits, (not . null) r ]
    transposes = [ l ++ r !! 1 : head r : drop 2 r | (l,r) <- splits, length r > 1 ]
    replaces   = [ l ++ c : tail r                 | (l,r) <- splits, (not . null) r, c <- letters ]
    inserts    = [ l ++ c : r                      | (l,r) <- splits, c <- letters]

known :: [String] -> [String] -> [String]
known candidates words = do
    [w | w <- candidates, elem w words]

correct :: String -> IO ()
correct word = do
    handle <- openFile "list.dic" ReadMode
    contents <- hGetContents handle
    let phrases = lines contents
    if elem word phrases
    then do
        putStr word
    else do
        let candidates = sort $ known (edits word) phrases
        if (length candidates > 0)
        then putStr (candidates !! 0)
        else putStr word
    hClose handle