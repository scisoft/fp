module Composition where

import Data.Char
import Data.List (sort)
import qualified Data.Char as C

main = do
    poem <- readFile "bjerke.txt"
    putStr poem
    putStr "\n"
    putStr . process $ poem
    putStr $ (replicate 55 '-') ++ "\n"
    putStr . indentEachLine $ poem

process t = unlines (sort (lines t))
process' t = (unlines . sort . lines) t
process'' = unlines . sort . lines

sortLines     = unlines . sort    . lines
reverseLines  = unlines . reverse . lines
firstTwoLines = unlines . take 2  . lines

byLines f = unlines . f . lines

sortLines'     = byLines sort
reverseLines'  = byLines reverse
firstTwoLines' = byLines (take 2)

indent :: String -> String
indent s = "    " ++ s

indentEachLine :: String -> String
indentEachLine = byLines (map indent)

eachLine :: (String -> String) -> String -> String
eachLine f = unlines . map f . lines

indentEachLine' :: String -> String
indentEachLine' = eachLine indent

