module AocLib (read_input, prepend, split_empty, raw_input, uniq) where

import System.IO
import Control.Monad
import Data.List.Split
import Data.List
import System.Environment

uniq :: (Eq a) => [a] -> [a]
uniq [v] = [v]
uniq (h:v:hs) 
 | h == v = uniq (v:hs)
 | otherwise = h:(uniq (v:hs))

raw_input :: IO String
raw_input = do
    args <- getArgs
    cont <- readFile $ head $ args
    return cont

read_input :: IO [String]
read_input = do 
        contents <- raw_input
        let result = splitOn "\n" contents
        return $ init $ result


prepend' :: [[String]] -> String -> [[String]]
prepend' (h:hs) "" = []:h:hs
prepend' [[]] val = [[val]]
prepend' (h:hs) val = (h ++ [val]) : hs

prepend :: [String] -> [[String]]
prepend lst = foldl prepend' [[]] lst

split_empty :: [String] -> [[String]]
split_empty l = reverse $ prepend $ l 


