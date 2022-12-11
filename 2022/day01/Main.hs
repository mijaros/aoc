module Main where

import System.IO
import Control.Monad
import Data.List.Split
import Data.List
import System.Environment
import AocLib

sumIs :: [Int] -> Int
sumIs xs = foldl (+) 0 xs

elfTotal :: [[Int]] -> [Int]
elfTotal l = [sumIs x | x <- l]

main = do
        list <- read_input
        let fixed = [f x | x <-  (split_empty list)]
        print fixed
        let result = elfTotal fixed
        print result
        putStrLn "Maximum elf!"
        print (maximum result)
        putStrLn "Finding sum for three"
        print (sumIs (take 3 (reverse $ sort $ result)))

f :: [String] -> [Int]
f = map read
