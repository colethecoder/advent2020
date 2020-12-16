module Advent10
    ( advent10
    ) where

import System.IO ()
import Data.List ( sort, nub, intersect, tails )
import Text.ParserCombinators.Parsec
import Paths_advent

advent10 :: IO ()
advent10 = do  
    filepath <- getDataFileName "input10.txt"
    contents <- readFile filepath   
    let nums = 0 : (sort $ getNumericList $ words contents)
        diffs = f 0 $  nums
        threes = length $ filter (\x -> x == 3) diffs
        ones   = length $ filter (\x -> x == 1) diffs    
    print $ ones * (threes)

getNumericList :: [String] -> [Int]
getNumericList words = map read words

f :: Int -> [Int] -> [Int]
f prev [] = [3] -- last one is always 3 higher
f prev (x:xs) = (x-prev) : f x xs

g :: [Int] -> Int
g diffs = 
    let threes = length $ filter (\x -> x == 3) diffs
        ones   = length $ filter (\x -> x == 1) diffs in
    ones * threes

--h :: Int -> [Int] -> [[Int]]


