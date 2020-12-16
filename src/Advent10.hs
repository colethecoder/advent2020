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
    let nums = sort $ getNumericList $ words contents
        diffs = f 0 (0:nums)
        threes = length $ filter (== 3) diffs
        ones   = length $ filter (== 1) diffs    
    print $ ones * threes
    print $ h nums [(0,1)]

getNumericList :: [String] -> [Int]
getNumericList = map read

f :: Int -> [Int] -> [Int]
f prev [] = [3] -- last one is always 3 higher
f prev (x:xs) = (x-prev) : f x xs

g :: [Int] -> Int
g diffs = 
    let threes = length $ filter (== 3) diffs
        ones   = length $ filter (== 1) diffs in
    ones * threes

h :: [Int] -> [(Int,Int)] -> Int
h (x:xs) memo =
  case xs of
    [] -> i
    _  -> h xs ((x, i):memo)
  where
    i = sum $ map snd $ takeWhile (\(y, _) -> x - y <= 3) memo
