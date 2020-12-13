module Advent9
    ( advent9
    ) where

import System.IO ()
import Data.List ( sort, nub, intersect, tails )
import Text.ParserCombinators.Parsec
import Paths_advent

advent9 :: IO ()
advent9 = do  
    filepath <- getDataFileName "input9.txt"
    contents <- readFile filepath   
    let nums = getNumericList $ words contents
    let invalid = f 25 25 nums
    print invalid
    let contiguous = h invalid nums
    print contiguous
    print $ minimum contiguous + maximum contiguous

h :: Int -> [Int] -> [Int]
h target (x:xs) =
    case g target [] (x:xs) of
        Just j -> j
        Nothing -> h target xs

g :: Int -> [Int] -> [Int] -> Maybe [Int]
g target collected (n:ns)
    | (sum collected) == target = Just collected
    | (sum collected) > target = Nothing
    | otherwise = g target (n:collected) ns

getNumericList :: [String] -> [Int]
getNumericList words = map read words

f :: Int -> Int -> [Int] -> Int
f pos prev nums =
    let target = (nums !! pos) 
        start = pos - (prev)
        end = pos-1 
        range = slice start end nums 
        combos = pairs range 
        poss = map (\(x,y) -> x+y) combos in
    if elem target poss
    then f (pos+1) prev nums
    else target

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]
    