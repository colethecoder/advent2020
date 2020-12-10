module Advent5
    ( advent5
    ) where

import System.IO ()
import Data.List ( sort ) 
import Paths_advent

advent5 :: IO ()
advent5 = do  
    filepath <- getDataFileName "input5.txt"
    contents <- readFile filepath
    let cards = words contents
        seatnos = map seatCalculator cards
        max = maximum seatnos
        min = minimum seatnos
        sorted = sort seatnos
        myseat = missing min sorted
    print max
    print myseat

missing :: Int -> [Int] -> Int
missing exp [] = 0
missing exp (x:xs) =
    if x == exp then missing (exp+1) xs else exp

seatCalculator :: [Char] -> Int
seatCalculator xs =
    let row = seatPosCalculator 'F' 'B' (take 7 xs) [0..127]
        col = seatPosCalculator 'L' 'R' (drop 7 xs) [0..7] in
    (row * 8) + col

seatPosCalculator :: Char -> Char -> [Char] -> [Int] -> Int
seatPosCalculator _ _ [] rows = head rows
seatPosCalculator min max (x:xs) rows = 
    let (f, b) = split rows in
        if x == min
        then seatPosCalculator min max xs f
        else seatPosCalculator min max xs b

split :: [a] -> ([a], [a])
split myList =
    let listLength = length myList
    in
        if even listLength
        then internalSplit myList (listLength `div` 2)
        else internalSplit myList ((listLength `div` 2) + 1)

internalSplit :: [a] -> Int -> ([a], [a])
internalSplit myList splitLength = ((take splitLength myList), (drop splitLength myList))


