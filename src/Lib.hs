module Lib
    ( advent1
    , advent2
    , advent3
    ) where

import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Data.List
import Data.Matrix
import Text.ParserCombinators.Parsec
import Control.Lens.Fold ((^?))
import Control.Lens.Combinators (element)

advent1 :: IO ()
advent1 = do  
        handle <- openFile "c:/repos/advent/advent/src/input.txt" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
            list = multiplypairs $ filter istarget $ sumPairs $ pairs $ f singlewords
        print list
        print $ map (\(x,y,z) -> x*y*z) $ filter (\(x,y,z) -> x+y+z == 2020) $ triples $ f singlewords
        hClose handle 

advent2 :: IO ()
advent2 = do
        handle <- openFile "c:/repos/advent/advent/src/input2.txt" ReadMode
        contents <- hGetContents handle
        print $ case parsePasswords contents of
            Left err -> show err
            Right pwds -> show $ countValidPasswords pwds
        print $ case parsePasswords contents of
            Left err -> show err
            Right pwds -> show $ countValidPasswords2 pwds
        hClose handle

advent3 :: IO ()
advent3 = do
        handle <- openFile "c:/repos/advent/advent/src/input3.txt" ReadMode
        contents <- hGetContents handle
        print $ case parseMap contents of
            Left err -> show err
            Right m  -> show $ countTreesOnRoute 3 1 m
        print $ case parseMap contents of
            Left err -> show err
            Right m  -> show $ (countTreesOnRoute 1 1 m) * 
                (countTreesOnRoute 3 1 m) * 
                (countTreesOnRoute 5 1 m) * 
                (countTreesOnRoute 7 1 m) * 
                (countTreesOnRoute 1 2 m)
        hClose handle


countTreesOnRoute :: Int -> Int -> Matrix MapCell -> Int
countTreesOnRoute xStep yStep m = length $ filter (\x -> x == Tree) $ navigateMap xStep yStep m

navigateMap :: Int -> Int -> Matrix MapCell -> [MapCell]
navigateMap xStep yStep m = recursiveMapping (1,1) (nextMapPath xStep yStep) m mapSuccess

type CheckMappingSuccess = (Int,Int) -> Matrix MapCell -> Bool

type PathStep = (Int,Int) -> (Int, Int)

recursiveMapping :: (Int, Int) -> PathStep -> Matrix MapCell -> CheckMappingSuccess -> [MapCell]
recursiveMapping start step m isSucc
    | isSucc start m = []
    | otherwise = getMapCell start m : recursiveMapping (step start) step m isSucc 

mapSuccess :: (Int,Int) -> Matrix MapCell -> Bool
mapSuccess (_,y) m = (nrows m) < y

nextMapPath :: Int -> Int -> (Int, Int) -> (Int, Int)
nextMapPath xOffset yOffset (startX, startY) = (startX+xOffset, startY+yOffset)

getMapCell :: (Int, Int) -> Matrix MapCell -> MapCell
getMapCell (x,y) m = getElem (getRollingPos y $ nrows m) (getRollingPos x $ ncols m) m

getRollingPos :: Int -> Int -> Int
getRollingPos pos max
    | pos <= max = pos
    | otherwise = getRollingPos (pos-max) max

mapFile :: GenParser Char st [[MapCell]]
mapFile = 
    do result <- many mapLine
       eof
       return result

mapLine :: GenParser Char st [MapCell]
mapLine = 
    do result <- many mapCell
       eol
       return result

mapCell :: GenParser Char st MapCell
mapCell = do
    ch <- oneOf ".#"
    return (if ch == '#' then Tree else Open)

parseMap :: String -> Either ParseError (Matrix MapCell)
parseMap input =  fmap fromLists $ parse mapFile "(unknown)" input

data MapCell = Open | Tree deriving (Show, Eq)

countValidPasswords :: [PasswordEntry] -> Int
countValidPasswords pwds = length $ filter isPasswordValid pwds 

isPasswordValid :: PasswordEntry -> Bool
isPasswordValid pwd = countSpecialOccur pwd >= minOccur pwd && 
                      countSpecialOccur pwd <= maxOccur pwd

countSpecialOccur :: PasswordEntry -> Int
countSpecialOccur pwd = length $ filter (\x -> x == specialChar pwd) $ password pwd

countValidPasswords2 :: [PasswordEntry] -> Int
countValidPasswords2 pwds = length $ filter isPasswordValid2 pwds 

isPasswordValid2 :: PasswordEntry -> Bool
isPasswordValid2 pwd = charAtPositionMatches (password pwd) (specialChar pwd) (minOccur pwd) /=
                       charAtPositionMatches (password pwd) (specialChar pwd) (maxOccur pwd)

charAtPositionMatches :: String -> Char -> Int -> Bool
charAtPositionMatches str ch pos = case str ^? element (pos-1) of
                                    Just x -> x == ch
                                    Nothing -> False


eol :: GenParser Char st Char
eol = char '\n'

passwordFile :: GenParser Char st [PasswordEntry]
passwordFile = 
    do result <- many line
       eof
       return result

line :: GenParser Char st PasswordEntry
line = 
    do result <- cells
       eol
       return result

cells :: GenParser Char st PasswordEntry
cells = do
    minStr <- many1 digit
    let min = read minStr
    char '-'
    maxStr <- many1 digit
    let max = read maxStr
    char ' '
    chr <- letter
    string ": "
    pwd <- many (noneOf "\n")
    return PasswordEntry { minOccur = min
           , maxOccur = max
           , specialChar = chr
           , password = pwd
           }

parsePasswords :: String -> Either ParseError [PasswordEntry]
parsePasswords input = parse passwordFile "(unknown)" input

data PasswordEntry = PasswordEntry { minOccur :: Int  
                     , maxOccur :: Int  
                     , specialChar :: Char
                     , password :: String
                     } deriving (Show) 

f :: [String] -> [Int]
f = map read

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

triples :: [a] -> [(a, a, a)]
triples l = [(x,y,z) | (x:ys) <- tails l, (y:zs) <- tails ys, z <- zs]

sumPairs :: [(Int, Int)] -> [(Int, Int, Int)]
sumPairs list = map (sumPair) list

sumPair :: (Int, Int) -> (Int, Int, Int)
sumPair (x,y) = (x, y, x + y)

istarget :: (Int, Int, Int) -> Bool
istarget (_, _, z) = z == 2020 

multiplypairs :: [(Int, Int, Int)] -> [Int]
multiplypairs list = map multiplypair list

multiplypair :: (Int, Int, Int) -> Int
multiplypair (x, y, _) = x * y




