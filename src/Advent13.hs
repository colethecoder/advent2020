module Advent13
    ( advent13
    ) where

import System.IO ()
import Data.List ( sort, nub, intersect )
import Text.ParserCombinators.Parsec
import Paths_advent
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

advent13 :: IO ()
advent13 = do  
    filepath <- getDataFileName "input13.txt"
    contents <- readFile filepath
    let parsed = parseTimetables contents
    print $ case parsed of
        Left err -> show err
        Right (target, buses)  -> show $ f target buses 0

f :: Int -> [Int] -> Int -> (Int, Int)
f target buses added =     
    let curr = target + added
        diffs = map (\x -> curr `mod` x) buses
        combined = zip buses diffs 
        matches = filter (\x -> snd x == 0) combined in
    if null matches
    then f target buses (added + 1)
    else (fst $ head matches, added)

eol :: GenParser Char st Char
eol = char '\n'

sign :: GenParser Char st (Int -> Int)
sign = do 
    f <- choice [neg, pos]
    return f     

neg :: GenParser Char st (Int -> Int)
neg = do
    char '-'
    return negate

pos :: GenParser Char st (Int -> Int)
pos = do
    optional (char '+')
    return id

int :: GenParser Char st Int
int = do
    s <- sign
    d <- many1 digit
    return $ s $ read d

timetableFile :: GenParser Char () (Int, [Int])
timetableFile = 
    do target <- int
       eol
       buses <- many1 busEntry
       eol
       eof
       return (target, buses)

busEntry :: GenParser Char st Int
busEntry =
    do many $ oneOf "x," 
       i <- int
       many $ oneOf "x,"
       return i

parseTimetables :: String -> Either ParseError (Int, [Int])
parseTimetables =  parse timetableFile "(unknown)"
