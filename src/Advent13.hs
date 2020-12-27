module Advent13
    ( advent13
    ) where

import System.IO ()
import Data.List ( sort, nub, intersect, sortBy, maximumBy)
import Data.Ord (comparing)
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
        Right (target, buses) -> show $ f target (map snd buses) 0
    case parsed of
        Left err -> print $ show err
        Right (target, buses) -> do
            let max = maximumBy (comparing snd) buses
            print $ show max
            print $ show $ g buses (snd max) (snd max - fst max)

f :: Int -> [Int] -> Int -> (Int, Int)
f target buses added =     
    let curr = target + added
        diffs = map (\x -> curr `mod` x) buses
        combined = zip buses diffs 
        matches = filter (\x -> snd x == 0) combined in
    if null matches
    then f target buses (added + 1)
    else (fst $ head matches, added)

--This is sooooo slow, I'm obviously missing something about how to determine the lowest number
g :: [(Int, Int)] -> Int -> Int -> Int
g buses increment current =
    let b = map (\(x,y) -> ((current + x) `mod` y) == 0) buses in
    if any not b
    then g buses increment (current + increment)
    else current

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

timetableFile :: GenParser Char () (Int, [(Int,Int)])
timetableFile = do 
    target <- int
    eol
    buses <- many1 busEntry
    let zipped = zip [0..] buses
    let busList = busTimesToTuple zipped
    eol
    eof
    return (target, busList)

busEntry :: GenParser Char st BusTime
busEntry = do 
    i <- choice [bus, placeholder]
    optional $ char ','
    return i

bus :: GenParser Char st BusTime
bus = do Bus <$> int

placeholder :: GenParser Char st BusTime
placeholder = do 
    i <- char 'x'
    return Placeholder

busTimesToTuple :: [(Int, BusTime)] -> [(Int, Int)]
busTimesToTuple [] = []
busTimesToTuple ((x,y):xs) =
    case y of
        (Bus i) -> (x,i) : busTimesToTuple xs
        _       -> busTimesToTuple xs  

data BusTime =
    Bus Int
  | Placeholder
  deriving (Show, Eq)

parseTimetables :: String -> Either ParseError (Int, [(Int,Int)])
parseTimetables =  parse timetableFile "(unknown)"
