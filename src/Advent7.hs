module Advent7
    ( advent7
    ) where

import System.IO ()
import Data.List ( sort, nub, intersect, isSubsequenceOf )
import Text.ParserCombinators.Parsec
import Paths_advent
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

advent7 :: IO ()
advent7 = do  
    filepath <- getDataFileName "input7.txt"
    contents <- readFile filepath   
    print $ case parseBags contents of
        Left err -> show err
        Right r -> show $ length $ filter (\x -> x/="shiny gold") $ h ["shiny gold"] r
    print $ case parseBags contents of
        Left err -> show err
        Right r -> show $ i "shiny gold" r

i :: String -> [(String, [(Int, String)])] -> Int
i col bags = 
    let bag = head $ filter (\x -> col == fst x) bags in
    sum $ map (\x -> (fst x) * (1 + (i (snd x) bags))) $ snd bag


h :: [String] -> [(String, [(Int, String)])] -> [String]
h inners bags = 
    let outers = sort $ foldr (f inners) [] bags in
    if outers == [] || (isSubsequenceOf outers $ sort inners)
    then inners
    else h (sort $ nub $ inners ++ outers) bags


f :: [String] -> (String, [(Int, String)]) -> [String] -> [String]
f matches (key, val) acc = if g matches val then key : acc else acc

g :: [String] -> [(Int,String)] -> Bool
g _ [] = False
g matches ((_, xName):xs) = 
    if any (\x->xName == x) matches 
    then True 
    else g matches xs

eol :: GenParser Char st Char
eol = char '\n'

bagsFile :: GenParser Char () [(String, [(Int, String)])]
bagsFile = 
    do result <- many1 bagEntry
       eof
       return result

bagEntry :: GenParser Char st (String, [(Int, String)])
bagEntry =
    do result <- manyTill anyChar (try $ string " bags contain ")
       x <- choice [emptyBagContent, many1 bagContentEntry]
       eol
       return $ (result, x)

emptyBagContent :: GenParser Char st [(Int, String)]
emptyBagContent =
    do string "no other bags."
       return []

bagContentEntry :: GenParser Char st (Int, String)
bagContentEntry =
    do count <- many1 digit
       char ' '
       x <- manyTill anyChar (try $ string " bag")
       optional $ char 's'
       oneOf ",."
       optional $ char ' '
       return (read count, x)

parseBags :: String -> Either ParseError [(String, [(Int, String)])]
parseBags =  parse bagsFile "(unknown)"