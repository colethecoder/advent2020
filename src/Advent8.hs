module Advent8
    ( advent8
    ) where

import System.IO ()
import Data.List ( sort, nub, intersect )
import Text.ParserCombinators.Parsec
import Paths_advent
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

advent8 :: IO ()
advent8 = do  
    filepath <- getDataFileName "input7.txt"
    contents <- readFile filepath   
    print $ case parseBags contents of
        Left err -> show err
        Right r -> show $ foldr f [] r

f :: (String, [(Int, String)]) -> [String] -> [String]
f (key, val) acc = if g val then key : acc else acc

g :: [(Int,String)] -> Bool
g [] = False
g ((_, xName):xs) = if xName == "shiny gold" then True else g xs

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