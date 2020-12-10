module Advent6
    ( advent6
    ) where

import System.IO ()
import Data.List ( sort, nub, intersect )
import Text.ParserCombinators.Parsec
import Paths_advent

advent6 :: IO ()
advent6 = do  
    filepath <- getDataFileName "input6.txt"
    contents <- readFile filepath    
    print $ case parseCustoms contents of
        Left err -> show err
        Right r  -> show $ sum $ map length $ map nub $ map concat r
    print $ case parseCustoms contents of
        Left err -> show err
        Right r  -> show $ sum $ map length $ map nub $ map intersection r

eol :: GenParser Char st Char
eol = char '\n'

customsFile :: GenParser Char () [[[Char]]]
customsFile = 
    do result <- many1 customsBlock
       eof
       return result

customsBlock :: GenParser Char st [[Char]]
customsBlock = 
    do entries <- customsEntries
       optional eol
       return entries

customsEntries :: GenParser Char st [[Char]]
customsEntries =
    do result <- many1 customsEntry
       return $ result

customsEntry :: GenParser Char st [Char]
customsEntry = do
    cs <- many1 letter
    eol
    return cs

parseCustoms :: String -> Either ParseError [[[Char]]]
parseCustoms =  parse customsFile "(unknown)"

intersection:: (Eq a) => [[a]] -> [a]
intersection = foldr1 intersect