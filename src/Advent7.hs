module Advent7
    ( advent7
    ) where

import System.IO ()
import Data.List ( sort, nub, intersect )
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
        Right r -> show r

eol :: GenParser Char st Char
eol = char '\n'

bagsFile :: GenParser Char () (Map String [(Int, String)])
bagsFile = 
    do result <- many1 bagEntry
       eof
       return $ Map.fromList result

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
       x <- many1 (noneOf ",.")
       oneOf ",."
       optional $ char ' '
       return $ (read count, x)

parseBags :: String -> Either ParseError (Map String [(Int, String)])
parseBags =  parse bagsFile "(unknown)"