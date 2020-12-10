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
    print $ contents

--eol :: GenParser Char st Char
--eol = char '\n'
--
--bagsFile :: GenParser Char () [[[Char]]]
--customsFile = 
--    do result <- many1 customsBlock
--       eof
--       return result
--
--customsBlock :: GenParser Char st [[Char]]
--customsBlock = 
--    do entries <- customsEntries
--       optional eol
--       return entries
--
--customsEntries :: GenParser Char st [[Char]]
--customsEntries =
--    do result <- many1 customsEntry
--       return $ result
--
--customsEntry :: GenParser Char st [Char]
--customsEntry = do
--    cs <- many1 letter
--    eol
--    return cs
--
--parseBags :: String -> Either ParseError (Map String [(Int, String)])
--parseBags =  parse customsFile "(unknown)"