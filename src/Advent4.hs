module Advent4
    ( advent4
    ) where

import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Text.ParserCombinators.Parsec
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Paths_advent

advent4 :: IO ()
advent4 = do  
        filepath <- getDataFileName "input4.txt"
        contents <- readFile filepath
        let list = parsePassports contents
        print list

eol :: GenParser Char st Char
eol = char '\n'

passportFile :: GenParser Char st [Map String String]
passportFile = 
    do result <- many1 passportBlock
       eof
       return result

passportBlock :: GenParser Char st (Map String String)
passportBlock = 
    do entries <- passportEntries
       optional eol
       return entries

passportEntries :: GenParser Char st (Map String String)
passportEntries =
    do result <- many1 passportEntry
       return $ Map.fromList result

passportEntry :: GenParser Char st (String, String)
passportEntry = do
    tag <- many1 letter
    char ':'
    val <- many1 (noneOf "\n ")
    oneOf "\n "
    return (tag, val)

parsePassports :: String -> Either ParseError [Map String String]
parsePassports input =  parse passportFile "(unknown)" input