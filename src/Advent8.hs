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
    filepath <- getDataFileName "input8.txt"
    contents <- readFile filepath   
    print $ parseInstructions contents

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

instructionFile :: GenParser Char () [Instruction]
instructionFile = 
    do result <- many1 instructionEntry
       eof
       return result

instructionEntry :: GenParser Char st Instruction
instructionEntry =
    do x <- choice [nop, jmp, acc]
       eol
       return x

nop :: GenParser Char st Instruction
nop =
    do string "nop "
       i <- int
       return $ NOP i

jmp :: GenParser Char st Instruction
jmp =
    do string "jmp "
       i <- int
       return $ JMP i

acc :: GenParser Char st Instruction
acc =
    do string "acc "
       i <- int
       return $ ACC i

parseInstructions :: String -> Either ParseError [Instruction]
parseInstructions =  parse instructionFile "(unknown)"

data Instruction =
    NOP Int
  | JMP Int
  | ACC Int
  deriving Show