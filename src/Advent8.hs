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
    print $ case parseInstructions contents of
        Left err -> show err
        Right r  -> show $ f r (BootState 0 0 [])
    print $ case parseInstructions contents of
        Left err -> show err
        Right r  -> show $ i (j 0 r) (BootState 0 0 [])

j :: Int -> [Instruction] -> [[Instruction]]
j pos ins =
    if length ins == pos
    then []
    else 
        if (isACC (ins !! pos))
        then (j (pos +1) ins)
        else (modifyNth pos switchNOPandJMP ins) : (j (pos +1) ins)

isACC :: Instruction -> Bool
isACC (ACC x) = True
isACC _ = False

switchNOPandJMP :: Instruction -> Instruction
switchNOPandJMP (NOP x) = JMP x
switchNOPandJMP (JMP x) = NOP x
switchNOPandJMP (ACC x) = ACC x

i :: [[Instruction]] -> BootState -> Maybe Int
i [] st = Nothing
i (x:xs) st =
    case g x st of
        Just j -> Just j
        Nothing -> i xs st

g :: [Instruction] -> BootState -> Maybe Int
g ins st = 
    if any (\x -> x == (position st)) $ visitedPositions st
    then Nothing    
    else 
        if (length ins) == (position st)
        then Just $ accumulator st
        else g ins $ moveNext (ins !! (position st)) st

f :: [Instruction] -> BootState -> Int
f ins st = 
    if any (\x -> x == (position st)) $ visitedPositions st
    then accumulator st
    else f ins $ moveNext (ins !! (position st)) st

modifyNth :: Int -> (a -> a) -> [a] -> [a]
modifyNth _ _ [] = []
modifyNth n calc (x:xs)
   | n == 0 = (calc x):xs
   | otherwise = x : modifyNth (n-1) calc xs

moveNext :: Instruction -> BootState -> BootState
moveNext (NOP i) st = BootState (accumulator st) ((position st) + 1) ((position st) : (visitedPositions st))
moveNext (JMP i) st = BootState (accumulator st) ((position st) + i) ((position st) : (visitedPositions st))
moveNext (ACC i) st = BootState ((accumulator st) + i) ((position st) + 1) ((position st) : (visitedPositions st))

data BootState = BootState {
    accumulator      :: Int
  , position         :: Int
  , visitedPositions :: [Int] 
} deriving Show

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