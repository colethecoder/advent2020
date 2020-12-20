module Advent12
    ( advent12
    ) where

import System.IO ()
import Data.List ( sort, nub, intersect )
import Text.ParserCombinators.Parsec
import Paths_advent
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

advent12 :: IO ()
advent12 = do  
    filepath <- getDataFileName "input12.txt"
    contents <- readFile filepath
    let parsed = reverse <$> parseInstructions contents
    print $ case parsed of
        Left err -> show err
        Right r  -> show $ foldr followInstruction (ShipState East (0,0)) r
    print $ case parsed of
        Left err -> show err
        Right r  -> show $ foldr followWaypointInstruction (WaypointShipState (10,-1) (0,0)) r

followInstruction :: Instruction -> ShipState -> ShipState
followInstruction (Shift d i) state = ShipState (bearing state) (moveDirection d i $ position state)
followInstruction (Rotate d i) state = ShipState (rotate (bearing state) d i) (position state)
followInstruction (Forward i) state = ShipState (bearing state) (moveDirection (bearing state) i $ position state)

followWaypointInstruction :: Instruction -> WaypointShipState -> WaypointShipState
followWaypointInstruction (Shift d i) state = WaypointShipState (moveDirection d i $ waypointPosition state) (shipPosition state)
followWaypointInstruction (Rotate d i) state = WaypointShipState (rotateWaypoint d i $ waypointPosition state) (shipPosition state)
followWaypointInstruction (Forward i) state = 
    let (initX, initY) =  waypointPosition state
        (changeX, changeY) = (initX*i, initY*i)
        (shipX, shipY) = shipPosition state
        newPosition = (shipX+changeX, shipY+changeY) in
    WaypointShipState (initX, initY) newPosition

moveDirection :: Direction -> Int -> Coord -> Coord
moveDirection North i (x,y) = (x, y-i)
moveDirection East  i (x,y) = (x+i, y)
moveDirection South i (x,y) = (x, y+i)
moveDirection West  i (x,y) = (x-i, y)

rotate :: Direction -> Rotation -> Int -> Direction
rotate d _ 0 = d
rotate North Clockwise i = rotate East  Clockwise $ i-90
rotate East  Clockwise i = rotate South Clockwise $ i-90
rotate South Clockwise i = rotate West  Clockwise $ i-90
rotate West  Clockwise i = rotate North Clockwise $ i-90
rotate North AntiClockwise i = rotate West  AntiClockwise $ i-90
rotate East  AntiClockwise i = rotate North AntiClockwise $ i-90
rotate South AntiClockwise i = rotate East  AntiClockwise $ i-90
rotate West  AntiClockwise i = rotate South AntiClockwise $ i-90

rotateWaypoint :: Rotation -> Int -> Coord -> Coord
rotateWaypoint _ 0 c = c
rotateWaypoint Clockwise i (x,y) = rotateWaypoint Clockwise (i-90) (-y,x)
rotateWaypoint AntiClockwise i (x,y) = rotateWaypoint AntiClockwise (i-90) (y,-x)

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
    do a <- oneOf "NESWRLF"
       i <- int
       eol
       return $ instructionFromChar a i

parseInstructions :: String -> Either ParseError [Instruction]
parseInstructions =  parse instructionFile "(unknown)"

instructionFromChar :: Char -> Int -> Instruction
instructionFromChar 'N' i = Shift North i
instructionFromChar 'E' i = Shift East i
instructionFromChar 'S' i = Shift South i
instructionFromChar 'W' i = Shift West i
instructionFromChar 'R' i = Rotate Clockwise i
instructionFromChar 'L' i = Rotate AntiClockwise i
instructionFromChar _   i = Forward i

type Coord = (Int, Int)

data WaypointShipState = WaypointShipState {
    waypointPosition :: Coord
  , shipPosition     :: Coord
} deriving Show

data ShipState = ShipState {
    bearing          :: Direction
  , position         :: Coord
} deriving Show

data Direction =
    North
  | South
  | East
  | West
  deriving Show

data Rotation =
    Clockwise
  | AntiClockwise
  deriving Show

data Instruction =
    Shift Direction Int
  | Rotate Rotation Int
  | Forward Int
  deriving Show