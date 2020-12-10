module Advent4
    ( advent4
    ) where

import Control.Monad (replicateM_)
import System.IO ()
import Text.ParserCombinators.Parsec
import Text.Read ( readMaybe )
import Data.Map.Strict (Map)
import Data.Validation (toEither,  Validation(..) )
import Data.Either ( partitionEithers )
import qualified Data.Map.Strict as Map
import Paths_advent

advent4 :: IO ()
advent4 = do  
        filepath <- getDataFileName "input4.txt"
        contents <- readFile filepath
        print $ case parsePassports contents of
            Left err ->  show err
            Right pps -> show $ length $ 
                successfulPassportValidations $ validatePassports1 pps
        print $ case parsePassports contents of
            Left err ->  show err
            Right pps -> show $ length $ successfulPassportValidations $ validatePassports2 $
                successfulPassportValidations $ validatePassports1 pps

eol :: GenParser Char st Char
eol = char '\n'

passportFile :: GenParser Char () [Map String String]
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
parsePassports =  parse passportFile "(unknown)"

validatePassports1 :: [Map String String] -> [PassportValidation Passport]
validatePassports1 = fmap validatePassport1

validatePassport1 :: Map String String -> PassportValidation Passport
validatePassport1 input =
    Passport <$>
    validateItem "byr" input <*>
    validateItem "iyr" input <*>
    validateItem "eyr" input <*>
    validateItem "hgt" input <*>
    validateItem "hcl" input <*>
    validateItem "ecl" input <*>
    validateItem "pid" input <*>
    getOptionalItem "cid" input

validatePassports2 :: [Passport] -> [PassportValidation ValidPassport]
validatePassports2 = fmap validatePassport2

validatePassport2 :: Passport -> PassportValidation ValidPassport
validatePassport2 input =
    ValidPassport <$>
    validateYear (birthyear input) 1920 2002 <*>
    validateYear (issueyear input) 2010 2020 <*>
    validateYear (expireyear input) 2020 2030 <*>
    validateHeight (height input) <*>
    validateHair (haircolour input) <*>
    validateEye (eyecolour input) <*>
    validatePassportNo (passportid input) <*>
    Success (countryid input)

validatePassportNo :: String -> PassportValidation String
validatePassportNo input = 
    case asNumeric input of
        Just _  -> if length input == 9 then Success input else Failure [InvalidData]
        Nothing -> Failure [InvalidData]

asNumeric :: String -> Maybe Int
asNumeric = readMaybe

validateHair :: String -> PassportValidation String
validateHair input = 
    case parse parseHair "(unknown)" input of
        Right _ -> Success input
        Left _  -> Failure [InvalidHair]

parseHair :: GenParser Char st ()
parseHair = do
    char '#'
    val <- replicateM_ 6 hexDigit
    eof
    return val

validateEye :: String -> PassportValidation String
validateEye input =
    if 1 == length (filter (\x -> x == input) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
    then Success input
    else Failure [InvalidEye]

parseEye :: GenParser Char st String
parseEye = do
    val  <- choice $ map string ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    eof
    return val

validateYear :: String -> Int -> Int -> PassportValidation Int
validateYear input min max =    
    case readMaybe input of
        Just a -> validateNumberRange min max a
        Nothing -> Failure [InvalidYear]

validateNumberRange :: Int -> Int -> Int -> PassportValidation Int
validateNumberRange min max val =
    if val >= min && val <= max
    then Success val
    else Failure [InvalidYear]

validateHeight :: String -> PassportValidation (Int, MeasureUnits)
validateHeight input = 
    case parse parseHeight "(unknown)" input of
        Left _ -> Failure [InvalidHeight]
        Right (val, unit) -> 
            case unit of
                Centimetres -> if val >= 150 && val <= 193 then Success (val, unit) else Failure [InvalidHeight]
                Inches      -> if val >= 59 && val <= 76 then Success (val, unit) else Failure [InvalidHeight]

parseHeight :: GenParser Char st (Int, MeasureUnits)
parseHeight = do
    val  <- many1 digit
    unit <- choice [parseCentimetres, parseInches]
    eof
    return (read val, unit)

parseCentimetres :: GenParser Char st MeasureUnits
parseCentimetres = do
    string "cm"
    return Centimetres

parseInches :: GenParser Char st MeasureUnits
parseInches = do
    string "in"
    return Inches

validateItem :: String -> Map String String -> PassportValidation String
validateItem label input = case Map.lookup label input of 
                            Just a -> Success a
                            Nothing -> Failure [MissingField]

getOptionalItem :: String -> Map String String -> PassportValidation (Maybe String)
getOptionalItem label input = Success $ Map.lookup label input

successfulPassportValidations :: [PassportValidation a] -> [a]
successfulPassportValidations valids = snd $ partitionEithers $ map toEither valids

type PassportValidation = Validation [Error]

data MeasureUnits =
    Centimetres
  | Inches
  deriving Show

data Error = 
    MissingField
  | InvalidData
  | NotNumeric
  | InvalidHair
  | InvalidEye
  | InvalidPassport
  | InvalidYear
  | InvalidHeight
  deriving Show

data Passport = Passport {
    birthyear  :: String
  , issueyear  :: String
  , expireyear :: String
  , height     :: String
  , haircolour :: String
  , eyecolour  :: String
  , passportid :: String
  , countryid  :: Maybe String
} deriving Show

data ValidPassport = ValidPassport {
    vbirthyear  :: Int
  , vissueyear  :: Int
  , vexpireyear :: Int
  , vheight     :: (Int, MeasureUnits)
  , vhaircolour :: String
  , veyecolour  :: String
  , vpassportid :: String
  , vcountryid  :: Maybe String
} deriving Show