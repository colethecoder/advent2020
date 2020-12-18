{-# language TypeFamilies #-}
module Advent11
  ( advent11) where

import Data.Functor.Compose (Compose(..))
import Data.Vector (Vector, (!), generate, fromList)
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Distributive (Distributive(..))
import Data.Functor.Rep (Representable(..), distributeRep)
import Data.Functor.Identity (Identity(..))
import Control.Comonad.Representable.Store (Store(..), StoreT(..), store, experiment)
import Control.Comonad (Comonad(..))
import Control.Concurrent
import Data.List.Split
import Paths_advent

advent11 :: IO ()
advent11 = do  
    filepath <- getDataFileName "input11.txt"
    contents <- readFile filepath
    let lines = splitOn "\n" contents
        gridish = map (map seatsFromChar) $ splitOn "\n" contents
        vgrid = fromList $ map fromList gridish 
        grid = mkGrid lines
    putStrLn $ show lines
    --print $ length vgrid
    --print $ length $ vgrid ! 0
    print $ length gridish
    print $ length $ head gridish  
    putStrLn $ render grid
    putStrLn $ show $ queryGridArray lines (0,0)
    putStrLn $ show $ queryGridArray lines (0,14)
    putStrLn $ show $ (lines !! 0) !! 0
    putStrLn $ show $ (lines !! 0) !! 14
    untilNoChanges grid

untilNoChanges :: Grid -> IO ()
untilNoChanges grid =
    let newGrid = step basicRule grid 
        rendered = render newGrid in
    if compareGrids grid newGrid
    then do
        putStrLn rendered 
        print $ length $ filter (== '#') rendered
    else do 
        putStrLn rendered
        --threadDelay 1000
        untilNoChanges newGrid

type Coord = (Int, Int)
type Grid = Store (Compose Vector Vector) SeatStatus
type Rule = Grid -> SeatStatus

data SeatStatus = 
    Occupied 
  | Empty
  | Floor
  deriving (Eq, Show)

renderSeats :: SeatStatus -> String
renderSeats Occupied = "#"
renderSeats Empty    = "L"
renderSeats Floor    = "."

seatsFromChar :: Char -> SeatStatus
seatsFromChar '#' = Occupied
seatsFromChar 'L' = Empty
seatsFromChar '.' = Floor

instance Distributive Vector where
  distribute = distributeRep

instance Representable Vector where
  type Rep Vector = Int
  index v i = v ! i
  tabulate = generate gridSize

gridSize :: Int
gridSize = 92

neighbourCoords :: [Coord]
neighbourCoords = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]

addCoords :: Coord -> Coord -> Coord
addCoords (x, y) (x', y') = (x + x', y + y')

basicRule :: Rule
basicRule g = 
    case status of
        Floor -> Floor
        Occupied -> if numNeighboursOccupied > 3 then Empty else Occupied
        Empty -> if numNeighboursOccupied == 0 then Occupied else Empty
  where
    status = extract g
    neighbours = experiment (coordsInGrid gridSize neighbourCoords) g
    numNeighboursOccupied = length (filter (== Occupied) neighbours)

step :: Rule -> Grid -> Grid
step = extend

compareGrids :: Grid -> Grid -> Bool
compareGrids (StoreT (Identity (Compose x)) _) (StoreT (Identity (Compose y)) _) = x == y

render :: Grid -> String
render (StoreT (Identity (Compose g)) _) = foldMap ((++ "\n") . foldMap renderSeats) g

mkGrid :: [[Char]] -> Grid
mkGrid statuses = store (queryGridArray statuses) (0, 0)

queryGridArray :: [[Char]] -> Coord -> SeatStatus
queryGridArray statuses (x,y) = 
    maybe Floor seatsFromChar $ do
    row <- statuses !!? x
    row !!? y

infix 9 !!?
(!!?) :: [a] -> Int -> Maybe a
(!!?) xs i
    | i < 0     = Nothing
    | otherwise = go i xs
  where
    go :: Int -> [a] -> Maybe a
    go 0 (x:_)  = Just x
    go j (_:ys) = go (j - 1) ys
    go _ []     = Nothing

coordsInGrid :: Int -> [Coord] -> Coord -> [Coord]
coordsInGrid gridSize coords origin =
    filter (\(x,y) -> x < gridSize && y < gridSize && x >= 0 && y >= 0) (at coords origin)

at :: [Coord] -> Coord -> [Coord]
coords `at` origin = map (addCoords origin) coords