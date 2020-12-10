module Advent6
    ( advent6
    ) where

import System.IO ()
import Data.List ( sort ) 
import Paths_advent

advent6 :: IO ()
advent6 = do  
    filepath <- getDataFileName "input6.txt"
    contents <- readFile filepath    
    print contents