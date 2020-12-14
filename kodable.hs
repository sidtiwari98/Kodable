import Control.Applicative
import Control.Monad
import Control.Monad.State
import System.IO  
import Data.Char
import Data.List

import Text.Printf



-- implement error handling for file doesnt exist
load :: IO()
load = do
        putStrLn "Enter the file name you want to load"
        xs <- getLine
        putStrLn "Loading your file..."
        textContents <- readFile xs
        let map = lines textContents
        showMap map
        putStrLn "File loaded"
        -- play map

-- implement error handling for empty list if @ doesnt exist
play :: [String] -> [String]
play map = undefined


currentPositionOfSpecialCharacters :: [String] -> Char -> [(Int,Int)]
currentPositionOfSpecialCharacters map c = [ (x,y) | (mapElement, x) <- zip map [0..], y <- elemIndices c mapElement, elem c mapElement]

showMap :: [String] -> IO()
showMap s = print $ s

            