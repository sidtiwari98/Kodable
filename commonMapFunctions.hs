module CommonMapFunctions (
    currentPositionOfCharacters,
    newPositionOfBall,
    dimensionOfMaze,
    showMap
)where 
import Control.Applicative
import Control.Monad
import Control.Monad.State
import System.IO  
import Data.Char
import Data.List
import Text.Printf
import Data.Ord

currentPositionOfCharacters :: [String] -> Char -> [(Int,Int)]
currentPositionOfCharacters map c = [ (x,y) | (mapElement, x) <- zip map [0..], y <- elemIndices c mapElement, elem c mapElement]

newPositionOfBall :: (Int, Int) -> String -> (Int, Int)
newPositionOfBall (ballX, ballY) "Up" = (ballX - 1, ballY)
newPositionOfBall (ballX, ballY) "Right" = (ballX, ballY + 2)
newPositionOfBall (ballX, ballY) "Down" = (ballX + 1, ballY)
newPositionOfBall (ballX, ballY) "Left" = (ballX, ballY - 2)

dimensionOfMaze :: [String] -> (Int, Int)
dimensionOfMaze maze = (length maze,  length $ head maze)

showMap :: [String] -> IO()
showMap [] = return ()
showMap (l:ls) = do
    putStrLn l
    showMap ls
