import CommonMapFunctions
-- import Kodable
import Control.Applicative
import Control.Monad
import Control.Monad.State
import System.IO  
import Data.Char
import Data.List
import Text.Printf

isSolvable :: [String] -> Bool
isSolvable maze = solveMaze maze (head $ currentPositionOfCharacters maze '@') (currentPositionOfCharacters maze '@') "Right"

solveMaze :: [String] -> (Int,Int) -> [(Int, Int)] -> String -> Bool
solveMaze maze (ballX, ballY) visited currentDirection
    | ballPosition == 't' = True
    | ballPosition /= 't' && possibleDirections == [] = False
    | ballPosition /= 't' && possibleDirections /= [] = any (True ==) (map (\direction -> solveMaze maze (newPositionOfBall (ballX, ballY) direction) newVisited direction)possibleDirections)
    where
        ballPosition = (maze !! ballX) !! ballY
        (ballXNew, ballYNew) = newPositionOfBall (ballX, ballY) currentDirection
        newVisited = visited ++ map (\direction -> newPositionOfBall (ballX, ballY) direction) possibleDirections
        possibleDirections = getPossibleDirections maze (ballX, ballY) visited currentDirection

getPossibleDirections :: [String] -> (Int, Int) -> [(Int, Int)] -> String -> [String]
getPossibleDirections maze (ballX, ballY) visited currentDirection
    | (maze !! ballX) !! ballY == '@' = filter (/= "") [moveUp, moveDown, moveRight, moveLeft]
    | (maze !! ballX) !! ballY `elem` ['p', 'o', 'y'] = filter (/= "") [moveUp, moveDown, moveRight, moveLeft]
    | ballXNew >=0 && (ballXNew < fst(dimensionOfMaze maze)) && (ballYNew < snd(dimensionOfMaze maze)) && (ballYNew >= 0) && elem newCharOfBall ['-', 'b', 't', 'p', 'o', 'y'] = [currentDirection]
    | otherwise = filter (/= "") [moveUp, moveDown, moveRight, moveLeft]
    where
        (ballXNew, ballYNew) = newPositionOfBall (ballX, ballY) currentDirection
        newCharOfBall =  (maze !! ballXNew) !! ballYNew
        moveUp = upPossible maze (ballX, ballY) visited 
        moveDown = downPossible maze (ballX, ballY) visited 
        moveRight = rightPossible maze (ballX, ballY) visited 
        moveLeft = leftPossible maze (ballX, ballY) visited 

notInVisited :: (Int, Int) -> [(Int, Int)] -> Bool
notInVisited (ballX, ballY) visited = not (elem (ballX, ballY) visited)

upPossible :: [String] -> (Int, Int) -> [(Int, Int)] -> String
upPossible maze (ballX, ballY) visited 
    | (ballXNew >= 0) && nextBallPosition /= '*' && (notInVisited (ballXNew, ballYNew) visited) = "Up"
    | otherwise = ""
    where 
        (ballXNew, ballYNew) = newPositionOfBall (ballX, ballY) "Up"
        nextBallPosition = (maze !! ballXNew) !! ballYNew

downPossible :: [String] -> (Int, Int) -> [(Int, Int)] -> String
downPossible maze (ballX, ballY) visited 
    |  (ballXNew < fst(dimensionOfMaze maze)) && nextBallPosition /= '*' && (notInVisited (ballXNew, ballYNew) visited) = "Down"
    | otherwise = ""
    where 
        (ballXNew, ballYNew) = newPositionOfBall (ballX, ballY) "Down"
        nextBallPosition = (maze !! ballXNew) !! ballYNew

rightPossible :: [String] -> (Int, Int) -> [(Int, Int)] -> String
rightPossible maze (ballX, ballY) visited
    | (ballYNew < snd(dimensionOfMaze maze)) && nextBallPosition /= '*' && (notInVisited (ballXNew, ballYNew) visited) = "Right"
    | otherwise = ""
    where 
        (ballXNew, ballYNew) = newPositionOfBall (ballX, ballY) "Right"
        nextBallPosition = (maze !! ballXNew) !! ballYNew

leftPossible :: [String] -> (Int, Int) -> [(Int, Int)] -> String
leftPossible maze (ballX, ballY) visited 
    | (ballYNew >= 0) &&  nextBallPosition /= '*' && (notInVisited (ballXNew, ballYNew) visited) = "Left"
    | otherwise = ""
    where 
        (ballXNew, ballYNew) = newPositionOfBall (ballX, ballY) "Left"
        nextBallPosition = (maze !! ballXNew) !! ballYNew
