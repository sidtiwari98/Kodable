module MazeSolver (
    isSolvable,
    solveMaze
) where
import CommonMapFunctions
import Control.Applicative
import Control.Monad
import Control.Monad.State
import System.IO  
import Data.Char
import Data.List
import Text.Printf

-- takes a map , calcualtes the current pos of ball and calls the solveMaze funciton
isSolvable :: [String] -> Bool
isSolvable maze = solveMaze maze (head $ currentPositionOfCharacters maze '@') (currentPositionOfCharacters maze '@') "Right" 't'


-- It takes a map, (xCoordinateOfBall, yCoordinateOfBall), a list of visited nodes, the current direction where the ball is going.
-- This is a recursive function which call itself until and unless the ball has reached target ‘t’ or the ball has not reached ‘t’ but has no possible moves. 
-- In the case of the former, it returns true and false if the later condition is satisfied.
-- If none of the above condition is satisfied, it then explores various paths the ball can travel to using the possible moves at a given node where the ball stops to change direction. 
-- This is done through the recursive call of solveMaze with different directions in the currentDirection parameter and different positions of the ball.
-- A visited list is kept to prevent the ball from retracing already traversed path. If any of the path returns True, solveMaze returns true
solveMaze :: [String] -> (Int,Int) -> [(Int, Int)] -> String -> Char -> Bool
solveMaze maze (ballX, ballY) visited currentDirection target
    | ballPosition == target = True
    | ballPosition /= target && possibleDirections == [] = False
    | ballPosition /= target && possibleDirections /= [] = any (True ==) (map (\direction -> solveMaze maze (newPositionOfBall (ballX, ballY) direction) newVisited direction target)possibleDirections)
    where
        ballPosition = (maze !! ballX) !! ballY
        (ballXNew, ballYNew) = newPositionOfBall (ballX, ballY) currentDirection
        newVisited = visited ++ map (\direction -> newPositionOfBall (ballX, ballY) direction) possibleDirections
        possibleDirections = getPossibleDirections maze (ballX, ballY) visited currentDirection

-- Takes a map, a position, visited nodes and currentDirection to give all possible moves the ball can go to from that point
-- returns the list of all possible directions a ball can traverse to from a given given position. 
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

-- Checks if a prospective node where the ball can travel to is already visited or not
notInVisited :: (Int, Int) -> [(Int, Int)] -> Bool
notInVisited (ballX, ballY) visited = not (elem (ballX, ballY) visited)

-- Checks if going up from a give node is possible
upPossible :: [String] -> (Int, Int) -> [(Int, Int)] -> String
upPossible maze (ballX, ballY) visited 
    | (ballXNew >= 0) && nextBallPosition /= '*' && (notInVisited (ballXNew, ballYNew) visited) = "Up"
    | otherwise = ""
    where 
        (ballXNew, ballYNew) = newPositionOfBall (ballX, ballY) "Up"
        nextBallPosition = (maze !! ballXNew) !! ballYNew

-- Checks if going down from a give node is possible
downPossible :: [String] -> (Int, Int) -> [(Int, Int)] -> String
downPossible maze (ballX, ballY) visited 
    |  (ballXNew < fst(dimensionOfMaze maze)) && nextBallPosition /= '*' && (notInVisited (ballXNew, ballYNew) visited) = "Down"
    | otherwise = ""
    where 
        (ballXNew, ballYNew) = newPositionOfBall (ballX, ballY) "Down"
        nextBallPosition = (maze !! ballXNew) !! ballYNew

-- Checks if going right from a give node is possible
rightPossible :: [String] -> (Int, Int) -> [(Int, Int)] -> String
rightPossible maze (ballX, ballY) visited
    | (ballYNew < snd(dimensionOfMaze maze)) && nextBallPosition /= '*' && (notInVisited (ballXNew, ballYNew) visited) = "Right"
    | otherwise = ""
    where 
        (ballXNew, ballYNew) = newPositionOfBall (ballX, ballY) "Right"
        nextBallPosition = (maze !! ballXNew) !! ballYNew

-- Checks if going left from a give node is possible
leftPossible :: [String] -> (Int, Int) -> [(Int, Int)] -> String
leftPossible maze (ballX, ballY) visited 
    | (ballYNew >= 0) &&  nextBallPosition /= '*' && (notInVisited (ballXNew, ballYNew) visited) = "Left"
    | otherwise = ""
    where 
        (ballXNew, ballYNew) = newPositionOfBall (ballX, ballY) "Left"
        nextBallPosition = (maze !! ballXNew) !! ballYNew
