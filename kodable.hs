import Control.Applicative
import Control.Monad
import Control.Monad.State
import System.IO  
import Data.Char
import Data.List

import Text.Printf



getDirections :: Int -> [String] -> IO [String]
getDirections x directions= do
    if x == -1
        then
            putStr "First direction: "
        else
            putStr "Next direction: "
    direction <- getLine
    if direction == ""
        then 
            return directions
        else
            do
                getDirections (x+1) (directions ++ [direction]) 


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
        putStrLn "Enter play to begin game"
        nextCommand <- getLine
        if (nextCommand == "play") 
                then do 
                        directions <- getDirections (-1) []
                        play map directions '-'
        else
                do
                        putStrLn "Invalid command, start from beginning"
                        load    


play :: [String] -> [String] -> Char -> IO()
play _ [] _ = return ()
play map (d:ds) charOnWhichBallIsSitting = 
        do
                let updatedMapAfterOneMove = makeMove map d charOnWhichBallIsSitting
                showMap updatedMapAfterOneMove
                putStrLn " "
                play updatedMapAfterOneMove ds '-'


makeMove :: [String] -> String -> Char -> [String]
makeMove map direction charOnWhichBallIsSitting
        | direction == "Right" = take ballRow map ++ [moveBallRight (map !! ballRow) charOnWhichBallIsSitting ballColumn] ++  drop(ballRow + 1) map
        | direction == "Left" = take ballRow map ++ [(moveBallLeft (map !! ballRow) charOnWhichBallIsSitting ballColumn)] ++  drop(ballRow + 1) map
        | direction == "Up" = moveBallUp map charOnWhichBallIsSitting (ballRow, ballColumn)
        | direction == "Down" = moveBallDown map charOnWhichBallIsSitting (ballRow, ballColumn)
        where
                positionOfBall = currentPositionOfCharacters map '@'
                ballRow = fst(head(positionOfBall))
                ballColumn = snd(head(positionOfBall))



moveBallUp ::  [String] -> Char -> (Int, Int) -> [String]
moveBallUp map charOnWhichBallIsSitting (ballRow, ballColumn) = 
        if ((ballRow > 0) && nextChar == '-')
                then moveBallUp (moveBallUpOnce map charOnWhichBallIsSitting (ballRow, ballColumn)) nextChar (ballRow -1, ballColumn)
        else map
        where 
                nextChar = (map !! (ballRow-1)) !! ballColumn


moveBallUpOnce :: [String] -> Char -> (Int, Int) -> [String]
moveBallUpOnce map charOnWhichBallIsSitting (ballRow, ballColumn) = 
        take (ballRow -1) map ++ rowUp ++ rowCurrent ++ drop (ballRow +1) map
        where 
                rowUp = [take ballColumn (map !! (ballRow -1)) ++ ['@'] ++ drop (ballColumn + 1) (map !! (ballRow -1))]
                rowCurrent =[take ballColumn (map !! ballRow ) ++ ['-'] ++ drop (ballColumn + 1) (map !! ballRow)]

moveBallDown ::  [String] -> Char -> (Int, Int) -> [String]
moveBallDown map charOnWhichBallIsSitting (ballRow, ballColumn) = 
        if ((ballRow < length map) && nextChar == '-')
                then moveBallDown (moveBallDownOnce map charOnWhichBallIsSitting (ballRow, ballColumn)) nextChar (ballRow +1, ballColumn)
        else map
        where 
                nextChar = (map !! (ballRow+1)) !! ballColumn


moveBallDownOnce :: [String] -> Char -> (Int, Int) -> [String]
moveBallDownOnce map charOnWhichBallIsSitting (ballRow, ballColumn) = 
        take (ballRow) map ++ rowCurrent ++ rowDown ++ drop (ballRow +2) map
        where 
                rowDown = [take ballColumn (map !! (ballRow +1)) ++ ['@'] ++ drop (ballColumn + 1) (map !! (ballRow +1))]
                rowCurrent =[take ballColumn (map !! ballRow) ++ ['-'] ++ drop (ballColumn + 1) (map !! ballRow)]

moveBallRight :: String -> Char -> Int -> String
moveBallRight s charOnWhichBallWasSitting ballposition = 
        if ((ballposition + 2 < length s) && (s !! (ballposition + 2) == '-'))
        then moveBallRight (moveBallRightOnce s charOnWhichBallWasSitting ballposition)  (s !! (ballposition + 2)) (ballposition + 2)
        else s

moveBallRightOnce :: String -> Char -> Int -> String
moveBallRightOnce s charOnWhichBallWasSitting ballposition = 
        take ballposition s ++ [charOnWhichBallWasSitting] ++ [' '] ++ ['@'] ++ [' '] ++ drop (ballposition + 4) s


moveBallLeft :: String -> Char -> Int -> String
moveBallLeft s charOnWhichBallWasSitting ballposition = 
        if ((ballposition - 2 >= 0) && (s !! (ballposition - 2) == '-'))
        then moveBallLeft (moveBallLeftOnce s charOnWhichBallWasSitting ballposition)  (s !! (ballposition - 2)) (ballposition - 2)
        else s

moveBallLeftOnce :: String -> Char -> Int -> String
moveBallLeftOnce s charOnWhichBallWasSitting ballposition = 
        take (ballposition - 2) s ++ ['@'] ++ [' '] ++ [charOnWhichBallWasSitting] ++ [' '] ++ drop (ballposition +2 ) s

currentPositionOfCharacters :: [String] -> Char -> [(Int,Int)]
currentPositionOfCharacters map c = [ (x,y) | (mapElement, x) <- zip map [0..], y <- elemIndices c mapElement, elem c mapElement]

showMap :: [String] -> IO()
showMap [] = return ()
showMap (l:ls) = do
    putStrLn l
    showMap ls

            