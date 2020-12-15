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
                let updatedMap = makeMove map d charOnWhichBallIsSitting
                if (map == updatedMap)
                        then do
                                putStr "Sorry, error: cannot move to the "
                                putStrLn d
                                putStrLn "Your current Board :"
                                showMap updatedMap
                else do
                        showMap updatedMap   
                        let bonusEarned = 3 - length (currentPositionOfCharacters updatedMap 'b')
                        let prevBonus = 3 - length (currentPositionOfCharacters map 'b')
                        if (bonusEarned /= prevBonus)
                                then do
                                        putStr "Got "
                                        putStr $ show (bonusEarned)
                                        putStrLn " bonus"
                                        putStrLn " "  
                        else
                                putStrLn " "  
                        play updatedMap ds '-'


makeMove :: [String] -> String -> Char -> [String]
makeMove map direction charOnWhichBallIsSitting
        | direction == "r" = take ballRow map ++ [moveBallRight (map !! ballRow) charOnWhichBallIsSitting ballColumn] ++  drop(ballRow + 1) map
        | direction == "l" = take ballRow map ++ [(moveBallLeft (map !! ballRow) charOnWhichBallIsSitting ballColumn)] ++  drop(ballRow + 1) map
        | direction == "u" = moveBallUp map charOnWhichBallIsSitting (ballRow, ballColumn)
        | direction == "d" = moveBallDown map charOnWhichBallIsSitting (ballRow, ballColumn)
        where
                positionOfBall = currentPositionOfCharacters map '@'
                ballRow = fst(head(positionOfBall))
                ballColumn = snd(head(positionOfBall))



moveBallUp ::  [String] -> Char -> (Int, Int) -> [String]
moveBallUp map charOnWhichBallIsSitting (ballRow, ballColumn) = 
        if (ballRow > 0)
                then 
                        case nextChar of
                                '-' -> moveBallUp (moveBallUpOnce map charOnWhichBallIsSitting (ballRow, ballColumn)) nextChar (ballRow -1, ballColumn)
                                'b' -> moveBallUp (moveBallUpOnce map charOnWhichBallIsSitting (ballRow, ballColumn)) '-' (ballRow -1, ballColumn)
                                _   -> map
        else map
        where 
                nextChar = (map !! (ballRow-1)) !! ballColumn


moveBallUpOnce :: [String] -> Char -> (Int, Int) -> [String]
moveBallUpOnce map charOnWhichBallIsSitting (ballRow, ballColumn) = 
        take (ballRow -1) map ++ rowUp ++ rowCurrent ++ drop (ballRow +1) map
        where 
                rowUp = [take ballColumn (map !! (ballRow -1)) ++ ['@'] ++ drop (ballColumn + 1) (map !! (ballRow -1))]
                rowCurrent =[take ballColumn (map !! ballRow ) ++ [charOnWhichBallIsSitting] ++ drop (ballColumn + 1) (map !! ballRow)]

moveBallDown ::  [String] -> Char -> (Int, Int) -> [String]
moveBallDown map charOnWhichBallIsSitting (ballRow, ballColumn) = 
        if (ballRow < length map)
                then 
                        case nextChar of
                                '-' -> moveBallDown (moveBallDownOnce map charOnWhichBallIsSitting (ballRow, ballColumn)) nextChar (ballRow +1, ballColumn)
                                'b' -> moveBallDown (moveBallDownOnce map charOnWhichBallIsSitting (ballRow, ballColumn)) '-' (ballRow +1, ballColumn)
                                _   -> map
        else map
        where 
                nextChar = (map !! (ballRow+1)) !! ballColumn


moveBallDownOnce :: [String] -> Char -> (Int, Int) -> [String]
moveBallDownOnce map charOnWhichBallIsSitting (ballRow, ballColumn) = 
        take (ballRow) map ++ rowCurrent ++ rowDown ++ drop (ballRow +2) map
        where 
                rowDown = [take ballColumn (map !! (ballRow +1)) ++ ['@'] ++ drop (ballColumn + 1) (map !! (ballRow +1))]
                rowCurrent =[take ballColumn (map !! ballRow) ++ [charOnWhichBallIsSitting] ++ drop (ballColumn + 1) (map !! ballRow)]

moveBallRight :: String -> Char -> Int -> String
moveBallRight s charOnWhichBallIsSitting ballposition = 
        if (ballposition + 2 < length s)
        then 
                case s !! (ballposition + 2) of 
                        '-' -> moveBallRight (moveBallRightOnce s charOnWhichBallIsSitting ballposition)  (s !! (ballposition + 2)) (ballposition + 2)
                        'b' -> moveBallRight (moveBallRightOnce s charOnWhichBallIsSitting ballposition)  '-' (ballposition + 2)
                        _ -> s
        else s

moveBallRightOnce :: String -> Char -> Int -> String
moveBallRightOnce s charOnWhichBallIsSitting ballposition = 
        take ballposition s ++ [charOnWhichBallIsSitting] ++ [' '] ++ ['@'] ++ [' '] ++ drop (ballposition + 4) s


moveBallLeft :: String -> Char -> Int -> String
moveBallLeft s charOnWhichBallIsSitting ballposition = 
        if (ballposition - 2 >= 0)
        then 
                case s !! (ballposition - 2) of 
                        '-' -> moveBallLeft (moveBallLeftOnce s charOnWhichBallIsSitting ballposition)  (s !! (ballposition - 2)) (ballposition - 2)
                        'b' -> moveBallLeft (moveBallLeftOnce s charOnWhichBallIsSitting ballposition)  '-' (ballposition - 2)
                        _ -> s
        else s

moveBallLeftOnce :: String -> Char -> Int -> String
moveBallLeftOnce s charOnWhichBallIsSitting ballposition = 
        take (ballposition - 2) s ++ ['@'] ++ [' '] ++ [charOnWhichBallIsSitting] ++ [' '] ++ drop (ballposition +2 ) s

currentPositionOfCharacters :: [String] -> Char -> [(Int,Int)]
currentPositionOfCharacters map c = [ (x,y) | (mapElement, x) <- zip map [0..], y <- elemIndices c mapElement, elem c mapElement]

showMap :: [String] -> IO()
showMap [] = return ()
showMap (l:ls) = do
    putStrLn l
    showMap ls

            