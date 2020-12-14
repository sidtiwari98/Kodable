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
        makeMove map '-' "Right"

-- implement error handling for empty list if @ doesnt exist
makeMove :: [String] -> Char -> String -> IO()
makeMove map char direction = do
                                let directions = ["Right", "Down", "Right", "Up", "Right", "Down", "Right", "Up", "Right"]
                                let positionOfBall = currentPositionOfCharacters map '@'
                                let ballRow = fst(head(positionOfBall))
                                let ballColumn = snd(head(positionOfBall))
                                let move = makeMoveUtility (map !! ballRow) char ballColumn direction
                                -- let rightMove = moveBallRight (map !! ballRow) char ballColumn 
                                let updateRowAfterMove = fst(move)
                                let nextCharAfterBall = fst(snd(move))
                                let charOnWhichBallIs = snd(snd(move))  
                                let updatedMap = updateMap map (ballRow) updateRowAfterMove
                                --        print $ updatedMap 
                                if nextCharAfterBall /= '*' then makeMove updatedMap charOnWhichBallIs direction
                                else showMap $ updatedMap

makeMoveUtility :: String -> Char -> Int -> String -> (String, (Char,Char))
makeMoveUtility s charOnWhichBallWasSitting ballposition direction
        |direction == "Right" = moveBallRight s charOnWhichBallWasSitting ballposition
        |direction == "Left" = moveBallLeft s charOnWhichBallWasSitting ballposition
        |otherwise = moveVertical s charOnWhichBallWasSitting ballposition direction
                                                
updateMap :: [String] -> Int -> String -> [String]
updateMap oldMap rowNumber rowUpdated = take (rowNumber) (oldMap) ++ [rowUpdated] ++ drop(rowNumber + 1)(oldMap)

-- getMapAfterRightMove :: [String] -> Char -> [String]
-- getMapAfterRightMove map char = if ()

-- (UpdateRow (NextCharAfterBall, Char on which ball is sitting)

moveVertical :: String -> Char -> Int -> String -> (String, (Char,Char))
moveVertical s charOnWhichBallWasSitting ballposition direction
        |direction == "Right" = moveBallRight s charOnWhichBallWasSitting ballposition
        |otherwise = moveBallLeft s charOnWhichBallWasSitting ballposition

moveBallRight :: String -> Char -> Int -> (String, (Char,Char))
moveBallRight s charOnWhichBallWasSitting ballposition = (take ballposition s ++ [charOnWhichBallWasSitting] ++ [' '] ++ ['@'] ++ [' '] ++ drop (ballposition + 4) s, (s !! (ballposition + 4), s !! (ballposition + 2)))

-- (UpdateRow (NextCharAfterBall, Char on which ball is sitting)
moveBallLeft :: String -> Char -> Int -> (String, (Char,Char))
moveBallLeft s charOnWhichBallWasSitting ballposition = (take (ballposition - 2) s ++ ['@'] ++ [' '] ++ [charOnWhichBallWasSitting] ++ [' '] ++ drop (ballposition +2 ) s, (s !! (ballposition - 4), s !! (ballposition - 2)))

currentPositionOfCharacters :: [String] -> Char -> [(Int,Int)]
currentPositionOfCharacters map c = [ (x,y) | (mapElement, x) <- zip map [0..], y <- elemIndices c mapElement, elem c mapElement]

showMap :: [String] -> IO()
showMap [] = return ()
showMap (l:ls) = do
    putStrLn l
    showMap ls


-- getDirection :: Int -> IO ()
-- getDirection x = do
--     if x == 0
--         then
--             putStr "First direction: "
--         else
--             putStr "Next direction: "
--     move <- getLine
--     if move == ""
--         then 
--             return ()
--         else
--             do
--                 putStrLn move
--                 getDirection (x+1)

            