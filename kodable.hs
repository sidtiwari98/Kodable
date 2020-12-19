import CommonMapFunctions
import OptimalSolution
import Control.Applicative
import Control.Monad
import Control.Monad.State
import System.IO  
import Data.Char
import Data.List
import Text.Printf

load :: String -> IO()
load s = do
        textContents <- readFile s
        let map = lines textContents
        showMap map
        putStrLn "File loaded"
        putStrLn "Enter play to begin game"
        nextCommand <- getLine
        if (nextCommand == "play") 
                then do 
                        directions <- getDirections (-1) [] []
                        if (length directions == 0) then
                                putStrLn "Invalid move"
                        else
                                play map directions '-'
        else
                if (take 5 nextCommand == "play ")
                        then do
                                let functionDirections = stringSplit ' ' (drop 5 nextCommand)
                                directions <- getDirections (-1) [] functionDirections
                                if (length directions == 0) then
                                        putStrLn "Invalid move"
                                else
                                        play map directions '-'
                                -- play map directions '-'
                else
                        do
                                putStrLn "Invalid command, start from beginning"
                                load s  

play :: [String] -> [String] -> Char -> IO()
play map [] charOnWhichBallIsSitting = 
        do
                if charOnWhichBallIsSitting == 't'
                        then 
                                putStrLn "Congratulations, you win the game!"
                else
                        do
                                putStrLn "The game has not been completed.For a hint type \"hint\" or enter any key to quit"
                                putStrLn "For hint, please wait a few seconds for a hint to appear"
                                command <- getLine
                                if (command == "hint")
                                        then do
                                                let (ballX, ballY) =  head (currentPositionOfCharacters map '@')
                                                let [hint] = getHint map (ballX, ballY) (3 - length (currentPositionOfCharacters map 'b'))
                                                putStr "Try playing -> "
                                                putStrLn hint
                                                directions <- getDirections (1) [] []
                                                play map directions ('-')
                                else
                                        do
                                                putStrLn "You have lost the game :("
                                                return ()


play map directions charOnWhichBallIsSitting = 
        do
                if charOnWhichBallIsSitting == 't'
                        then
                              putStrLn "Congratulations, you win the game!"
                else
                        do  
                        let updatedMap = makeMove map directions charOnWhichBallIsSitting
                        if (map == updatedMap)
                                then do
                                        if (length (head(directions)) == 1)
                                                then do
                                                        putStr "Sorry, error: The colour "
                                                        putStr (head(directions))
                                                        putStrLn " was not found"
                                        else 
                                                do        
                                                        putStr "Sorry, error: cannot move to the "
                                                        putStrLn (head(directions))
                                        
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

                                let (ballXNew, ballYNew) =  head (currentPositionOfCharacters updatedMap '@')
                                if ([[(map !! ballXNew)!! ballYNew]] == take 1 (tail directions))
                                then 
                                        play updatedMap (drop 2 directions) (getBallPositionOnPrevMap map (ballXNew, ballYNew))
                                else
                                        play updatedMap (tail directions) (getBallPositionOnPrevMap map (ballXNew, ballYNew))   

-- maze (ballX, ballY) [] bonusCount [(ballX, ballY, bonusCount)])
getHint :: [String] -> (Int, Int) -> Int -> [String]
getHint maze (ballX, ballY) bonusCount = take 1 (optimalPathShortner $ optimalPath (allSolution maze (ballX, ballY) [] bonusCount [(ballX, ballY, bonusCount)]))

-- stringSplit ',' "a,b,c"
stringSplit :: Eq a => a -> [a] -> [[a]]
stringSplit d [] = []
stringSplit d s = x : stringSplit d (drop 1 y) where (x,y) = span (/= d) s

getBallPositionOnPrevMap :: [String] -> (Int,Int) -> Char
getBallPositionOnPrevMap map (ballXNew, ballYNew) = 
        if (((map !! ballXNew)!! ballYNew) == 'b')
                then '-'
        else
            ((map !! ballXNew)!! ballYNew)  

getDirections :: Int -> [String] -> [String] -> IO [String]
getDirections x directions functionDirections = do
    if (x == -1)
        then
            putStr "First direction: "
        else
            putStr "Next direction: "
    direction <- getLine
    if (direction == "")
        then
                return directions
        else
                if (direction == "Function")
                        then getDirections (x+1) (directions ++ concatMap parseDirections functionDirections) functionDirections
                else getDirections (x+1) (directions ++ parseDirections direction) functionDirections

parseDirections :: String -> [String]
parseDirections directions
        | length directions > 4 && take 4 directions == "Cond" = [ [(directions !! 5)] , take (len - 1 - 8) (drop 8 directions) ]
        | length directions > 4 && take 4 directions == "Loop" = concatMap parseDirections loopParsed
        | otherwise = [directions]
        where 
                len = length(directions)
                loopParsed = parseLoop (take (len - 8 -1) $ drop 8 directions) (read (take 1 (drop 5 directions)) ::Int)

parseLoop :: String -> Int -> [String]
parseLoop loopCommand iterations = concat $ replicate iterations (stringSplit ',' loopCommand)

-- getHint :: [String] -> 

makeMove :: [String] -> [String] -> Char -> [String]
makeMove map (d:ds) charOnWhichBallIsSitting
        | d == "Right" = take ballRow map ++ [moveBallRight (map !! ballRow) charOnWhichBallIsSitting ballColumn (take 1 ds)] ++  drop(ballRow + 1) map
        | d == "Left" = take ballRow map ++ [moveBallLeft (map !! ballRow) charOnWhichBallIsSitting ballColumn (take 1 ds)] ++  drop(ballRow + 1) map
        | d == "Up" = moveBallUp map charOnWhichBallIsSitting (ballRow, ballColumn) (take 1 ds)
        | d == "Down" = moveBallDown map charOnWhichBallIsSitting (ballRow, ballColumn) (take 1 ds)
        | otherwise = map
        where
                positionOfBall = currentPositionOfCharacters map '@'
                ballRow = fst(head(positionOfBall))
                ballColumn = snd(head(positionOfBall))

moveBallUp ::  [String] -> Char -> (Int, Int) -> [String] -> [String]
moveBallUp map charOnWhichBallIsSitting (ballRow, ballColumn) nextDir= 
        if (ballRow > 0)
                then 
                        case nextChar of
                                '-' -> moveBallUp (moveBallUpOnce map charOnWhichBallIsSitting (ballRow, ballColumn)) nextChar (ballRow -1, ballColumn) nextDir
                                
                                'b' -> moveBallUp (moveBallUpOnce map charOnWhichBallIsSitting (ballRow, ballColumn)) '-' (ballRow -1, ballColumn) nextDir
                                
                                'y' ->  if (nextDir == [['y']]) then moveBallUpOnce map charOnWhichBallIsSitting (ballRow, ballColumn)
                                        else moveBallUp (moveBallUpOnce map charOnWhichBallIsSitting (ballRow, ballColumn)) nextChar (ballRow -1, ballColumn) nextDir
                                
                                'o' ->  if (nextDir == [['o']]) then moveBallUpOnce map charOnWhichBallIsSitting (ballRow, ballColumn)
                                        else moveBallUp (moveBallUpOnce map charOnWhichBallIsSitting (ballRow, ballColumn)) nextChar (ballRow -1, ballColumn) nextDir
                                
                                'p' ->  if (nextDir == [['p']]) then moveBallUpOnce map charOnWhichBallIsSitting (ballRow, ballColumn)
                                        else moveBallUp (moveBallUpOnce map charOnWhichBallIsSitting (ballRow, ballColumn)) nextChar (ballRow -1, ballColumn) nextDir
                        
                                't' ->  moveBallUpOnce map charOnWhichBallIsSitting (ballRow, ballColumn)                                
                        
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

moveBallDown ::  [String] -> Char -> (Int, Int) -> [String] -> [String]
moveBallDown map charOnWhichBallIsSitting (ballRow, ballColumn) nextDir = 
        if (ballRow < length map - 1)
                then 
                        case nextChar of
                                '-' -> moveBallDown (moveBallDownOnce map charOnWhichBallIsSitting (ballRow, ballColumn)) nextChar (ballRow +1, ballColumn) nextDir
                                
                                'b' -> moveBallDown (moveBallDownOnce map charOnWhichBallIsSitting (ballRow, ballColumn)) '-' (ballRow +1, ballColumn) nextDir
                                
                                'y' ->  if (nextDir == [['y']]) then moveBallDownOnce map charOnWhichBallIsSitting (ballRow, ballColumn)
                                        else moveBallDown (moveBallDownOnce map charOnWhichBallIsSitting (ballRow, ballColumn)) nextChar (ballRow +1, ballColumn) nextDir
                                
                                'o' ->  if (nextDir == [['o']]) then moveBallDownOnce map charOnWhichBallIsSitting (ballRow, ballColumn)
                                        else moveBallDown (moveBallDownOnce map charOnWhichBallIsSitting (ballRow, ballColumn)) nextChar (ballRow +1, ballColumn) nextDir
                                
                                'p' ->  if (nextDir == [['p']]) then moveBallDownOnce map charOnWhichBallIsSitting (ballRow, ballColumn)
                                        else moveBallDown (moveBallDownOnce map charOnWhichBallIsSitting (ballRow, ballColumn)) nextChar (ballRow +1, ballColumn) nextDir

                                't' ->  moveBallDownOnce map charOnWhichBallIsSitting (ballRow, ballColumn)

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

moveBallRight :: String -> Char -> Int -> [String] -> String
moveBallRight s charOnWhichBallIsSitting ballposition nextDir = 
        if (ballposition + 2 < length s)
        then 
                case s !! (ballposition + 2) of 
                        '-' -> moveBallRight (moveBallRightOnce s charOnWhichBallIsSitting ballposition)  (s !! (ballposition + 2)) (ballposition + 2) nextDir
                        
                        'b' -> moveBallRight (moveBallRightOnce s charOnWhichBallIsSitting ballposition)  '-' (ballposition + 2) nextDir
                        
                        'y' ->  if (nextDir == [['y']]) then moveBallRightOnce s charOnWhichBallIsSitting ballposition
                                else moveBallRight (moveBallRightOnce s charOnWhichBallIsSitting ballposition)  (s !! (ballposition + 2)) (ballposition + 2) nextDir 
                        
                        'o' ->  if (nextDir == [['o']]) then moveBallRightOnce s charOnWhichBallIsSitting ballposition
                                else moveBallRight (moveBallRightOnce s charOnWhichBallIsSitting ballposition)  (s !! (ballposition + 2)) (ballposition + 2) nextDir 
                        
                        'p' ->  if (nextDir == [['p']]) then moveBallRightOnce s charOnWhichBallIsSitting ballposition
                                else moveBallRight (moveBallRightOnce s charOnWhichBallIsSitting ballposition)  (s !! (ballposition + 2)) (ballposition + 2) nextDir
                        
                        't' ->  moveBallRightOnce s charOnWhichBallIsSitting ballposition 
                        
                        _ -> s            
        else s

moveBallRightOnce :: String -> Char -> Int -> String
moveBallRightOnce s charOnWhichBallIsSitting ballposition = 
        take ballposition s ++ [charOnWhichBallIsSitting] ++ [' '] ++ ['@'] ++ [' '] ++ drop (ballposition + 4) s


moveBallLeft :: String -> Char -> Int -> [String] -> String
moveBallLeft s charOnWhichBallIsSitting ballposition nextDir = 
        if (ballposition - 2 >= 0)
        then 
                case s !! (ballposition - 2) of 
                        '-' -> moveBallLeft (moveBallLeftOnce s charOnWhichBallIsSitting ballposition)  (s !! (ballposition - 2)) (ballposition - 2) nextDir
                        
                        'b' -> moveBallLeft (moveBallLeftOnce s charOnWhichBallIsSitting ballposition)  '-' (ballposition - 2) nextDir
                        
                        'y' ->  if (nextDir == [['y']]) then moveBallLeftOnce s charOnWhichBallIsSitting ballposition
                                else moveBallLeft (moveBallLeftOnce s charOnWhichBallIsSitting ballposition)  (s !! (ballposition - 2)) (ballposition - 2) nextDir
                        
                        'o' ->  if (nextDir == [['o']]) then moveBallLeftOnce s charOnWhichBallIsSitting ballposition
                                else moveBallLeft (moveBallLeftOnce s charOnWhichBallIsSitting ballposition)  (s !! (ballposition - 2)) (ballposition - 2) nextDir 
                        
                        'p' ->  if (nextDir == [['p']]) then moveBallLeftOnce s charOnWhichBallIsSitting ballposition
                                else moveBallLeft (moveBallLeftOnce s charOnWhichBallIsSitting ballposition)  (s !! (ballposition - 2)) (ballposition - 2) nextDir

                        't' ->  moveBallLeftOnce s charOnWhichBallIsSitting ballposition                        
                        _ -> s
        else s

moveBallLeftOnce :: String -> Char -> Int -> String
moveBallLeftOnce s charOnWhichBallIsSitting ballposition = 
        take (ballposition - 2) s ++ ['@'] ++ [' '] ++ [charOnWhichBallIsSitting] ++ [' '] ++ drop (ballposition +2 ) s
        