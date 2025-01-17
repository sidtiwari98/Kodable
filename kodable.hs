import CommonMapFunctions
import OptimalSolution
import MazeSolver
import Control.Applicative
import Control.Monad
import Control.Monad.State
import System.IO  
import Data.Char
import Data.List
import Text.Printf
import System.Directory


-- This function takes a filename (.txt) which contains the map and displays it on the terminal followed by the game interface.
-- It also checks if the map is valid or invalid.
load :: String -> IO()
load s= do
        textContents <- readFile s
        let map = lines textContents
        putStrLn " "
        putStrLn "File loaded!"
        putStrLn " "
        putStrLn "Initial: "
        showMap map
        putStrLn " "
        putStrLn "Checking validity of map..."
        putStrLn " "
        if (isMapValid map) 
                then do
                        putStrLn "The map is valid."
                        putStrLn " "
                        putStrLn "Enter:"
                        putStrLn "check - to see if map is solvable"
                        putStrLn "solve - to get the solution of the map"
                        putStrLn "quit - to quit the game"
                        putStrLn "play - to begin game"
                        putStrLn " "
                        nextCommand <- getLine 
                        gameCommandHandler map s nextCommand  
        else 
                do
                        putStrLn "The map is invalid. Please ensure you have 1 ball, 1 target and 3 bonuses."
                        putStrLn " "
                        return ()          

-- This loads a saved map stored in savedMap.txt. It displays it on the terminal followed by the game interface.
loadSavedMap :: IO()
loadSavedMap = do
                savedMap <- getSaveMap
                if (length (savedMap) == 0)
                        then do
                                putStrLn "You have no saved map. Please use load function to load a new map"
                else
                        do
                                textContents <- readFile "savedMap.txt"
                                let map = stringSplit ',' textContents
                                putStrLn "File loaded!"
                                putStrLn "Last saved position: "
                                showMap map
                                putStrLn "Enter:"
                                putStrLn "check - to see if map is solvable"
                                putStrLn "solve - to get the solution of the map"
                                putStrLn "quit - to quit the game"
                                putStrLn "play - to begin game"
                                putStrLn " "
                                nextCommand <- getLine
                                gameCommandHandler map "saveMap.txt" nextCommand                 

--This is the central command hanlding function for all the commands entered by the user.
--It matches the commands input and returns the corresponding outputs
-- For an invalid command it outputs Invalid command 
gameCommandHandler :: [String] -> String -> String -> IO()
gameCommandHandler map filename command = do
                        if (take 5 command == "play ")
                                then do
                                        let functionDirections = stringSplit ' ' (drop 5 command)
                                        if (isFunctionDirectionValid functionDirections)
                                                then do
                                                        directions <- getDirections (-1) [] functionDirections
                                                        if (length directions == 0) then do
                                                                putStrLn "Invalid move"
                                                                putStrLn " "
                                                        else
                                                                play map directions '-'
                                        else
                                                do
                                                        putStrLn "Your function contains invalid move(s)."
                                                        load filename
                        else
                                do
                                case command of
                                        "play" -> do 
                                                        directions <- getDirections (-1) [] []
                                                        if (length directions == 0) then do
                                                                putStrLn "Invalid move"
                                                                putStrLn " "
                                                        else
                                                                play map directions '-'
                                        "hint" -> do
                                                        let [hint] = getHint map
                                                        putStrLn ("Try playing -> " ++ hint)
                                                        directions <- getDirections (1) [] []
                                                        putStrLn " "
                                                        play map directions ('-')
                                        "quit"  -> do
                                                        putStrLn " " 
                                                        putStrLn "You have lost the game :("
                                                        putStrLn "See you soon"
                                                        putStrLn " "
                                                        return ()
                                        
                                        "save"  -> do
                                                       saveMap map
                                                       putStrLn "Game saved succesfully!"
                                                       putStrLn ""     

                                        "check" -> do
                                                        if (isSolvable map) then do
                                                                putStrLn " " 
                                                                putStrLn "The map is solvable"
                                                                putStrLn "Enter:"
                                                                putStrLn "solve - to get the solution of the map"
                                                                putStrLn "quit - to quit the game"
                                                                putStrLn "play - to begin game"
                                                                putStrLn " "
                                                                command <- getLine
                                                                gameCommandHandler map filename command
                                                        else do
                                                                putStrLn " "   
                                                                putStrLn "The map is not solvable. Load the game with a solvable map" 
                                        "solve" -> do
                                                        if (isSolvable map) then do
                                                                putStrLn " " 
                                                                let solution = optimalSolution map
                                                                putStrLn "The solution is: "
                                                                putStrLn (mapToString solution ' ')
                                                                let compressedSol = compressOptSol solution
                                                                putStrLn " "
                                                                putStrLn "(Beta version) The most compressed solution is: "
                                                                putStrLn (mapToString compressedSol ' ')
                                                                putStrLn " "
                                                                putStrLn "Enter:"
                                                                putStrLn "quit - to quit the game"
                                                                putStrLn "play - to begin game" 
                                                                putStrLn " "
                                                                command <- getLine
                                                                gameCommandHandler map filename command                                                                 
                                                        else do
                                                                putStrLn " "  
                                                                putStrLn "The map is not solvable. Load the game with a solvable map"      
                                                                        
                                        _       ->      do
                                                                putStrLn "INVALID COMMAND, start from beginning"
                                                                load filename                        
            
-- main function to sequentially play the directions inoutted by the user
-- checks for game win, prints the map after every move, checks for number of bonus eaten
-- checks for an error like not able to move to a particular direction or colour not found
-- terminates when direction list is empty.
play :: [String] -> [String] -> Char -> IO()
play map [] charOnWhichBallIsSitting = 
        do
                if charOnWhichBallIsSitting == 't'
                        then do
                                if (length (currentPositionOfCharacters map 'b') == 0)
                                then 
                                        putStrLn "Congratulations, you win the game!"
                                else
                                        do
                                                putStrLn "You have won the game but did not collect all 3 bonuses!"
                                                putStrLn ("Your bonus count is " ++ (show $ 3- length (currentPositionOfCharacters map 'b'))) 
                else
                        do
                                putStrLn "The game has not been completed.For a hint type \"hint\". To save type \"save\". To quit type quit \"quit\"."
                                putStrLn "For hint, please wait a few seconds for a hint to appear."
                                putStrLn "To play a saved game, use the loadSavedMap function next time to start the game."
                                putStrLn ""
                                command <- getLine
                                gameCommandHandler map "none" command

play map directions charOnWhichBallIsSitting = 
        do
                if charOnWhichBallIsSitting == 't'
                        then
                                if (length (currentPositionOfCharacters map 'b') == 0)
                                then 
                                        putStrLn "Congratulations, you win the game!"
                                else
                                        do
                                                putStrLn "You have won the game but did not collect all 3 bonuses!"
                                                putStrLn ("Your bonus count is " ++ (show $ 3- length (currentPositionOfCharacters map 'b'))) 
                else
                        do  
                        let updatedMap = makeMove map directions charOnWhichBallIsSitting
                        if (map == updatedMap)
                                then do
                                        let [hint] = getHint map
                                        if (length (head(directions)) == 1)
                                                then do
                                                        putStrLn ("Sorry, error: The colour " ++ (head(directions)) ++ " was not found")
                                                        putStrLn " "
                                                        putStrLn ("Next time try playing " ++ hint ++ " at this stage")
                                                        putStrLn ""

                                        else 
                                                do        
                                                        putStrLn ("Sorry, error: cannot move to the " ++ (head(directions)))
                                                        putStrLn " "
                                                        putStrLn ("Next time try playing " ++ hint ++ " at this stage")
                                                        putStrLn " "
                                        
                                        putStrLn "Your current Board :"
                                        showMap updatedMap
                                        gameCommandHandler map "none" "quit"
                        else do
                                showMap updatedMap   
                                let bonusEarned = 3 - length (currentPositionOfCharacters updatedMap 'b')
                                let prevBonus = 3 - length (currentPositionOfCharacters map 'b')
                                if (bonusEarned /= prevBonus)
                                        then do
                                                putStrLn ("Got " ++ (show (bonusEarned)) ++ " bonus")
                                                putStrLn " "  
                                else
                                        putStrLn " " 

                                let (ballXNew, ballYNew) =  head (currentPositionOfCharacters updatedMap '@')
                                if ([[(map !! ballXNew)!! ballYNew]] == take 1 (tail directions))
                                then 
                                        play updatedMap (drop 2 directions) (getBallPositionOnPrevMap map (ballXNew, ballYNew))
                                else
                                        play updatedMap (tail directions) (getBallPositionOnPrevMap map (ballXNew, ballYNew))  
  
-- Used to get directions input by the user
-- returns the final list of directions which is later sent back to the play function
getDirections :: Int -> [String] -> [String] -> IO [String]
getDirections x directions functionDirections = do
    if (x == -1)
        then
            putStr "First direction: "
        else
            putStr "Next direction: "
    direction <- getLine
    if (direction == "")
        then do
                putStrLn " "
                return directions
        else
                do
                        if ((isNormalDirection direction) || isCondValid direction || isLoopValid direction || direction `elem` ["Function", "Undo"])
                                then do        
                                        case direction of
                                                "Function"      -> getDirections (x+1) (directions ++ concatMap parseDirections functionDirections) functionDirections
                                                "Undo"          -> getDirections (x+1) (take (length (directions) - 1) directions) functionDirections
                                                _               -> getDirections (x+1) (directions ++ parseDirections direction) functionDirections
                        else do
                               putStrLn "INVALID MOVE"
                               getDirections (x+1) (directions) functionDirections 

--parses all the direction inpputted by the user and is used extract the colour from conditional directions
-- also used to replicate the loop directions n number of times
parseDirections :: String -> [String]
parseDirections directions
        | length directions > 4 && take 4 directions == "Cond" = [ [(directions !! 5)] , take (len - 1 - 8) (drop 8 directions) ]
        | length directions > 4 && take 4 directions == "Loop" = concatMap parseDirections loopParsed
        | otherwise = [directions]
        where 
                len = length(directions)
                loopParsed = parseLoop (take (len - 8 -1) $ drop 8 directions) (read (take 1 (drop 5 directions)) ::Int)

-- used to replicate the loop directions n number of times
parseLoop :: String -> Int -> [String]
parseLoop loopCommand iterations = concat $ replicate iterations (stringSplit ',' loopCommand)

-- gets the map stored in savedMap.txt
getSaveMap :: IO [String]
getSaveMap = do 
                all <- getDirectoryContents "./"
                let savedMap = filter (isSuffixOf "savedMap.txt") all
                return savedMap

-- saves a map to savedMap.txt
saveMap :: [String] -> IO ()
saveMap map =  writeFile "savedMap.txt" (mapToString map ',')

--converts a list of string to a single string seperated by a delimiter
mapToString :: [String] -> Char -> String
mapToString [] d = ""
mapToString (s:ss) d= s ++ [d] ++ (mapToString ss d)

--gets Hint using the optimalSolution 
getHint :: [String] -> [String]
getHint maze  = take 1 (optimalSolution maze)

-- stringSplit ',' "a,b,c"
-- used to split a string by a delimiter and return a list
stringSplit :: Eq a => a -> [a] -> [[a]]
stringSplit d [] = []
stringSplit d s = x : stringSplit d (drop 1 y) where (x,y) = span (/= d) s

--gets the position of the ball in the map before the updatedMap
getBallPositionOnPrevMap :: [String] -> (Int,Int) -> Char
getBallPositionOnPrevMap map (ballXNew, ballYNew) = 
        if (((map !! ballXNew)!! ballYNew) == 'b')
                then '-'
        else
            ((map !! ballXNew)!! ballYNew)

--Checks if the map is valid or not
isMapValid :: [String] -> Bool
isMapValid map
        | (length ball) == 1 && (length target) == 1 && (length bonuses) == 3 = True
        | otherwise =  False
        where
                ball = currentPositionOfCharacters map '@'
                target =  currentPositionOfCharacters map 't'
                bonuses = currentPositionOfCharacters map 'b'

--Checks if the function directions input are valid or not
isFunctionDirectionValid :: [String] -> Bool
isFunctionDirectionValid (d1:d2:d3:ds)
        | length (ds) > 0 = False
        | (isNormalDirection d1 || isCondValid d1) && (isNormalDirection d2 || isCondValid d2) && (isNormalDirection d3 || isCondValid d3) = True
        | take 4 d1 == "Loop" || take 4 d2 == "Loop" || take 4 d3 == "Loop" = False
        | otherwise = False
isFunctionDirectionValid _ = False

--Checks if the normal direction input is valid or not
isNormalDirection :: String -> Bool
isNormalDirection dir
        | dir `elem` ["Right", "Left", "Up", "Down"] = True
        | otherwise = False

--Checks if the condition direction input is valid or not
isCondValid :: String -> Bool
isCondValid dir 
        | length dir > 4 && take 4 dir == "Cond" && ( colour `elem` ["o", "p", "y"]) && isNormalDirection (normalDir) = True
        | otherwise = False
        where
                colour = drop 5 (take 6 dir)
                normalDir = drop 8 (take (length dir - 1) dir)

--Checks if the loop direction input is valid or not
isLoopValid :: String -> Bool
isLoopValid dir 
        | length dir > 4 && take 4 dir == "Loop" && ((read number :: Int) <= 5) && length (loopDirections) == 2 && d1Valid && d2Valid = True
        | otherwise = False
        where
                number  = drop 5 (take 6 dir)
                loopDirections = stringSplit ',' (drop 8 (take (length dir - 1) dir))
                d1Valid = isNormalDirection (head loopDirections) || isCondValid (head loopDirections)
                d2Valid = isNormalDirection (concat (tail loopDirections)) || isCondValid (concat (tail loopDirections))

--This function takes all the direction inputted by the user and sequentially plays them out
--It returns the updated map after every move
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

--This function takes the map and recursively calls moveBallUpOnce until the ball reaches the wall or reaches a colour where it can change direction.
-- Returns the updated map after moving the ball up as much as possible
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

--This funcntion takes the map, character of ball current position, balls current position on the map
--It returns the updated map after moving ball once up
moveBallUpOnce :: [String] -> Char -> (Int, Int) -> [String]
moveBallUpOnce map charOnWhichBallIsSitting (ballRow, ballColumn) = 
        take (ballRow -1) map ++ rowUp ++ rowCurrent ++ drop (ballRow +1) map
        where 
                rowUp = [take ballColumn (map !! (ballRow -1)) ++ ['@'] ++ drop (ballColumn + 1) (map !! (ballRow -1))]
                rowCurrent =[take ballColumn (map !! ballRow ) ++ [charOnWhichBallIsSitting] ++ drop (ballColumn + 1) (map !! ballRow)]

--This function takes the map and recursively calls moveBallDownOnce until the ball reaches the wall or reaches a colour where it can change direction.
-- Returns the updated map after moving the ball down as mich as possible
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

--This funcntion takes the map, character of ball current position, balls current position on the map
--It returns the updated map after moving ball once down
moveBallDownOnce :: [String] -> Char -> (Int, Int) -> [String]
moveBallDownOnce map charOnWhichBallIsSitting (ballRow, ballColumn) = 
        take (ballRow) map ++ rowCurrent ++ rowDown ++ drop (ballRow +2) map
        where 
                rowDown = [take ballColumn (map !! (ballRow +1)) ++ ['@'] ++ drop (ballColumn + 1) (map !! (ballRow +1))]
                rowCurrent =[take ballColumn (map !! ballRow) ++ [charOnWhichBallIsSitting] ++ drop (ballColumn + 1) (map !! ballRow)]

--This function takes a row and recursively calls moveRightOnce until the ball reaches the wall or reaches a colour where it can change direction.
-- Returns the updated row after moving the ball right as mich as possible
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

--This funcntion takes a row, character of ball current position, balls current position on the row
--It returns the updated row after moving ball once towards the left
moveBallRightOnce :: String -> Char -> Int -> String
moveBallRightOnce s charOnWhichBallIsSitting ballposition = 
        take ballposition s ++ [charOnWhichBallIsSitting] ++ [' '] ++ ['@'] ++ [' '] ++ drop (ballposition + 4) s


--This function takes a row and recursively calls moveLeftOnce until the ball reaches the wall or reaches a colour where it can change direction.
-- Returns the updated row after moving the ball left as mich as possible
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

--This funcntion takes a row, character of ball current position, balls current position on the row
--It returns the updated row after moving ball once towards the left
moveBallLeftOnce :: String -> Char -> Int -> String
moveBallLeftOnce s charOnWhichBallIsSitting ballposition = 
        take (ballposition - 2) s ++ ['@'] ++ [' '] ++ [charOnWhichBallIsSitting] ++ [' '] ++ drop (ballposition +2 ) s