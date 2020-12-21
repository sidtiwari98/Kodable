module OptimalSolution (
    optimalSolution,
    allSolution,
    compressOptSol
) where
import CommonMapFunctions
import Control.Applicative
import Control.Monad
import Control.Monad.State
import System.IO  
import Data.Char
import Data.List
import Text.Printf
import Data.Ord

optimalSolution :: [String] -> [String]
optimalSolution maze = optimalPath (map optimalPathShortner allSolutions)
                        where
                            (ballX, ballY) = head $ currentPositionOfCharacters maze '@'
                            bonusCount = 3 - length (currentPositionOfCharacters maze 'b')
                            allSolutions = allSolution maze (ballX, ballY) [] bonusCount [(ballX, ballY, bonusCount)]

optimalPathShortner :: [String] -> [String]
optimalPathShortner [] = []
optimalPathShortner [direction] = [direction]
optimalPathShortner (d1: d2: d) =   if d1 == take (length(d2) - 1 - 8) (drop 8 d2)
                                        then [d1] ++ optimalPathShortner d
                                    else
                                        [d1] ++ optimalPathShortner([d2] ++ d)
                                
optimalPath :: (Foldable t1, Foldable t2) => t1 (t2 a) -> t2 a
optimalPath allsolutions = minimumBy (comparing length) allsolutions

compressOptSol :: [String] -> [String]
compressOptSol solution = compressOptSolUtil solution 0 [] False [] 0 []

--I developed the logic of this function with Kush Baheti (UID - 3035436583)
compressOptSolUtil :: (Eq a, Num a, Show a) => [String] -> Int -> [String] -> Bool -> [String] -> a -> [String] -> [String]
compressOptSolUtil solution i alreadyParsed fBranch loopPrevPair loopCounter func = 
    optimalPath $ [loopResult, singleResult, functionResult]
    where
        loopPair = [solution !! i] ++ [solution !! (i + 1)] 
        loopCount = if (loopPair == loopPrevPair) 
                        then (loopCounter + 1) 
                        else 1 
        loopIntermediateParsed = if ((loopPair == loopPrevPair) && (loopCounter == 1)) 
                            then take ((length alreadyParsed) - 1) alreadyParsed
                            else alreadyParsed
        loopNumber = show loopCount
        loopDir1 = head loopPair
        loopDir2 = concat $ tail loopPair
        loopParsed' =  (take ((length loopIntermediateParsed) - 1) loopIntermediateParsed) ++ [("Loop{" ++ loopNumber ++ "}{" ++ loopDir1 ++ "," ++ loopDir2 ++ "}")]
        loopParsed = if (loopPair == loopPrevPair) then loopParsed' else (alreadyParsed ++ loopPair)
        singleParsed = alreadyParsed ++ [solution !! i]
        functionParsed = if (fBranch == True) 
                            then 
                                (take ((length alreadyParsed) - 1) alreadyParsed) ++ ["Function"] 
                            else (alreadyParsed ++ drop i solution) 
        currentFunc = if (fBranch == True) 
                            then [solution !! (i -1)] ++ [solution !! i] ++ [solution !! (i +1)] 
                            else []

        loopResult = if (i + 2 < length solution) 
                    then compressOptSolUtil solution (i+2) loopParsed False loopPair loopCount func 
                    else (alreadyParsed ++ drop i solution) 

        functionResult = 
            if ( ((i +1) < length solution) && (fBranch == True) && ((func == []) || (func /= [] && currentFunc == func))) 
                then 
                    compressOptSolUtil solution (i+2) functionParsed False [] 0 currentFunc 
                else (alreadyParsed ++ drop i solution)

        singleResult = if (i < length solution) 
                            then 
                                compressOptSolUtil solution (i + 1) singleParsed True [] 0 func 
                            else alreadyParsed

allSolution :: [String] -> (Int, Int) -> [String] -> Int -> [(Int, Int, Int)] -> [[String]]
allSolution maze (ballX, ballY) currentSolution bonusCount visited
    | currentCharOfBall == 't' && bonusCount /= 3 = []
    | currentCharOfBall == 't' && bonusCount == 3 = [currentSolution]
    | currentCharOfBall `elem` ['p', 'o', 'y'] = cRight ++ cLeft ++ cUp ++ cDown
    | otherwise = right ++ left ++ up ++ down
    where
        currentCharOfBall = (maze !! ballX) !! ballY
        (mazeR, (ballXr, ballYr), bonusr) = pathToRight maze (ballX, ballY) bonusCount visited
        (mazeL, (ballXl, ballYl), bonusl) = pathToLeft maze (ballX, ballY) bonusCount visited
        (mazeU, (ballXu, ballYu), bonusu) = pathToUp maze (ballX, ballY) bonusCount visited
        (mazeD, (ballXd, ballYd), bonusd) = pathToDown maze (ballX, ballY) bonusCount visited
        right = if mazeR == [] then [] else allSolution mazeR (ballXr, ballYr) (currentSolution ++ ["Right"]) bonusr (visited ++ [(ballXr, ballYr, bonusr)])
        left = if mazeL == [] then [] else allSolution mazeL (ballXl, ballYl) (currentSolution ++ ["Left"]) bonusl (visited ++ [(ballXl, ballYl, bonusl)])
        up = if mazeU == [] then [] else allSolution mazeU (ballXu, ballYu) (currentSolution ++ ["Up"]) bonusu (visited ++ [(ballXu, ballYu, bonusu)])
        down = if mazeD == [] then [] else allSolution mazeD (ballXd, ballYd) (currentSolution ++ ["Down"]) bonusd (visited ++ [(ballXd, ballYd, bonusd)])
        cRightDirection = "Cond{" ++ [currentCharOfBall] ++ "}{Right}"
        cLeftDirection = "Cond{" ++ [currentCharOfBall] ++ "}{Left}"
        cUpDirection = "Cond{" ++ [currentCharOfBall] ++ "}{Up}"
        cDownDirection = "Cond{" ++ [currentCharOfBall] ++ "}{Down}"
        cRight = if mazeR == [] then [] else allSolution mazeR (ballXr, ballYr) (currentSolution ++ [cRightDirection]) bonusr (visited ++ [(ballXr, ballYr, bonusr)])
        cLeft = if mazeL == [] then [] else allSolution mazeL (ballXl, ballYl) (currentSolution ++ [cLeftDirection]) bonusl (visited ++ [(ballXl, ballYl, bonusl)])
        cUp = if mazeU == [] then [] else allSolution mazeU (ballXu, ballYu) (currentSolution ++ [cUpDirection]) bonusu (visited ++ [(ballXu, ballYu, bonusu)])
        cDown = if mazeD == [] then [] else allSolution mazeD (ballXd, ballYd) (currentSolution ++ [cDownDirection]) bonusd (visited ++ [(ballXd, ballYd, bonusd)])

pathToRight :: [String] -> (Int, Int) -> Int -> [(Int, Int, Int)] -> ([String], (Int, Int), Int)
pathToRight maze (ballX, ballY) bonusCount visited 
    | (ballXr == ballX && ballYr == ballY) || (ballXr, ballYr, bonusr) `elem` visited = ([], (ballX, ballY), bonusCount)
    | otherwise = (pathR, (ballXr, ballYr), bonusr)
    where
        (pathR, (ballXr, ballYr), bonusr) = goRight maze (ballX, ballY) bonusCount

pathToLeft :: [String] -> (Int, Int) -> Int -> [(Int, Int, Int)] -> ([String], (Int, Int), Int)
pathToLeft maze (ballX, ballY) bonusCount visited 
    | (ballXl == ballX && ballYl == ballY) || (ballXl, ballYl, bonusl) `elem` visited = ([], (ballX, ballY), bonusCount)
    | otherwise = (pathL, (ballXl, ballYl), bonusl)
    where
        (pathL, (ballXl, ballYl), bonusl) = goLeft maze (ballX, ballY) bonusCount

pathToUp :: [String] -> (Int, Int) -> Int -> [(Int, Int, Int)] -> ([String], (Int, Int), Int)
pathToUp maze (ballX, ballY) bonusCount visited 
    | (ballXu == ballX && ballYu == ballY) || (ballXu, ballYu, bonusu) `elem` visited = ([], (ballX, ballY), bonusCount)
    | otherwise = (pathU, (ballXu, ballYu), bonusu)
    where
        (pathU, (ballXu, ballYu), bonusu) = goUp maze (ballX, ballY) bonusCount

pathToDown :: [String] -> (Int, Int) -> Int -> [(Int, Int, Int)] -> ([String], (Int, Int), Int)
pathToDown maze (ballX, ballY) bonusCount visited 
    | (ballXd == ballX && ballYd == ballY) || (ballXd, ballYd, bonusd) `elem` visited = ([], (ballX, ballY), bonusCount)
    | otherwise = (pathD, (ballXd, ballYd), bonusd)
    where
        (pathD, (ballXd, ballYd), bonusd) = goDown maze (ballX, ballY) bonusCount

goRight :: [String] -> (Int, Int) -> Int -> ([String], (Int, Int), Int)
goRight maze (ballX, ballY) bonusCount
    | ballYNew >= snd(dimensionOfMaze maze) || nextCharOfBall == '*' = (maze, (ballX, ballY), bonusCount)
    | nextCharOfBall == 'b' = goRight (deleteBonus maze (ballXNew, ballYNew)) (ballXNew, ballYNew) (bonusCount + 1)
    | nextCharOfBall `elem` ['p', 'o', 't', 'y'] = (maze, (ballXNew, ballYNew), bonusCount)
    | nextCharOfBall `elem` ['-', '@' ] = goRight maze (ballXNew, ballYNew) bonusCount
    | otherwise = goRight maze (ballXNew, ballYNew) bonusCount
    where
        (ballXNew, ballYNew) = newPositionOfBall (ballX, ballY) "Right"
        nextCharOfBall =  (maze !! ballXNew) !! ballYNew

goLeft :: [String] -> (Int, Int) -> Int -> ([String], (Int, Int), Int)
goLeft maze (ballX, ballY) bonusCount
    | ballYNew < 0 || nextCharOfBall == '*' = (maze, (ballX, ballY), bonusCount)
    | nextCharOfBall == 'b' = goLeft (deleteBonus maze (ballXNew, ballYNew)) (ballXNew, ballYNew) (bonusCount + 1)
    | nextCharOfBall `elem` ['p', 'o', 't', 'y'] = (maze, (ballXNew, ballYNew), bonusCount)
    | nextCharOfBall `elem` ['-', '@' ] = goLeft maze (ballXNew, ballYNew) bonusCount
    | otherwise = goLeft maze (ballXNew, ballYNew) bonusCount
    where
        (ballXNew, ballYNew) = newPositionOfBall (ballX, ballY) "Left"
        nextCharOfBall =  (maze !! ballXNew) !! ballYNew

goUp :: [String] -> (Int, Int) -> Int -> ([String], (Int, Int), Int)
goUp maze (ballX, ballY) bonusCount
    | ballXNew < 0 || nextCharOfBall == '*' = (maze, (ballX, ballY), bonusCount)
    | nextCharOfBall == 'b' = goUp (deleteBonus maze (ballXNew, ballYNew)) (ballXNew, ballYNew) (bonusCount + 1)
    | nextCharOfBall `elem` ['p', 'o', 't', 'y'] = (maze, (ballXNew, ballYNew), bonusCount)
    | nextCharOfBall `elem` ['-', '@' ] = goUp maze (ballXNew, ballYNew) bonusCount
    | otherwise = goUp maze (ballXNew, ballYNew) bonusCount
    where
        (ballXNew, ballYNew) = newPositionOfBall (ballX, ballY) "Up"
        nextCharOfBall =  (maze !! ballXNew) !! ballYNew

goDown :: [String] -> (Int, Int) -> Int -> ([String], (Int, Int), Int)
goDown maze (ballX, ballY) bonusCount
    | ballXNew >= fst(dimensionOfMaze maze) || nextCharOfBall == '*' = (maze, (ballX, ballY), bonusCount)
    | nextCharOfBall == 'b' = goDown (deleteBonus maze (ballXNew, ballYNew)) (ballXNew, ballYNew) (bonusCount + 1)
    | nextCharOfBall `elem` ['p', 'o', 't', 'y'] = (maze, (ballXNew, ballYNew), bonusCount)
    | nextCharOfBall `elem` ['-', '@' ] = goDown maze (ballXNew, ballYNew) bonusCount
    | otherwise = goDown maze (ballXNew, ballYNew) bonusCount
    where
        (ballXNew, ballYNew) = newPositionOfBall (ballX, ballY) "Down"
        nextCharOfBall =  (maze !! ballXNew) !! ballYNew

deleteBonus :: [String] -> (Int, Int) -> [String]
deleteBonus maze (bonusX, bonusY) = take bonusX maze ++ [(take bonusY (maze !! bonusX) ++ ['-'] ++ drop (bonusY +1) (maze !! bonusX))] ++ drop (bonusX + 1) maze