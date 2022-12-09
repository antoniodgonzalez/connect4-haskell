import Data.Char
import System.IO
import Data.List
import Text.Read
import Board
import Draw
import System.Random
import Minimax

main :: IO()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False    
    drawBoard emptyBoard
    gameTurn emptyBoard X

gameTurn :: Board -> Piece -> IO()
gameTurn board currentPlayer = do
    drawPrompt currentPlayer
    let mover = if currentPlayer == X then moveFromHuman else moveFromComputer
    column <- mover board
    let nextBoard = move currentPlayer board column
    drawBoard nextBoard
    if checkLine nextBoard column 
        then drawEndOfGame $ Just currentPlayer
        else if noMoreMoves nextBoard 
            then drawEndOfGame Nothing
            else gameTurn nextBoard $ switchPlayer currentPlayer 

moveFromHuman :: Board -> IO Int
moveFromHuman board = do 
    input <- getChar
    case (subtract 1 <$> readMaybe [input]) >>= validMove of
        Nothing -> moveFromHuman board
        Just x -> return x
    where validMove x = if columnInRange x && canMove (board!!x) then Just x else Nothing

moveFromComputer :: Board -> IO Int
moveFromComputer board = do
    putStrLn "Thinking..." 
    return (minimax board)
-- moveFromComputer board = do
--     gen <- getStdGen
--     let (randColumn, newGen) = randomR (0, 6) gen :: (Int, StdGen)
--     newStdGen
--     if canMove (board!!randColumn) 
--         then return randColumn
--         else moveFromComputer board  

switchPlayer :: Player -> Player
switchPlayer X = O
switchPlayer O = X
