module Draw ( drawBoard, drawPrompt, drawEndOfGame ) where

import Data.List
import Data.Strings
import System.Console.ANSI

import Board

drawBoard :: Board -> IO()
drawBoard board = do
    clearFromCursorToLineBeginning
    setCursorColumn 0
    mapM_ drawRow $ map processRow . transpose $ map processColumn board
    drawRow $ " " ++ intersperse ' ' ['1'..'7'] ++ "\n"

drawPrompt :: Player -> IO()
drawPrompt player = putStr $ "Enter move for " ++ show player ++ ": "

drawEndOfGame :: Maybe Player -> IO()
drawEndOfGame Nothing = putStrLn "DRAW"
drawEndOfGame (Just player) = putStrLn $ show player ++ " WINS"

processRow :: String -> String
processRow r = "[" ++ intersperse '|' r ++ "]\n"

processColumn :: Column -> String
processColumn = strPadLeft ' ' 6 . map pieceToChar

pieceToChar :: Piece -> Char
pieceToChar = head . show

drawRow :: String -> IO()
drawRow = mapM_ drawChar

drawChar :: Char -> IO()
drawChar char = do
    setSGR [SetColor Foreground Vivid (colorFromChar char)]
    putChar char

colorFromChar :: Char -> Color
colorFromChar 'X' = Red
colorFromChar 'O' = Yellow
colorFromChar _ = Blue
