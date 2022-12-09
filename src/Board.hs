module Board ( Piece(O, X), Player, Column, Board, 
    emptyBoard, columnInRange, canMove, move, noMoreMoves, checkLine ) where

data Piece = O | X deriving (Show, Eq)
type Player = Piece
type Column = [Piece]
type Board = [Column]

emptyBoard :: Board
emptyBoard = replicate 7 []

columnInRange :: Int -> Bool
columnInRange = (`elem` [0..6])

canMove :: Column -> Bool
canMove = (< 6) . length

move :: Piece -> Board -> Int -> Board
move piece board column = left ++ (piece:c) : right
    where (left, c:right) = splitAt column board

noMoreMoves :: Board -> Bool
noMoreMoves = not . any canMove

checkLine :: Board -> Int -> Bool
checkLine board c = 
    any (\f -> f board c) [verticalLine, horizontalLine, diagonalRightLine, diagonalLeftLine]

verticalLine :: Board -> Int -> Bool
verticalLine board c = verticalLine' $ board!!c
    where verticalLine' (a:b:c:d:xs) = all (== a) [b, c ,d]
          verticalLine' _ = False

horizontalLine :: Board -> Int -> Bool
horizontalLine = line 0

diagonalRightLine :: Board -> Int -> Bool
diagonalRightLine = line 1

diagonalLeftLine :: Board -> Int -> Bool
diagonalLeftLine = line (-1)

line :: Int -> Board -> Int -> Bool
line rowOffset board c =
    (1 + count board (c-1) (r-rowOffset) piece (-1) (-rowOffset) 
       + count board (c+1) (r+rowOffset) piece 1 rowOffset) >= 4 
    where column = board!!c
          r = length column
          piece = head column

count :: Board -> Int -> Int -> Piece -> Int -> Int -> Int
count board c r piece columnOffset rowOffset
    | not (columnInRange c) || getPieceAt r (board!!c) /= Just piece = 0
    | otherwise =  1 + count board (c + columnOffset) (r + rowOffset) piece columnOffset rowOffset
                
getPieceAt :: Int -> Column -> Maybe Piece
getPieceAt _ [] = Nothing
getPieceAt 1 xs = Just (last xs)
getPieceAt n xs = getPieceAt (n-1) (init xs)