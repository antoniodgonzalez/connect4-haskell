module Minimax ( evaluate, minimax ) where

import Board
import Data.List
import Data.Function
import System.Random

evaluate :: Board -> Int -> Int -> Maybe Int
evaluate board c depth
    | checkLine board c = if head (board!!c) == O then Just (50 - depth) else Just (depth - 50)
    | otherwise = if noMoreMoves board then Just 0 else Nothing

minimax :: Board -> Int
minimax board = fst $ maximumBy (compare `on` snd) $ map eval availableMoves
    where eval c = (c, minimax' (move' c) c 0 False)
          move' = move O board
          availableMoves = filter canMove' [0, 6, 5, 1, 2, 4, 3] --[0..6]
          canMove' c = canMove (board!!c) 

minimax' :: Board -> Int -> Int -> Bool -> Int
minimax' board c depth isMaximizingPlayer = case evaluate board c depth of
    Just value -> value
    Nothing -> if depth == 6 then 0 else if isMaximizingPlayer 
        -- then maximum $ map (\c -> minimax' (move' O c) c (depth + 1) False) availableMoves
        -- else minimum $ map (\c -> minimax' (move' X c) c (depth + 1) True) availableMoves
        -- then maximum $ map maximize availableMoves
        -- else minimum $ map minimize availableMoves
        -- then foldl (\a c -> max a (minimax' (move' O c) c (depth + 1) False)) (-99999) availableMoves
        -- else foldl (\a c -> min a (minimax' (move' X c) c (depth + 1) True)) 99999 availableMoves
        then foldl (\a c -> max a (maximize c)) minBound availableMoves
        else foldl (\a c -> min a (minimize c)) maxBound availableMoves
    where maximize c = minimax' (move' O c) c (depth + 1) False
          minimize c = minimax' (move' X c) c (depth + 1) True
          move' p = move p board
          availableMoves = filter canMove' [0..6]
          canMove' c = canMove (board!!c)

-- atRandIndex :: [a] -> IO a
-- atRandIndex l = do
--     i <- randomRIO (0, length l - 1)
--     return $ l !! i

-- topEvals :: [(Int, Int)] -> [(Int, Int)]
-- topEvals evals = filter (\x -> maxValue == snd x) evals 
--     where maxValue = fst $ maximumBy (compare `on` snd) evals
