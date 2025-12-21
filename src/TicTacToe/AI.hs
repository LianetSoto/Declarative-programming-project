module TicTacToe.AI where

import TicTacToe.Types
import TicTacToe.Game

-- Obtener celdas vacías
emptyCells :: Board -> [Int]
emptyCells board = [i | (i, cell) <- zip [0..] board, cell == Nothing]

-- Colocar un movimiento en el tablero
placeMove :: Board -> Int -> Player -> Board
placeMove board idx player =
  take idx board ++ [Just player] ++ drop (idx + 1) board

-- Verificar si el juego terminó
isTerminal :: Board -> Bool
isTerminal board = 
  case checkWinner board of
    Just _ -> True
    Nothing -> null (emptyCells board)

-- Función de evaluación para Minimax
evaluate :: Board -> Player -> Int
evaluate board player =
  case checkWinner board of
    Just winnerPlayer 
      | winnerPlayer == player -> 10
      | otherwise -> -10
    Nothing -> 0

-- Algoritmo Minimax 
minimax :: Board -> Int -> Bool -> Player -> Int
minimax board depth isMaximizing player
  | isTerminal board = evaluate board player
  | isMaximizing = 
      let moves = emptyCells board
          scores = map (\idx -> 
            minimax (placeMove board idx player) (depth + 1) False player) moves
      in maximum scores 
  | otherwise =  -- Minimizando
      let opponent = switchPlayer player
          moves = emptyCells board
          scores = map (\idx -> 
            minimax (placeMove board idx opponent) (depth + 1) True player) moves
      in minimum scores 

-- Función auxiliar para comparar tuplas por el primer elemento
compareScore :: (Int, Int) -> (Int, Int) -> Ordering
compareScore (score1, _) (score2, _) = compare score1 score2

-- Función auxiliar para encontrar el máximo por el primer elemento
maximumByScore :: [(Int, Int)] -> (Int, Int)
maximumByScore [] = error "maximumByScore: lista vacía"
maximumByScore xs = foldl1 selectBest xs
  where
    selectBest (score1, idx1) (score2, idx2)
      | score1 > score2 = (score1, idx1)
      | score1 < score2 = (score2, idx2)
      | otherwise = (score1, idx1) 

-- Encontrar el mejor movimiento usando Minimax
findBestMove :: Board -> Player -> Maybe Int
findBestMove board player
  | null availableMoves = Nothing
  | otherwise = Just bestIdx
  where
    availableMoves = emptyCells board
    
    evaluateMove idx = 
      let newBoard = placeMove board idx player
          score = minimax newBoard 0 False player  
      in (score, idx)
    
    evaluatedMoves = map evaluateMove availableMoves
    (_, bestIdx) = maximumByScore evaluatedMoves