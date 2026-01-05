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

-- Función de evaluación 
evaluate :: Board -> Player -> Int
evaluate board player =
  case checkWinner board of
    Just winnerPlayer 
      | winnerPlayer == player -> 1000   -- Victoria para el jugador
      | otherwise -> -1000               -- Victoria para el oponente
    Nothing -> 0                         -- Empate

-- Algoritmo Minimax con poda Alpha-Beta
minimax :: Board -> Int -> Int -> Int -> Bool -> Player -> Int
minimax board depth alpha beta maximizingPlayer player
  | isTerminal board = evaluate board player
  | maximizingPlayer =
      let maxEval = -10000  -- -∞
      in goMax (emptyCells board) maxEval alpha beta
  | otherwise =
      let minEval = 10000   -- +∞
      in goMin (emptyCells board) minEval alpha beta
  where
    -- MAXIMIZING PLAYER
    goMax [] best _ _ = best
    goMax (idx:rest) best a b =
      let newBoard = placeMove board idx player
          eval = minimax newBoard (depth + 1) a b False player
          best' = max best eval
          a' = max a eval
      in if b <= a'  
         then best'   -- break
         else goMax rest best' a' b
    
    -- MINIMIZING PLAYER
    goMin [] best _ _ = best
    goMin (idx:rest) best a b =
      let opponent = switchPlayer player
          newBoard = placeMove board idx opponent
          eval = minimax newBoard (depth + 1) a b True player
          best' = min best eval
          b' = min b eval
      in if b' <= a 
         then best'   -- break
         else goMin rest best' a b'

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
          score = minimax newBoard 0 (-10000) 10000 False player
      in (score, idx)
    
    evaluatedMoves = map evaluateMove availableMoves
    (_, bestIdx) = maximumByScore evaluatedMoves