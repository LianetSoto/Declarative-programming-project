module TicTacToe.Computer where

import TicTacToe.Types
import TicTacToe.Game
import TicTacToe.AI (findBestMove)

-- Realizar movimiento de la computadora
makeComputerMove :: GameState -> GameState
makeComputerMove gs
  | isJust (winner gs) = gs  -- Juego ya terminó
  | gameMode gs /= HumanVsComputer = gs  -- No es modo contra computadora
  | otherwise =
      case findBestMove (board gs) (currentPlayer gs) of
        Nothing -> gs  -- No hay movimientos posibles
        Just bestIdx -> makeMove bestIdx gs

-- Verificar si es turno de la computadora
isComputersTurn :: GameState -> Bool
isComputersTurn gs = 
  gameMode gs == HumanVsComputer && currentPlayer gs == O  -- Asumimos que la computadora es O