module TicTacToe.Game where

import TicTacToe.Types

initialState :: GameState
initialState = GameState
  { board = replicate 9 Nothing
  , currentPlayer = X
  , winner = Nothing
  , gameMode = HumanVsHuman
  }

switchPlayer :: Player -> Player
switchPlayer X = O
switchPlayer O = X

-- Cambiar modo de juego
switchMode :: GameMode -> GameMode
switchMode HumanVsHuman = HumanVsComputer
switchMode HumanVsComputer = HumanVsHuman

-- Combinaciones ganadoras 
winningCombinations :: [[Int]]
winningCombinations =
  [ [0,1,2]  -- fila superior
  , [3,4,5]  -- fila media
  , [6,7,8]  -- fila inferior
  , [0,3,6]  -- columna izquierda
  , [1,4,7]  -- columna centro
  , [2,5,8]  -- columna derecha
  , [0,4,8]  -- diagonal \
  , [2,4,6]  -- diagonal /
  ]

allSamePlayer :: [Cell] -> Maybe Player
allSamePlayer cells =
  case cells of
    [Just X, Just X, Just X] -> Just X
    [Just O, Just O, Just O] -> Just O
    _ -> Nothing

-- Verifica si hay ganador en el tablero actual
checkWinner :: Board -> Maybe Player
checkWinner board = 
  -- Busca en todas las combinaciones ganadoras
  let results = map (\indices -> allSamePlayer [board !! i | i <- indices]) winningCombinations
  in foldr checkResult Nothing results
  where
    checkResult (Just player) _ = Just player  -- Si encontramos ganador, lo devolvemos
    checkResult Nothing acc = acc  -- Si no, seguimos buscando

-- Verifica si el tablero está lleno (empate)
isBoardFull :: Board -> Bool
isBoardFull = all (/= Nothing)  -- Verifica que no haya celdas vacías

makeMove :: Int -> GameState -> GameState
makeMove idx gs
  -- Validaciones
  | idx < 0 || idx >= length (board gs) = gs
  | isJust (winner gs) = gs
  | board gs !! idx /= Nothing = gs
  | otherwise =
      let newBoard = take idx (board gs)
                  ++ [Just (currentPlayer gs)]
                  ++ drop (idx + 1) (board gs)
          
          gameWinner = checkWinner newBoard
          boardFull = isBoardFull newBoard
          
          newWinner = case gameWinner of
                       Just _ -> gameWinner
                       Nothing -> if boardFull 
                                 then Just (switchPlayer (currentPlayer gs))
                                 else Nothing
          
          nextPlayer = switchPlayer (currentPlayer gs)
          
      in gs { board = newBoard
            , currentPlayer = nextPlayer
            , winner = newWinner
            }

-- Función auxiliar para Maybe
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False