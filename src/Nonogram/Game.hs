module Nonogram.Game
  ( initialGame
  , toggleCell
  , resetGame
  , checkVictory
  , puzzleFromSample5
  , puzzleFromSample10
  ) where

import Nonogram.Types
import Data.List (group)

-- Crear tablero vacío (todas las celdas Unknown)
mkEmptyBoard :: Size -> [[Cell]]
mkEmptyBoard (rows, cols) = replicate rows (replicate cols Unknown)

-- Inicializar juego con Puzzle dado
initialGame :: Puzzle -> Game
initialGame puzzle =
  let size = puzzleSize puzzle
  in Game
     { gameBoard  = mkEmptyBoard size
     , gamePuzzle = puzzle
     , gameState  = Playing
     , startTime  = Nothing
     }

-- Alternar celda en posición (fila,col)
-- Secuencia: Unknown -> Filled -> MarkedEmpty -> Unknown
toggleCell :: Position -> Game -> Game
toggleCell (r, c) game@Game{ gameBoard = board, gamePuzzle = puzzle } =
  let (rows, cols) = puzzleSize puzzle
  in if r < 1 || r > rows || c < 1 || c > cols
     then game  -- fuera de rango
     else
       let zeroR = r - 1
           zeroC = c - 1
           oldRow = board !! zeroR
           oldCell = oldRow !! zeroC
           newCell = case oldCell of
             Unknown     -> Filled
             Filled      -> MarkedEmpty
             MarkedEmpty -> Unknown
           newRow = take zeroC oldRow ++ [newCell] ++ drop (zeroC + 1) oldRow
           newBoard = take zeroR board ++ [newRow] ++ drop (zeroR + 1) board
           provisionalGame = game { gameBoard = newBoard }
       in if checkVictory provisionalGame
          then provisionalGame { gameState = Won }
          else provisionalGame

-- Reiniciar el juego (tablero a Unknown, estado Playing)
resetGame :: Game -> Game
resetGame g@Game{ gamePuzzle = puzzle } = g { gameBoard = mkEmptyBoard (puzzleSize puzzle), gameState = Playing }

-- Calcular pistas de una línea (fila o columna) a partir del estado actual del tablero
-- Sólo cuentan celdas Filled
lineCluesFromCells :: [Cell] -> [Int]
lineCluesFromCells cells =
  let nums = map (\c -> if c == Filled then 1 else 0) cells
      groups = group nums
  in [ length g | g <- groups, head g == 1 ]

-- Obtener una fila por índice 1-based
getRow :: [[Cell]] -> Int -> [Cell]
getRow board r = board !! (r - 1)

-- Obtener una columna por índice 1-based
getCol :: [[Cell]] -> Int -> [Cell]
getCol board c = map (!! (c - 1)) board

-- Verifica si el puzzle está resuelto comparando con la solución
checkVictory :: Game -> Bool
checkVictory Game{ gameBoard = board, gamePuzzle = puzzle } =
  let sol = puzzleSolution puzzle
      rows = fst (puzzleSize puzzle)
      cols = snd (puzzleSize puzzle)
      allMatch = and
        [ let expected = (sol !! (r-1)) !! (c-1)
              actualIsFilled = (board !! (r-1)) !! (c-1) == Filled
          in expected == actualIsFilled
        | r <- [1..rows], c <- [1..cols]
        ]
  in allMatch

-- Exportar accesos a puzzles de ejemplo
puzzleFromSample5 :: Puzzle
puzzleFromSample5 = samplePuzzle5

puzzleFromSample10 :: Puzzle
puzzleFromSample10 = samplePuzzle10