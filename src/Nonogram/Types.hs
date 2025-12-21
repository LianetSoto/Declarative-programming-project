module Nonogram.Types where

-- Tipos para Nonogramas (Picross)
-- Celda: Unknown = no marcada; Filled = marcada como rellena; MarkedEmpty = marcada como vacía (X)
data Cell = Unknown | Filled | MarkedEmpty
  deriving (Eq, Show)

type Position = (Int, Int) -- (fila, columna), 1-based
type Size = (Int, Int)

-- Pistas: listas de enteros por fila/columna
-- Añadimos puzzleSolution :: [[Bool]] donde True = celda que debe estar rellena
data Puzzle = Puzzle
  { puzzleRows     :: [[Int]]
  , puzzleCols     :: [[Int]]
  , puzzleSize     :: Size
  , puzzleSolution :: [[Bool]]
  } deriving (Show)

data GameState = Playing | Won
  deriving (Eq, Show)

data Game = Game
  { gameBoard  :: [[Cell]]   -- filas de celdas
  , gamePuzzle :: Puzzle
  , gameState  :: GameState
  , startTime  :: Maybe Int
  } deriving (Show)

-- Algunos puzzles de ejemplo
-- 5x5 "cruz" (solución y pistas consistentes)
samplePuzzle5 :: Puzzle
samplePuzzle5 = Puzzle
  { puzzleRows =
      [ [1]     -- 00100
      , [3]     -- 01110
      , [5]     -- 11111
      , [3]     -- 01110
      , [1]     -- 00100
      ]
  , puzzleCols =
      [ [1]
      , [3]
      , [5]
      , [3]
      , [1]
      ]
  , puzzleSize = (5,5)
  , puzzleSolution =
      [ [False, False, True , False, False]
      , [False, True , True , True , False]
      , [True , True , True , True , True ]
      , [False, True , True , True , False]
      , [False, False, True , False, False]
      ]
  }

-- 10x10 ejemplo simple (simétrico). Asegúrate de que pistas coinciden con solución.
samplePuzzle10 :: Puzzle
samplePuzzle10 = Puzzle
  { puzzleRows =
      [ [2]
      , [1,1]
      , [1,1,1]
      , [1,1,1]
      , [6]
      , [6]
      , [1,1,1]
      , [1,1,1]
      , [1,1]
      , [2]
      ]
  , puzzleCols =
      [ [2]
      , [1,1]
      , [1,1,1]
      , [1,1,1]
      , [6]
      , [6]
      , [1,1,1]
      , [1,1,1]
      , [1,1]
      , [2]
      ]
  , puzzleSize = (10,10)
  , puzzleSolution =
      -- Aquí un patrón simétrico simple; puedes sustituir por cualquier otro que cumpla las pistas
      [ [False,False,False,False,True ,True ,False,False,False,False]
      , [False,False,False,True ,False,False,True ,False,False,False]
      , [False,False,True ,False,False,False,False,True ,False,False]
      , [False,False,True ,False,False,False,False,True ,False,False]
      , [True ,True ,True ,True ,True ,True ,False,False,False,False]
      , [True ,True ,True ,True ,True ,True ,False,False,False,False]
      , [False,False,True ,False,False,False,False,True ,False,False]
      , [False,False,True ,False,False,False,False,True ,False,False]
      , [False,False,False,True ,False,False,True ,False,False,False]
      , [False,False,False,False,True ,True ,False,False,False,False]
      ]
  }