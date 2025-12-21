module Nonogram.Types where

-- Tipos para Nonogramas (Picross)
-- Cell:
--   Unknown      = no marcada
--   Filled       = marcada como rellena (usuario o revelada)
--   MarkedEmpty  = marcada como vacía por el usuario (X)
--   LockedEmpty  = marcada automáticamente como vacía (X bloqueada, no se puede cambiar)
data Cell = Unknown | Filled | MarkedEmpty | LockedEmpty
  deriving (Eq, Show)

type Position = (Int, Int) -- (fila, columna), 1-based
type Size = (Int, Int)

-- Pistas: listas de enteros por fila/columna
-- puzzleSolution :: [[Bool]] donde True = celda que debe estar rellena
-- puzzleDifficulty: "easy" | "medium" | "hard"
data Puzzle = Puzzle
  { puzzleRows     :: [[Int]]
  , puzzleCols     :: [[Int]]
  , puzzleSize     :: Size
  , puzzleSolution :: [[Bool]]
  , puzzleDifficulty :: String
  } deriving (Show)

data GameState = Playing | Won | Lost
  deriving (Eq, Show)

data Game = Game
  { gameBoard  :: [[Cell]]   -- filas de celdas
  , gamePuzzle :: Puzzle
  , gameState  :: GameState
  , startTime  :: Maybe Int
  , errorCount :: Int        -- errores cometidos por el jugador
  , maxErrors  :: Int        -- máximo de errores permitidos (p. ej. 3)
  } deriving (Show)

-- Algunos puzzles embebidos (fallback)
samplePuzzle5 :: Puzzle
samplePuzzle5 = Puzzle
  { puzzleRows =
      [ [1]
      , [3]
      , [5]
      , [3]
      , [1]
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
  , puzzleDifficulty = "easy"
  }

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
  , puzzleDifficulty = "medium"
  }