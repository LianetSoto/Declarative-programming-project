module MinesWeeper.Types
  ( Position, BoardSize
  , CellState(..), CellContent(..), Cell(..)
  , Board, GameState(..), Game(..)
  , PlayerAction(..), Difficulty(..), Config(..)
  ) where
    
import Data.Array

-- Tipos basicos
type Position = (Int, Int)
type BoardSize = (Int, Int)  -- (filas, columnas)

-- Estado de una celda
data CellState = Hidden | Revealed | Flagged | Questioned
    deriving (Eq, Show, Enum)

-- Contenido de una celda
data CellContent = Empty | Mine | Number Int
    deriving (Eq, Show)

-- Celda completa
data Cell = Cell
    { cellState :: CellState
    , cellContent :: CellContent
    , adjacentMines :: Int
    }
    deriving (Eq, Show)

-- Tablero como array bidimensional
type Board = Array Position Cell

-- Estado del juego
data GameState = 
    Playing
    | Won
    | Lost Position  -- Posicion de la mina que hizo perder
    deriving (Eq, Show)

-- Juego completo
data Game = Game
    { gameBoard :: Board
    , gameState :: GameState
    , boardSize :: BoardSize
    , mineCount :: Int
    , flagsLeft :: Int
    , startTime :: Maybe Int  -- Tiempo de inicio en segundos
    }
    deriving (Show)

-- Accion del jugador
data PlayerAction = 
    RevealCell Position
    | ToggleFlag Position
    | ToggleQuestion Position
    | Quit
    | Restart
    | Help
    deriving (Eq, Show)

-- Dificultad predefinida
data Difficulty = 
    Beginner      -- 9x9, 10 minas
    | Intermediate -- 16x16, 40 minas
    | Expert       -- 16x30, 99 minas
    | Custom Int Int Int  -- filas, columnas, minas
    deriving (Eq, Show)

-- Configuracion inicial
data Config = Config
    { configDifficulty :: Difficulty
    , configSeed :: Maybe Int  -- Semilla para reproducción
    }
    deriving (Show)