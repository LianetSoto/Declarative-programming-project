module TicTacToe.Types where

-- Definir jugadores
data Player = X | O
  deriving (Eq, Show)

type Cell = Maybe Player
type Board = [Cell]  

-- Modo de juego
data GameMode = HumanVsHuman | HumanVsComputer
  deriving (Eq, Show)

-- Estado del juego
data GameState = GameState
  { board :: Board
  , currentPlayer :: Player
  , winner :: Maybe Player
  , gameMode :: GameMode
  } deriving (Show)
