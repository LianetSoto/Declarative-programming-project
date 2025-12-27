module MinesWeeper.Board where

import MinesWeeper.Types
import Data.Array
import System.Random
import Data.List (nub)

-- Crear un tablero vacío
createEmptyBoard :: BoardSize -> Board
createEmptyBoard (rows, cols) = 
    array ((1,1), (rows, cols))
        [ ((r, c), Cell Hidden Empty 0)
        | r <- [1..rows]
        , c <- [1..cols]
        ]

-- Generar posiciones aleatorias para minas
generateMinePositions :: BoardSize -> Int -> StdGen -> ([Position], StdGen)
generateMinePositions (rows, cols) numMines gen =
    let totalCells = rows * cols
        positions = [(r, c) | r <- [1..rows], c <- [1..cols]]
        (indices, newGen) = takeUniqueIndices numMines totalCells gen
        minePositions = map (positions !!) indices
    in (minePositions, newGen)
  where
    takeUniqueIndices :: Int -> Int -> StdGen -> ([Int], StdGen)
    takeUniqueIndices n maxIdx g = go n [] g
      where
        go 0 acc g = (acc, g)
        go k acc g =
            let (idx, g') = randomR (0, maxIdx-1) g
            in if idx `elem` acc
                then go k acc g'
                else go (k-1) (idx:acc) g'

-- Calcular minas adyacentes para una posición
calculateAdjacentMines :: [Position] -> Position -> Int
calculateAdjacentMines mines pos =
    length $ filter isAdjacent mines
  where
    (r, c) = pos
    adjacentPositions = 
        [(r+dr, c+dc) | dr <- [-1..1], dc <- [-1..1], (dr, dc) /= (0,0)]
    isAdjacent minePos = minePos `elem` adjacentPositions

-- Crear un tablero con minas
createBoardWithMines :: BoardSize -> [Position] -> Board
createBoardWithMines size@(rows, cols) minePositions =
    let emptyBoard = createEmptyBoard size
        boardWithMines = placeMines emptyBoard minePositions
        boardWithNumbers = calculateAllNumbers boardWithMines minePositions
    in boardWithNumbers
  where
    placeMines :: Board -> [Position] -> Board
    placeMines board mines = 
        board // [(pos, Cell Hidden Mine 0) | pos <- mines]
    
    calculateAllNumbers :: Board -> [Position] -> Board
    calculateAllNumbers board mines =
        board // [((r, c), updateCell (r, c)) 
                | r <- [1..rows], c <- [1..cols]]
      where
        updateCell pos
            | pos `elem` mines = board ! pos
            | otherwise = 
                let count = calculateAdjacentMines mines pos
                in if count > 0
                    then Cell Hidden (Number count) count
                    else Cell Hidden Empty 0

-- Inicializar un nuevo juego (CORREGIDO)
initializeGame :: Difficulty -> Maybe Int -> IO Game
initializeGame diff customMines = do
    let (size, defaultMines) = case diff of
            Beginner     -> ((9, 9), 10)
            Intermediate -> ((16, 16), 40)
            Expert       -> ((16, 30), 99)
            Custom r c m -> ((r, c), m)

        -- ⭐ Si customMines tiene un número → usarlo
        mines = case customMines of
            Just m  -> m
            Nothing -> defaultMines

    randomGen <- newStdGen

    let (minePositions, _) = generateMinePositions size mines randomGen
        board = createBoardWithMines size minePositions

    return $ Game
        { gameBoard = board
        , gameState = Playing
        , boardSize = size
        , mineCount = mines
        , flagsLeft = mines
        , startTime = Nothing
        }

-- Verificar si una posición es válida
isValidPosition :: BoardSize -> Position -> Bool
isValidPosition (rows, cols) (r, c) =
    r >= 1 && r <= rows && c >= 1 && c <= cols

-- Obtener todas las posiciones adyacentes
getAdjacentPositions :: BoardSize -> Position -> [Position]
getAdjacentPositions size (r, c) =
    filter (isValidPosition size) 
        [(r+dr, c+dc) | dr <- [-1..1], dc <- [-1..1], (dr, dc) /= (0,0)]

-- Obtener celdas adyacentes
getAdjacentCells :: Board -> Position -> [(Position, Cell)]
getAdjacentCells board pos =
    let (rows, cols) = snd $ bounds board
        size = (rows, cols)
        adjacentPositions = getAdjacentPositions size pos
    in [(p, board ! p) | p <- adjacentPositions]
