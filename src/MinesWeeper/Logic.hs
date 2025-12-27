module MinesWeeper.Logic where

import MinesWeeper.Types
import MinesWeeper.Board
import Data.Array
import Data.List (foldl')
import Control.Monad.State
import System.Random

-- Revelar una celda individual
revealCell :: Position -> Game -> Game
revealCell pos game@Game{gameBoard = board, gameState = state}
    | state /= Playing = game
    | not (isValidPosition (boardSize game) pos) = game
    | otherwise =
        let cell = board ! pos
        in case cellState cell of
            Hidden -> processReveal pos game
            _      -> game  -- No hacer nada si ya esta revelada o con bandera

-- Procesar la revelacion 
processReveal :: Position -> Game -> Game
processReveal pos game@Game{gameBoard = board} =
    let cell = board ! pos
    in case cellContent cell of
        Mine ->
            let gameRevealed = revealAllMines game
            in gameRevealed { gameState = Lost pos }
        _    -> floodReveal pos game

-- algoritmo revelacion en cascada
floodReveal :: Position -> Game -> Game
floodReveal startPos game =
    let board = gameBoard game
        size = boardSize game
        
        -- Funcion recursiva de flood fill
        doFloodFill :: [Position] -> Board -> Board
        doFloodFill [] b = b
        doFloodFill (pos:queue) b
            | not (isValidPosition size pos) = doFloodFill queue b
            | otherwise =
                let cell = b ! pos
                in case cellState cell of
                    -- Solo procesar celdas ocultas que no sean minas
                    Hidden -> 
                        case cellContent cell of
                            Mine -> doFloodFill queue b  -- No revelar minas
                            _ ->
                                let newCell = cell { cellState = Revealed }
                                    newBoard = b // [(pos, newCell)]
                                    
                                    -- Si es una celda vacia, annadir vecinos
                                    nextQueue = if adjacentMines cell == 0
                                        then queue ++ getAdjacentPositions size pos
                                        else queue
                                in doFloodFill nextQueue newBoard
                    _ -> doFloodFill queue b
        
        -- Iniciar flood fill desde la posicion
        newBoard = doFloodFill [startPos] board

        -- Verificar si gano
        hasWon = checkWinCondition newBoard (mineCount game)

    in if hasWon
        then
            let gameWon = game { gameBoard = newBoard, gameState = Won }
            in revealAllMines gameWon
        else
            game { gameBoard = newBoard, gameState = Playing }

-- Verificar condicion de victoria
checkWinCondition :: Board -> Int -> Bool
checkWinCondition board totalMines =
    let cells = elems board
        totalCells = length cells
        revealedNonMines = length $ filter (\c -> cellContent c /= Mine && cellState c == Revealed) cells
    in revealedNonMines == (totalCells - totalMines)

-- Alternar bandera en una celda
toggleFlag :: Position -> Game -> Game
toggleFlag pos game@Game{gameBoard = board, gameState = state, flagsLeft = flags}
    | state /= Playing = game
    | not (isValidPosition (boardSize game) pos) = game
    | otherwise =
        let cell = board ! pos
            (newCell, newFlags) = case cellState cell of
                Hidden -> 
                    if flags > 0 
                    then (cell { cellState = Flagged }, flags - 1)
                    else (cell, flags)  -- No hay banderas disponibles
                Flagged -> (cell { cellState = Hidden }, flags + 1)
                _ -> (cell, flags)  -- No se puede poner bandera en celdas reveladas
            
            newBoard = board // [(pos, newCell)]
            newState = if checkWinCondition newBoard (mineCount game)
                      then Won else Playing
                      
        in game 
            { gameBoard = newBoard
            , gameState = newState
            , flagsLeft = newFlags
            }

-- Alternar signo de pregunta
toggleQuestion :: Position -> Game -> Game
toggleQuestion pos game@Game{gameBoard = board}
    | not (isValidPosition (boardSize game) pos) = game
    | otherwise =
        let cell = board ! pos
            newCell = case cellState cell of
                Hidden -> cell { cellState = Questioned }
                Questioned -> cell { cellState = Hidden }
                _ -> cell  -- No cambiar estado en otros casos
        in game { gameBoard = board // [(pos, newCell)] }

-- Revelar todas las minas (al perder)
revealAllMines :: Game -> Game
revealAllMines game@Game{gameBoard = board} =
    let minePositions = 
            [pos | pos <- indices board, 
                   cellContent (board ! pos) == Mine]
        newBoard = foldl' (\b pos -> 
            let cell = b ! pos
                newCell = cell { cellState = Revealed }
            in b // [(pos, newCell)]) board minePositions
    in game { gameBoard = newBoard }

-- Contar minas restantes
countRemainingMines :: Game -> Int
countRemainingMines game =
    let board = gameBoard game
        cells = elems board
        flaggedMines = length $ filter (\c -> 
            cellState c == Flagged && cellContent c == Mine) cells
    in mineCount game - flaggedMines

-- Obtener estadísticas del juego
getGameStats :: Game -> (Int, Int, Int)  -- (minas, banderas, celdas reveladas)
getGameStats game =
    let board = gameBoard game
        cells = elems board
        revealed = length $ filter (\c -> cellState c == Revealed) cells
        flagged = length $ filter (\c -> cellState c == Flagged) cells
    in (mineCount game, flagged, revealed)