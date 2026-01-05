{-# LANGUAGE OverloadedStrings #-}

module TicTacToe.View where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Data.IORef
import Control.Monad (void, when)
import Control.Concurrent (threadDelay)
import qualified Data.Maybe as Maybe

import Core.Types
import TicTacToe.Types
import TicTacToe.Game
import TicTacToe.Computer

ticTacToeView :: Window -> IORef Screen -> UI () -> UI ()
ticTacToeView window screenRef renderApp = do
  void $ return window # set title "Tic Tac Toe"
  
  ---------------------------------------------------------
  -- LIMPIAR BODY PRIMERO
  ---------------------------------------------------------
  body <- getBody window
  element body # set children []
  
  ---------------------------------------------------------
  -- INSERTAR CSS
  ---------------------------------------------------------
  let styles = unlines
        [ "* { margin: 0; padding: 0; box-sizing: border-box; }"
        , "body {"
        , "  font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;"
        , "  background: linear-gradient(135deg, #0f0c29 0%, #302b63 50%, #24243e 100%);"
        , "  min-height: 100vh;"
        , "  display: flex;"
        , "  align-items: center;"
        , "  justify-content: center;"
        , "  padding: 20px;"
        , "}"
        , ".container { max-width: 800px; width: 100%; }"
        , ".game-container {"
        , "  background: rgba(255, 255, 255, 0.05);"
        , "  backdrop-filter: blur(10px);"
        , "  border-radius: 20px;"
        , "  padding: 40px;"
        , "  box-shadow: 0 20px 60px rgba(0, 0, 0, 0.5), 0 0 40px rgba(138, 43, 226, 0.2);"
        , "  border: 1px solid rgba(255, 255, 255, 0.1);"
        , "}"
        , ".game-title {"
        , "  text-align: center;"
        , "  color: #fff;"
        , "  font-size: 2.5em;"
        , "  margin-bottom: 30px;"
        , "  text-shadow: 0 0 20px rgba(138, 43, 226, 0.8), 0 0 40px rgba(138, 43, 226, 0.5);"
        , "  font-weight: bold;"
        , "}"
        , ".turn-info {"
        , "  text-align: center;"
        , "  color: #00d4ff;"
        , "  font-size: 1.5em;"
        , "  margin-bottom: 30px;"
        , "  font-weight: 600;"
        , "  text-shadow: 0 0 10px rgba(0, 212, 255, 0.7);"
        , "}"
        , ".board-container {"
        , "  display: flex;"
        , "  flex-direction: column;"
        , "  align-items: center;"
        , "  margin-bottom: 40px;"
        , "}"
        , ".board-row {"
        , "  display: flex;"
        , "  flex-direction: row;"
        , "  justify-content: center;"
        , "}"
        , ".cell {"
        , "  width: 100px;"
        , "  height: 100px;"
        , "  margin: 8px;"
        , "  font-size: 48px;"
        , "  font-weight: bold;"
        , "  border-radius: 12px;"
        , "  cursor: pointer;"
        , "  background: rgba(255, 255, 255, 0.05);"
        , "  border: 2px solid rgba(138, 43, 226, 0.5);"
        , "  display: flex;"
        , "  align-items: center;"
        , "  justify-content: center;"
        , "  transition: all 0.3s ease;"
        , "  user-select: none;"
        , "}"
        , ".cell:hover {"
        , "  border-color: rgba(138, 43, 226, 0.8);"
        , "  box-shadow: 0 0 20px rgba(138, 43, 226, 0.4);"
        , "}"
        , ".cell-x {"
        , "  color: #ff6b9d;"
        , "  text-shadow: 0 0 15px rgba(255, 107, 157, 0.8);"
        , "}"
        , ".cell-o {"
        , "  color: #00d4ff;"
        , "  text-shadow: 0 0 15px rgba(0, 212, 255, 0.8);"
        , "}"
        , ".buttons-container {"
        , "  display: flex;"
        , "  flex-direction: column;"
        , "  align-items: center;"
        , "  gap: 20px;"
        , "  margin-top: 30px;"
        , "}"
        , ".game-button {"
        , "  background: linear-gradient(135deg, rgba(138, 43, 226, 0.2) 0%, rgba(75, 0, 130, 0.3) 100%);"
        , "  border: 2px solid rgba(138, 43, 226, 0.5);"
        , "  border-radius: 15px;"
        , "  padding: 15px 40px;"
        , "  color: #fff;"
        , "  font-size: 1.1em;"
        , "  font-weight: 600;"
        , "  cursor: pointer;"
        , "  transition: all 0.3s ease;"
        , "  box-shadow: 0 10px 30px rgba(0, 0, 0, 0.3);"
        , "  width: 300px;"
        , "}"
        , ".game-button:hover {"
        , "  transform: translateY(-3px);"
        , "  box-shadow: 0 15px 40px rgba(138, 43, 226, 0.4), 0 0 30px rgba(138, 43, 226, 0.3);"
        , "  border-color: rgba(138, 43, 226, 0.8);"
        , "  background: linear-gradient(135deg, rgba(138, 43, 226, 0.3) 0%, rgba(75, 0, 130, 0.4) 100%);"
        , "}"
        , ".back-button {"
        , "  background: linear-gradient(135deg, rgba(255, 215, 0, 0.2) 0%, rgba(255, 140, 0, 0.3) 100%);"
        , "  border: 2px solid rgba(255, 215, 0, 0.5);"
        , "  border-radius: 15px;"
        , "  padding: 15px 40px;"
        , "  color: #ffd700;"
        , "  font-size: 1.1em;"
        , "  font-weight: 600;"
        , "  cursor: pointer;"
        , "  transition: all 0.3s ease;"
        , "  box-shadow: 0 10px 30px rgba(0, 0, 0, 0.3);"
        , "  width: 300px;"
        , "}"
        , ".back-button:hover {"
        , "  transform: translateY(-3px);"
        , "  box-shadow: 0 15px 40px rgba(255, 215, 0, 0.3);"
        , "  border-color: rgba(255, 215, 0, 0.8);"
        , "}"
        ]

  styleEl <- UI.div # set UI.html ("<style>" ++ styles ++ "</style>")
  void $ element body #+ [pure styleEl]

  -- Game state
  state <- liftIO $ newIORef initialState

  ---------------------------------------------------------
  -- UI Elements (con clases CSS)
  ---------------------------------------------------------
  title <- UI.h1
    # set UI.text "TIC TAC TOE"
    # set UI.class_ "game-title"
    
  turnInfo <- UI.div
    # set UI.text "Turno: X"
    # set UI.class_ "turn-info"
    
  modeButton <- UI.button
    # set UI.text "Modo: Humano vs Humano"
    # set UI.class_ "game-button"
    
  resetButton <- UI.button
    # set UI.text "Nuevo Juego"
    # set UI.class_ "game-button"
    
  backButton <- UI.button
    # set UI.text "← Volver al Menú"
    # set UI.class_ "back-button"
    
  cells <- mapM (\_ -> UI.div # set UI.class_ "cell") [1..9]

  ---------------------------------------------------------
  -- Game Rendering
  ---------------------------------------------------------
  let renderGame :: UI ()
      renderGame = do
        gs <- liftIO $ readIORef state
        
        let current    = currentPlayer gs
            win        = winner gs
            boardState = board gs
            mode       = gameMode gs
            
            message =
              case win of
                Just p ->
                  if Maybe.isJust (checkWinner boardState)
                    then "Ganador: " ++ show p ++ "!"
                    else "Empate!"
                Nothing -> "Turno: " ++ show current
        
        element turnInfo # set UI.text message
        
        element modeButton # set UI.text
          (if mode == HumanVsHuman
            then "Modo: Humano vs Humano"
            else "Modo: Humano vs Computadora")
        
        mapM_ (\(cellDiv, cell) -> do
            let cellText = showCell cell
                cellClass = case cell of
                  Just X -> "cell cell-x"
                  Just O -> "cell cell-o"
                  Nothing -> "cell"
            
            element cellDiv
              # set UI.text cellText
              # set UI.class_ cellClass
          ) (zip cells boardState)
        
        when (isComputersTurn gs && Maybe.isNothing win) $ do
          liftIO $ threadDelay 400000
          liftIO $ modifyIORef state makeComputerMove
          renderGame

  ---------------------------------------------------------
  -- Events
  ---------------------------------------------------------
  mapM_ (\(i, cellDiv) ->
      on UI.click cellDiv $ \_ -> do
        gs <- liftIO $ readIORef state
        when (gameMode gs == HumanVsHuman || currentPlayer gs == X) $ do
          liftIO $ modifyIORef state (makeMove i)
          renderGame
    ) (zip [0..] cells)

  on UI.click modeButton $ \_ -> do
    liftIO $ modifyIORef state
      (\gs -> gs { gameMode = switchMode (gameMode gs) })
    renderGame

  on UI.click resetButton $ \_ -> do
    -- Reiniciar el juego manteniendo el modo actual
    liftIO $ modifyIORef state $ \currentState ->
      currentState { 
        board = emptyBoard,
        currentPlayer = X,
        winner = Nothing
      }
    renderGame

  on UI.click backButton $ \_ -> do
    liftIO $ writeIORef screenRef Menu
    renderApp

  ---------------------------------------------------------
  -- Layout
  ---------------------------------------------------------
  boardRows <- mapM (\i -> do
      let rowCells = take 3 (drop (i*3) cells)
      UI.div # set UI.class_ "board-row" #+ map pure rowCells
    ) [0..2]
    
  boardContainer <- UI.div 
    # set UI.class_ "board-container"
    #+ map pure boardRows
    
  buttonsContainer <- UI.div
    # set UI.class_ "buttons-container"
    #+ [ pure modeButton
       , pure resetButton
       , pure backButton
       ]
    
  gameContainer <- UI.div 
    # set UI.class_ "game-container"
    #+ [ pure title
       , pure turnInfo
       , pure boardContainer
       , pure buttonsContainer
       ]
    
  container <- UI.div 
    # set UI.class_ "container"
    #+ [ pure gameContainer ]
    
  -- Initial Render
  renderGame
  void $ element body #+ [pure container]

-- Helpers

showCell :: Cell -> String
showCell Nothing  = ""
showCell (Just X) = "X"
showCell (Just O) = "O"

-- Tablero vacío para reiniciar
emptyBoard :: Board
emptyBoard = replicate 9 Nothing