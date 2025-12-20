{-# LANGUAGE OverloadedStrings #-}

module TicTacToe.View where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Data.IORef
import Control.Monad (void)
import Control.Concurrent (threadDelay)
import qualified Data.Maybe as Maybe

import Core.Types
import TicTacToe.Types
import TicTacToe.Game
import TicTacToe.Computer

--------------------------------------------------
-- Vista principal del minijuego
--------------------------------------------------

ticTacToeView :: Window -> IORef Screen -> UI () -> UI ()
ticTacToeView window screenRef renderApp = do
  void $ return window # set title "Tic Tac Toe"

  -- Estado del juego
  state <- liftIO $ newIORef initialState

  ------------------------------------------------
  -- UI Elements
  ------------------------------------------------

  backButton <- UI.button
    # set UI.text "← Volver al menú"
    # set UI.class_ "back-button"

  title <- UI.h1
    # set UI.text "Tic Tac Toe"
    # set UI.class_ "title"

  info <- UI.div
    # set UI.class_ "info"
    # set UI.text "Turno: X"

  modeButton <- UI.button
    # set UI.text "Modo: Humano vs Humano"
    # set UI.class_ "mode-button"

  resetButton <- UI.button
    # set UI.text "Nueva Partida"
    # set UI.class_ "reset-button"

  buttons <- mapM (\_ ->
      UI.button # set UI.text "" # set UI.class_ "cell"
    ) [1..9]

  ------------------------------------------------
  -- Render del juego
  ------------------------------------------------

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
                    then "¡Ganador: " ++ show p ++ "!"
                    else "¡Empate!"
                Nothing -> "Turno: " ++ show current

        element info # set UI.text message

        element modeButton # set UI.text
          (if mode == HumanVsHuman
            then "Modo: Humano vs Humano"
            else "Modo: Humano vs Computadora")

        mapM_ (\(btn, cell) -> do
            let cellText = showCell cell
                cellColor = case cell of
                  Just X -> "red"
                  Just O -> "blue"
                  _      -> "black"

            element btn
              # set UI.text cellText
              # set UI.style [("color", cellColor)]
              # set UI.enabled
                  ( Maybe.isNothing win &&
                    (mode == HumanVsHuman || current == X)
                  )
          ) (zip buttons boardState)

        element resetButton # set UI.style
          [("background-color",
            if Maybe.isJust win then "#4CAF50" else "#f44336")]

        when (isComputersTurn gs && Maybe.isNothing win) $ do
          liftIO $ threadDelay 400000
          liftIO $ modifyIORef state makeComputerMove
          renderGame

  ------------------------------------------------
  -- Eventos
  ------------------------------------------------

  mapM_ (\(i, btn) ->
      on UI.click btn $ \_ -> do
        gs <- liftIO $ readIORef state
        when (gameMode gs == HumanVsHuman || currentPlayer gs == X) $ do
          liftIO $ modifyIORef state (makeMove i)
          renderGame
    ) (zip [0..] buttons)

  on UI.click modeButton $ \_ -> do
    liftIO $ modifyIORef state
      (\gs -> gs { gameMode = switchMode (gameMode gs) })
    renderGame

  on UI.click resetButton $ \_ -> do
    liftIO $ writeIORef state initialState
    renderGame

  on UI.click backButton $ \_ -> do
    liftIO $ writeIORef screenRef Menu
    renderApp

  ------------------------------------------------
  -- Layout
  ------------------------------------------------

  let rowsUI =
        [ map pure (take 3 buttons)
        , map pure (take 3 (drop 3 buttons))
        , map pure (take 3 (drop 6 buttons))
        ]

  boardUI <- UI.grid rowsUI # set UI.class_ "board"

  controls <- UI.div # set UI.class_ "controls"
    #+ [ pure modeButton
       , pure resetButton
       ]

  container <- UI.div # set UI.class_ "container"
    #+ [ pure backButton
       , pure title
       , pure info
       , pure boardUI
       , pure controls
       ]

  ------------------------------------------------
  -- Inicial
  ------------------------------------------------

  renderGame
  void $ getBody window #+ [pure container]

--------------------------------------------------
-- Auxiliares
--------------------------------------------------

showCell :: Cell -> String
showCell Nothing  = ""
showCell (Just X) = "X"
showCell (Just O) = "O"

when :: Monad m => Bool -> m () -> m ()
when cond action = if cond then action else return ()
