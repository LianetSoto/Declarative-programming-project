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

  -- Game state
  state <- liftIO $ newIORef initialState

 -- UI Elements
  backButton <- UI.button
    # set UI.text "← Back"

  turnInfo <- UI.div
    # set UI.text "Turn: X"

  modeButton <- UI.button
    # set UI.text "Mode: Human vs Human"

  resetButton <- UI.button
    # set UI.text "New Game"

  cells <- mapM (\_ -> UI.div # set UI.class_ "cell") [1..9]

 -- Styles
  let containerStyle =
        [ ("background", "#000000")
        , ("padding", "50px")
        , ("border-radius", "25px")
        , ("box-shadow", "0 0 50px #00ffff, 0 0 80px #ff00ff")
        , ("display", "flex")
        , ("flex-direction", "column")
        , ("align-items", "center")
        , ("justify-content", "center")
        , ("min-width", "350px")
        ]

      infoStyle =
        [ ("color", "#00ffff")
        , ("margin-bottom", "20px")
        , ("font-size", "18px")
        , ("text-shadow", "0 0 10px #00ffff")
        , ("font-family", "'Orbitron', sans-serif")
        ]

      buttonStyle =
        [ ("background", "transparent")
        , ("color", "#00ffff")
        , ("border", "2px solid #00ffff")
        , ("padding", "10px 30px")
        , ("margin", "10px")
        , ("cursor", "pointer")
        , ("border-radius", "12px")
        , ("font-family", "'Orbitron', sans-serif")
        , ("font-size", "16px")
        , ("text-shadow", "0 0 5px #00ffff")
        , ("box-shadow", "0 0 15px #00ffff")
        ]

      cellStyle =
        [ ("width", "70px")
        , ("height", "70px")
        , ("margin", "5px")
        , ("font-size", "32px")
        , ("font-weight", "bold")
        , ("border-radius", "12px")
        , ("cursor", "pointer")
        , ("background", "#111111")
        , ("text-align", "center")
        , ("box-shadow", "0 0 15px #00ffff")
        , ("user-select", "none")
        , ("position", "relative")
        , ("display", "flex")
        , ("align-items", "center")
        , ("justify-content", "center")
        ]

      boardStyle =
        [ ("display", "flex")
        , ("flex-direction", "column")
        , ("align-items", "center")
        ]

      rowStyle =
        [ ("display", "flex")
        , ("flex-direction", "row")
        , ("justify-content", "center")
        ]

  -- Apply styles
  mapM_ (\c -> element c # set UI.style cellStyle) cells
  element turnInfo    # set UI.style infoStyle
  element modeButton  # set UI.style buttonStyle
  element resetButton # set UI.style buttonStyle
  element backButton  # set UI.style buttonStyle

 -- Game Rendering
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
                    then "Winner: " ++ show p
                    else "Draw!"
                Nothing -> "Turn: " ++ show current

        element turnInfo # set UI.text message

        element modeButton # set UI.text
          (if mode == HumanVsHuman
            then "Mode: Human vs Human"
            else "Mode: Human vs Computer")

        mapM_ (\(cellDiv, cell) -> do
            let cellText = showCell cell
                cellColor = case cell of
                  Just X -> "#ff0000"
                  Just O -> "#ff4000ff"
                  Nothing -> "#ffaa00"
                glow = case cell of
                  Just X -> "0 0 20px #ff0000"
                  Just O -> "0 0 20px #ffaa00"
                  Nothing -> "0 0 15px #00ffff"

            element cellDiv
              # set UI.text cellText
              # set UI.style [("color", cellColor), ("box-shadow", glow)]
          ) (zip cells boardState)

        when (isComputersTurn gs && Maybe.isNothing win) $ do
          liftIO $ threadDelay 400000
          liftIO $ modifyIORef state makeComputerMove
          renderGame

 -- Events
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
    liftIO $ writeIORef state initialState
    renderGame

  on UI.click backButton $ \_ -> do
    liftIO $ writeIORef screenRef Menu
    renderApp

 -- Layout
  boardRows <- mapM (\i -> do
      let rowCells = take 3 (drop (i*3) cells)
      UI.div # set UI.style rowStyle #+ map pure rowCells
    ) [0..2]

  boardUI <- UI.div # set UI.style boardStyle #+ map pure boardRows

  buttonsContainer <- UI.div
        # set UI.style [("display", "flex")
                       ,("flex-direction", "column")
                       ,("align-items", "center")
                       ,("margin-top", "20px")]
        #+ [pure modeButton, pure resetButton, pure backButton]

  container <- UI.div # set UI.style containerStyle
        #+ [ pure turnInfo
           , pure boardUI
           , pure buttonsContainer
           ]

 -- Initial Render
  renderGame
  void $ getBody window #+ [pure container]

-- Helpers

showCell :: Cell -> String
showCell Nothing  = ""
showCell (Just X) = "X"
showCell (Just O) = "O"
