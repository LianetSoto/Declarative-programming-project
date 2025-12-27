{-# LANGUAGE OverloadedStrings #-}

module MinesWeeper.View where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Data.IORef
import Control.Monad (void, forM, forM_, when)
import Data.Array
import qualified Data.Maybe as Maybe

import Core.Types
import MinesWeeper.Types
import MinesWeeper.Logic
import MinesWeeper.Board

--------------------------------------------------
-- Tipo para modo de acción
--------------------------------------------------
data ActionMode = Reveal | Flag
  deriving (Eq, Show)

--------------------------------------------------
-- Vista principal del Buscaminas
--------------------------------------------------

minesweeperView :: Window -> IORef Screen -> UI () -> UI ()
minesweeperView window screenRef renderApp = do
  void $ return window # set title "Buscaminas"

  addStyles window

  game0 <- liftIO $ initializeGame Beginner Nothing
  state <- liftIO $ newIORef game0
  actionMode <- liftIO $ newIORef Reveal

  ------------------------------------------------
  -- UI Elements
  ------------------------------------------------

  backButton <- UI.button
    # set UI.text "← Volver"
    # set UI.class_ "back-button"

  title <- UI.h1
    # set UI.text "Buscaminas"
    # set UI.class_ "game-title"

  info <- UI.div
    # set UI.class_ "game-info"
    # set UI.text "Minas restantes: 10"

  status <- UI.div
    # set UI.class_ "game-status"
    # set UI.text "JUGANDO"

  modeButton <- UI.button
    # set UI.text "Modo: Revelar"
    # set UI.class_ "game-mode-button"

  newGameButton <- UI.button
    # set UI.text "Nuevo Juego"
    # set UI.class_ "game-reset-button"


  ------------------------------------------------
  -- Crear botones del tablero
  ------------------------------------------------
  let (rows, cols) = boardSize game0
      positions = [(r, c) | r <- [1..rows], c <- [1..cols]]

  buttons <- forM positions $ \(r, c) ->
    UI.button
      # set UI.text ""
      # set UI.class_ "mine-cell hidden"
      # set (attr "data-row") (show r)
      # set (attr "data-col") (show c)

  ------------------------------------------------
  -- Render del juego
  ------------------------------------------------
  let renderGame :: UI ()
      renderGame = do
        gs <- liftIO $ readIORef state
        mode <- liftIO $ readIORef actionMode

        let minesLeft = flagsLeft gs
            gameStatus = case gameState gs of
              Playing -> "JUGANDO"
              Won -> "¡GANASTE!"
              Lost _ -> "¡PERDISTE!"

        element info # set UI.text 
          ("Minas restantes: " ++ 
            (case gameState gs of 
              Won -> "0" 
              _ -> show minesLeft))

        element status # set UI.text gameStatus

        element status # set UI.style
          [ ("color", case gameState gs of
                Won -> "#4CAF50"
                Lost _ -> "#ff4d4d"
                _ -> "#00d4ff")
          ]

        element modeButton # set UI.text
          (case mode of
              Reveal -> "Modo: Revelar"
              Flag   -> "Modo: Bandera")

        forM_ (zip buttons (indices (gameBoard gs))) $ \(btn, pos) -> do
          let cell = gameBoard gs ! pos
          let (cellText, cellClass, enabled, style) =
                renderCell cell (gameState gs)

          element btn
            # set UI.text cellText
            # set UI.class_ ("mine-cell " ++ cellClass)
            # set UI.enabled (enabled && gameState gs == Playing)
            # set UI.style style

  ------------------------------------------------
  -- Eventos
  ------------------------------------------------

  forM_ (zip buttons positions) $ \(btn, pos) ->
    on UI.click btn $ \_ -> do
      mode <- liftIO $ readIORef actionMode
      gs <- liftIO $ readIORef state
      when (gameState gs == Playing) $ do
        liftIO $ modifyIORef state $ \st ->
          case mode of
            Reveal -> revealCell pos st
            Flag   -> toggleFlag pos st
        renderGame

  on UI.click modeButton $ \_ -> do
    liftIO $ modifyIORef actionMode (\m -> if m == Reveal then Flag else Reveal)
    renderGame

  on UI.click newGameButton $ \_ -> do
    gameNew <- liftIO $ initializeGame Beginner Nothing
    liftIO $ writeIORef state gameNew
    liftIO $ writeIORef actionMode Reveal
    renderGame


  on UI.click backButton $ \_ -> do
    liftIO $ writeIORef screenRef Menu
    renderApp

  ------------------------------------------------
  -- Layout
  ------------------------------------------------

  let rowsUI = chunksOf cols buttons
      chunksOf n [] = []
      chunksOf n xs = take n xs : chunksOf n (drop n xs)

  boardRows <- forM rowsUI $ \rowButtons -> do
    rowDiv <- UI.div # set UI.class_ "mine-row"
    element rowDiv #+ map pure rowButtons
    return rowDiv

  boardUI <- UI.div # set UI.class_ "mine-board"
  element boardUI #+ map pure boardRows

  controlsRow <- UI.div # set UI.class_ "controls-row"
    #+ [ pure modeButton
       , pure newGameButton
       ]

  container <- UI.div # set UI.class_ "game-container"
    #+ [ pure backButton
       , pure title
       , pure info
       , pure status
       , pure boardUI
       , pure controlsRow
       ]

  renderGame
  void $ getBody window #+ [pure container]

--------------------------------------------------
-- Estilos ajustados
--------------------------------------------------

addStyles :: Window -> UI ()
addStyles window = do
  let styles = unlines
        [ "body {"
        , "  background: linear-gradient(135deg, #0f0c29 0%, #302b63 50%, #24243e 100%) !important;"
        , "  font-family: 'Segoe UI', sans-serif !important;"
        , "  color: white;"
        , "  overflow: hidden;"
        , "}"
        , ".game-container {"
        , "  background: rgba(255,255,255,0.05);"
        , "  backdrop-filter: blur(10px);"
        , "  border-radius: 20px;"
        , "  padding: 15px;"
        , "  width: 85%;"
        , "  max-width: 750px;"
        , "  margin: 10px auto;"
        , "  box-shadow: 0 20px 60px rgba(0,0,0,0.5), 0 0 40px rgba(138,43,226,0.2);"
        , "  border: 1px solid rgba(255,255,255,0.1);"
        , "}"
        , ".game-title {"
        , "  font-size: 2.2em;"
        , "  text-align: center;"
        , "  margin: 5px 0;"
        , "  text-shadow: 0 0 20px rgba(138,43,226,0.8);"
        , "}"
        , ".game-info, .game-status {"
        , "  text-align: center;"
        , "  font-size: 1.1em;"
        , "  margin: 5px 0;"
        , "}"
        , ".back-button {"
        , "  background: linear-gradient(135deg, rgba(138,43,226,0.2), rgba(75,0,130,0.3));"
        , "  border: 2px solid rgba(138,43,226,0.5);"
        , "  color: white;"
        , "  padding: 8px 15px;"
        , "  border-radius: 10px;"
        , "  cursor: pointer;"
        , "  margin-bottom: 10px;"
        , "  font-size: 0.9em;"
        , "}"
        , ".controls-row {"
        , "  display: flex;"
        , "  justify-content: center;"
        , "  align-items: center;"
        , "  gap: 12px;"
        , "  margin-top: 10px;"
        , "  flex-wrap: nowrap;"
        , "}"
        , ".game-mode-button, .game-reset-button, .game-help-button {"
        , "  background: linear-gradient(135deg, rgba(138,43,226,0.2), rgba(75,0,130,0.3));"
        , "  border: 2px solid rgba(138,43,226,0.5);"
        , "  color: white;"
        , "  padding: 10px 15px;"
        , "  border-radius: 12px;"
        , "  cursor: pointer;"
        , "  font-size: 0.95em;"
        , "  box-shadow: 0 10px 30px rgba(0,0,0,0.3);"
        , "}"
        , ".mine-board {"
        , "  display: flex;"
        , "  flex-direction: column;"
        , "  gap: 5px;"
        , "  margin: 10px auto;"
        , "  padding: 10px;"
        , "  background: rgba(255,255,255,0.05);"
        , "  border-radius: 12px;"
        , "  backdrop-filter: blur(6px);"
        , "  box-shadow: 0 10px 40px rgba(0,0,0,0.4);"
        , "  align-items: center;"
        , "}"
        , ".mine-row {"
        , "  display: flex;"
        , "  gap: 5px;"
        , "  justify-content: center;"
        , "}"
        , ".mine-cell {"
        , "  width: 50px;"
        , "  height: 50px;"
        , "  border-radius: 10px;"
        , "  font-size: 1.3em;"
        , "  font-weight: bold;"
        , "  cursor: pointer;"
        , "  transition: 0.2s;"
        , "  display: flex;"
        , "  align-items: center;"
        , "  justify-content: center;"
        , "  background: rgba(138,43,226,0.15);"
        , "  border: 2px solid rgba(138,43,226,0.4);"
        , "  color: white;"
        , "}"
        , ".mine-cell.hidden:hover {"
        , "  transform: scale(1.1);"
        , "  background: rgba(138,43,226,0.3);"
        , "}"
        , ".mine-cell.revealed {"
        , "  background: transparent !important;"
        , "  border: none !important;"
        , "  box-shadow: none !important;"
        , "  cursor: default !important;"
        , "}"
        , ".mine-cell.revealed.number {"
        , "  background: transparent !important;"
        , "  border: none !important;"
        , "  box-shadow: none !important;"
        , "}"
        , ".mine-cell.mine.revealed {"
        , "  background: #ff4d4d !important;"
        , "  color: white !important;"
        , "}"
        , ".mine-cell.flagged {"
        , "  background: rgba(255,215,0,0.4);"
        , "  color: #ffd700;"
        , "}"
        ]

  body <- getBody window
  styleEl <- UI.div # set UI.html ("<style>" ++ styles ++ "</style>")
  void $ element body #+ [pure styleEl]

--------------------------------------------------
-- Renderizar celdas
--------------------------------------------------

renderCell :: Cell -> GameState -> (String, String, Bool, [(String, String)])
renderCell cell gameState =
  case cellState cell of
    Hidden -> ("", "hidden", True, [])
    Revealed ->
      case cellContent cell of
        Mine -> ("💣", "mine revealed", False, [])
        Empty -> ("", "revealed", False, [])
        Number n ->
          let color = case n of
                1 -> "#00d4ff"
                2 -> "#4dff4d"
                3 -> "#ff4d4d"
                4 -> "#b366ff"
                5 -> "#ff944d"
                6 -> "#4dffff"
                7 -> "#ffffff"
                8 -> "#cccccc"
          in (show n, "revealed number", False, [("color", color)])
    Flagged -> ("🚩", "flagged", True, [])
    Questioned -> ("?", "flagged", True, [])

