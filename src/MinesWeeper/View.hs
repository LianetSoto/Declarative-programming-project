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
-- Vista principal del Buscaminas
--------------------------------------------------

minesweeperView :: Window -> IORef Screen -> UI () -> UI ()
minesweeperView window screenRef renderApp = do
  void $ return window # set title "Buscaminas"
  
  -- Agregar estilos CSS
  addStyles window

  -- Estado del juego (inicializar con minas reales)
  game0 <- liftIO $ initializeGame Beginner Nothing
  state <- liftIO $ newIORef game0

  -- Modo de acción
  actionMode <- liftIO $ newIORef Reveal

  ------------------------------------------------
  -- UI Elements
  ------------------------------------------------

  backButton <- UI.button
    # set UI.text "← Volver al menú"
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
    # set UI.text "Modo: Revelar (Clic Izquierdo)"
    # set UI.class_ "game-mode-button"

  -- (Se eliminó el botón de alternar banderas)

  newGameButton <- UI.button
    # set UI.text "Nuevo Juego"
    # set UI.class_ "game-reset-button"

  helpButton <- UI.button
    # set UI.text "Ayuda"
    # set UI.class_ "game-help-button"

  -- Crear botones para el tablero
  let (rows, cols) = boardSize game0
      positions = [(r, c) | r <- [1..rows], c <- [1..cols]]
  
  buttons <- forM positions $ \(r, c) -> do
    btn <- UI.button
      # set UI.text ""
      # set UI.class_ "mine-cell hidden"
      # set (attr "data-row") (show r)
      # set (attr "data-col") (show c)
    return btn

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
        
        -- Actualizar información
        element info # set UI.text ("Minas restantes: " ++ show minesLeft)
        element status # set UI.text gameStatus
        
        -- Actualizar estilo del estado
        element status # set UI.style [("color", case gameState gs of
          Won -> "#4CAF50"
          Lost _ -> "#f44336"
          _ -> "#2196F3")]
        
        -- Actualizar texto del botón de modo
        element modeButton # set UI.text
          (case mode of
            Reveal -> "Modo: Revelar (Clic Izquierdo)"
            Flag -> "Modo: Bandera (Clic Derecho)")
        
        -- Renderizar cada celda
        forM_ (zip buttons (indices (gameBoard gs))) $ \(btn, pos) -> do
          let cell = gameBoard gs ! pos
          let (cellText, cellClass, enabled, style) = renderCell cell (gameState gs)
          
          element btn
            # set UI.text cellText
            # set UI.class_ ("mine-cell " ++ cellClass)
            # set UI.enabled (enabled && gameState gs == Playing)
            # set UI.style style

  ------------------------------------------------
  -- Eventos
  ------------------------------------------------

  -- Eventos para celdas del tablero (usar posiciones capturadas al crear los botones)
  let positions = [(r, c) | r <- [1..rows], c <- [1..cols]]
  forM_ (zip buttons positions) $ \(btn, pos) -> do
    on UI.click btn $ \_ -> do
      let cellPos = pos
      mode <- liftIO $ readIORef actionMode
      gs <- liftIO $ readIORef state
      when (gameState gs == Playing) $ do
        liftIO $ modifyIORef state $ \currentState ->
          case mode of
            Reveal -> revealCell cellPos currentState
            Flag -> toggleFlag cellPos currentState
        renderGame

  on UI.click modeButton $ \_ -> do
    liftIO $ modifyIORef actionMode $ \mode ->
      case mode of
        Reveal -> Flag
        Flag -> Reveal
    renderGame

  -- botón de alternar banderas eliminado; usar el botón de modo para alternar

  on UI.click newGameButton $ \_ -> do
    gameNew <- liftIO $ initializeGame Beginner Nothing
    liftIO $ writeIORef state gameNew
    liftIO $ writeIORef actionMode Reveal
    renderGame

  on UI.click helpButton $ \_ -> do
    showHelpDialog window

  on UI.click backButton $ \_ -> do
    liftIO $ writeIORef screenRef Menu
    renderApp

  ------------------------------------------------
  -- Layout
  ------------------------------------------------

  -- Crear grid del tablero
  let rowsUI = chunksOf cols buttons
      chunksOf n [] = []
      chunksOf n xs = take n xs : chunksOf n (drop n xs)
  
  boardRows <- forM rowsUI $ \rowButtons -> do
    rowDiv <- UI.div # set UI.class_ "mine-row"
    forM_ rowButtons $ \btn -> do
      element rowDiv #+ [pure btn]
    return rowDiv

  boardUI <- UI.div # set UI.class_ "mine-board"
  forM_ boardRows $ \row -> do
    element boardUI #+ [pure row]

  controlsRow1 <- UI.div # set UI.class_ "controls-row"
    #+ [ pure modeButton
       ]

  controlsRow2 <- UI.div # set UI.class_ "controls-row"
    #+ [ pure newGameButton
       , pure helpButton
       ]

  container <- UI.div # set UI.class_ "game-container"
    #+ [ pure backButton
       , pure title
       , pure info
       , pure status
       , pure boardUI
       , pure controlsRow1
       , pure controlsRow2
       ]

  ------------------------------------------------
  -- Inicial
  ------------------------------------------------

  renderGame
  void $ getBody window #+ [pure container]

--------------------------------------------------
-- Auxiliares
--------------------------------------------------

-- Tipo para modo de acción
-- Tipo para modo de acción
data ActionMode = Reveal | Flag
  deriving (Eq, Show)

-- Función para agregar estilos CSS
addStyles :: Window -> UI ()
addStyles window = do
  let styles = unlines [
        ".game-container {",
        "  display: flex;",
        "  flex-direction: column;",
        "  align-items: center;",
        "  padding: 20px;",
        "  font-family: 'Arial', sans-serif;",
        "  background: linear-gradient(135deg, #363636ff 0%, #252525ff 100%);",
        "  min-height: 100vh;",
        "}",
        "",
        ".back-button {",
        "  align-self: flex-start;",
        "  margin: 10px;",
        "  padding: 10px 20px;",
        "  background-color: #2196F3;",
        "  color: white;",
        "  border: none;",
        "  border-radius: 5px;",
        "  cursor: pointer;",
        "  font-size: 16px;",
        "  transition: background-color 0.3s;",
        "}",
        "",
        ".back-button:hover {",
        "  background-color: #1976D2;",
        "}",
        "",
        ".game-title {",
        "  color: white;",
        "  text-shadow: 2px 2px 4px rgba(0,0,0,0.5);",
        "  margin: 20px 0;",
        "}",
        "",
        ".game-info, .game-status {",
        "  color: white;",
        "  font-size: 18px;",
        "  margin: 10px 0;",
        "  font-weight: bold;",
        "}",
        "",
        ".mine-board {",
        "  display: flex;",
        "  flex-direction: column;",
        "  gap: 2px;",
        "  background-color: #333;",
        "  padding: 5px;",
        "  border-radius: 8px;",
        "  box-shadow: 0 10px 30px rgba(0,0,0,0.3);",
        "  margin: 20px 0;",
        "}",
        "",
        ".mine-row {",
        "  display: flex;",
        "  gap: 2px;",
        "}",
        "",
        ".mine-cell {",
        "  width: 45px !important;",
        "  height: 45px !important;",
        "  min-width: 45px !important;",
        "  min-height: 45px !important;",
        "  max-width: 45px !important;",
        "  max-height: 45px !important;",
        "  border: none;",
        "  border-radius: 4px;",
        "  font-size: 18px !important;",
        "  font-weight: bold !important;",
        "  display: flex !important;",
        "  align-items: center !important;",
        "  justify-content: center !important;",
        "  cursor: pointer !important;",
        "  transition: all 0.2s !important;",
        "  box-sizing: border-box !important;",
        "  flex-shrink: 0 !important;",
        "  user-select: none !important;",
        "}",
        "",
        ".mine-cell.hidden {",
        "  background-color: #bbb !important;",
        "  color: transparent !important;",
        "  box-shadow: inset 0 2px 5px rgba(0,0,0,0.2) !important;",
        "}",
        "",
        ".mine-cell.hidden:hover {",
        "  background-color: #999 !important;",
        "  transform: scale(1.05) !important;",
        "}",
        "",
        ".mine-cell.hidden:disabled {",
        "  background-color: #888 !important;",
        "  cursor: not-allowed !important;",
        "}",
        "",
        ".mine-cell.revealed {",
        "  background-color: #f0f0f0 !important;",
        "  color: #333 !important;",
        "  border: 2px solid #ddd !important;",
        "}",
        "",
        ".mine-cell.mine.revealed {",
        "  background-color: #ff4444 !important;",
        "  color: white !important;",
        "}",
        "",
        ".mine-cell.flagged {",
        "  background-color: #ffcc00 !important;",
        "  color: #333 !important;",
        "}",
        "",
        ".mine-cell.questioned {",
        "  background-color: #66ccff !important;",
        "  color: #333 !important;",
        "}",
        "",
        ".mine-cell[disabled] {",
        "  opacity: 1 !important;",
        "  cursor: default !important;",
        "}",
        "",
        ".controls-row {",
        "  display: flex;",
        "  gap: 10px;",
        "  margin: 10px 0;",
        "  flex-wrap: wrap;",
        "  justify-content: center;",
        "}",
        "",
        ".game-mode-button, .game-flag-button, .game-reset-button, .game-help-button {",
        "  padding: 12px 24px !important;",
        "  background-color: #4CAF50 !important;",
        "  color: white !important;",
        "  border: none !important;",
        "  border-radius: 5px !important;",
        "  cursor: pointer !important;",
        "  font-size: 16px !important;",
        "  transition: background-color 0.3s, transform 0.2s !important;",
        "  min-width: 180px !important;",
        "}",
        "",
        ".game-mode-button:hover, .game-flag-button:hover, .game-reset-button:hover, .game-help-button:hover {",
        "  background-color: #45a049 !important;",
        "  transform: translateY(-2px) !important;",
        "}",
        "",
        ".game-reset-button {",
        "  background-color: #2196F3 !important;",
        "}",
        "",
        ".game-reset-button:hover {",
        "  background-color: #1976D2 !important;",
        "}",
        "",
        ".game-help-button {",
        "  background-color: #ff9800 !important;",
        "}",
        "",
        ".game-help-button:hover {",
        "  background-color: #e68900 !important;",
        "}",
        "",
        ".help-dialog {",
        "  position: fixed !important;",
        "  top: 50% !important;",
        "  left: 50% !important;",
        "  transform: translate(-50%, -50%) !important;",
        "  background: white !important;",
        "  padding: 30px !important;",
        "  border-radius: 10px !important;",
        "  box-shadow: 0 20px 60px rgba(0,0,0,0.3) !important;",
        "  z-index: 1000 !important;",
        "  max-width: 500px !important;",
        "  width: 90% !important;",
        "}",
        "",
        ".close-button {",
        "  padding: 10px 20px !important;",
        "  background-color: #f44336 !important;",
        "  color: white !important;",
        "  border: none !important;",
        "  border-radius: 5px !important;",
        "  cursor: pointer !important;",
        "  font-size: 16px !important;",
        "  margin-top: 20px !important;",
        "}",
        "",
        ".close-button:hover {",
        "  background-color: #d32f2f !important;",
        "}"
        ]
  
  -- Agregar los estilos al documento (inserta un <style> en el body)
  body <- getBody window
  styleEl <- UI.div # set UI.html ("<style>" ++ styles ++ "</style>")
  void $ getBody window #+ [pure styleEl]

-- Renderizar una celda con colores para números
renderCell :: Cell -> GameState -> (String, String, Bool, [(String, String)])
renderCell cell gameState =
  case cellState cell of
    Hidden -> ("", "hidden", True, [])
    Revealed -> case cellContent cell of
      Mine -> ("💣", "mine revealed", False, [])
      Empty -> ("", "empty revealed", False, [])
      Number n -> 
        let color = case n of
              1 -> "#0000FF"  -- Azul
              2 -> "#008000"  -- Verde
              3 -> "#FF0000"  -- Rojo
              4 -> "#800080"  -- Púrpura
              5 -> "#800000"  -- Marrón
              6 -> "#008080"  -- Turquesa
              7 -> "#000000"  -- Negro
              8 -> "#808080"  -- Gris
              _ -> "#000000"  -- Negro por defecto
        in (show n, "number revealed", False, [("color", color), ("font-weight", "bold")])
    Flagged -> ("🚩", "flagged", True, [])
    Questioned -> ("?", "questioned", True, [])

-- Mostrar diálogo de ayuda
showHelpDialog :: Window -> UI ()
showHelpDialog window = do
  let helpText = "Controles del Buscaminas:\n\n" ++
                 "• Clic Izquierdo: Revelar celda\n" ++
                 "• Clic Derecho: Poner/quitar bandera\n" ++
                 "• Botón 'Modo': Alterna entre Revelar y Bandera\n" ++
                 "• Botón 'Nuevo Juego': Reinicia la partida\n" ++
                 "• Botón 'Ayuda': Muestra este mensaje\n\n" ++
                 "Objetivo: Revelar todas las celdas sin minas."
  
  closeBtn <- UI.button # set UI.text "Cerrar" # set UI.class_ "close-button"

  dialog <- UI.div # set UI.class_ "help-dialog"
    #+ [ UI.h2 # set UI.text "Ayuda"
       , UI.p # set UI.text helpText
       , pure closeBtn
       ]

  body <- getBody window
  element body #+ [pure dialog]

  on UI.click closeBtn $ \_ ->
    element dialog # set UI.style [("display", "none")]