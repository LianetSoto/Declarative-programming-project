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
    # set UI.class_ "title"

  info <- UI.div
    # set UI.class_ "info"
    # set UI.text "Minas restantes: 10"

  status <- UI.div
    # set UI.class_ "status"
    # set UI.text "JUGANDO"

  modeButton <- UI.button
    # set UI.text "Modo: Revelar (Clic Izquierdo)"
    # set UI.class_ "mode-button"

  flagButton <- UI.button
    # set UI.text "Alternar a Banderas"
    # set UI.class_ "flag-button"

  newGameButton <- UI.button
    # set UI.text "Nuevo Juego"
    # set UI.class_ "reset-button"

  helpButton <- UI.button
    # set UI.text "Ayuda"
    # set UI.class_ "help-button"

  -- Crear botones para el tablero
  let (rows, cols) = boardSize game0
      positions = [(r, c) | r <- [1..rows], c <- [1..cols]]
  
  buttons <- forM positions $ \(r, c) -> do
    btn <- UI.button
      # set UI.text ""
      # set UI.class_ "mine-cell"
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

        let minesLeft = mineCount gs - flagsLeft gs
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
          _ -> "black")]
        
        -- Actualizar texto del botón de modo
        element modeButton # set UI.text
          (case mode of
            Reveal -> "Modo: Revelar (Clic Izquierdo)"
            Flag -> "Modo: Bandera (Clic Derecho)"
            Question -> "Modo: Pregunta (Clic Medio)")
        
        -- Renderizar cada celda
        forM_ (zip buttons (indices (gameBoard gs))) $ \(btn, pos) -> do
          let cell = gameBoard gs ! pos
          let (cellText, cellClass, enabled) = renderCell cell (gameState gs)
          
          element btn
            # set UI.text cellText
            # set UI.class_ ("mine-cell " ++ cellClass)
            # set UI.enabled (enabled && gameState gs == Playing)

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
            Question -> toggleQuestion cellPos currentState
        renderGame

  on UI.click modeButton $ \_ -> do
    liftIO $ modifyIORef actionMode $ \mode ->
      case mode of
        Reveal -> Flag
        Flag -> Question
        Question -> Reveal
    renderGame

  on UI.click flagButton $ \_ -> do
    liftIO $ writeIORef actionMode Flag
    renderGame

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
       , pure flagButton
       ]

  controlsRow2 <- UI.div # set UI.class_ "controls-row"
    #+ [ pure newGameButton
       , pure helpButton
       ]

  container <- UI.div # set UI.class_ "container"
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
data ActionMode = Reveal | Flag | Question
  deriving (Eq, Show)

-- Renderizar una celda
renderCell :: Cell -> GameState -> (String, String, Bool)
renderCell cell gameState =
  case cellState cell of
    Hidden -> ("", "hidden", True)
    Revealed -> case cellContent cell of
      Mine -> ("💣", "mine revealed", False)
      Empty -> ("", "empty revealed", False)
      Number n -> (show n, "number revealed", False)
    Flagged -> ("🚩", "flagged", True)
    Questioned -> ("?", "questioned", True)

-- Mostrar diálogo de ayuda
showHelpDialog :: Window -> UI ()
showHelpDialog window = do
  let helpText = "Controles del Buscaminas:\n\n" ++
                 "• Clic Izquierdo: Revelar celda\n" ++
                 "• Clic Derecho: Poner/quitar bandera\n" ++
                 "• Clic Medio: Poner/quitar signo de pregunta\n" ++
                 "• Botón 'Alternar a Banderas': Cambia al modo bandera\n" ++
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

-- Juego inicial (9x9 con 10 minas)
initialGame :: Game
initialGame = Game
  { gameBoard = createEmptyBoard (9, 9)
  , gameState = Playing
  , boardSize = (9, 9)
  , mineCount = 10
  , flagsLeft = 10
  , startTime = Nothing
  }