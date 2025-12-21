{-# LANGUAGE OverloadedStrings #-}

module Nonogram.View where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Data.IORef
import Control.Monad (void, forM_, forM, when)
import qualified Data.List as List

import Core.Types
import Nonogram.Types
import Nonogram.Game ( initialGame, toggleCell, resetGame, checkVictory
                    , puzzleFromSample5, puzzleFromSample10 )

-- Vista principal para Nonogramas
nonogramView :: Window -> IORef Screen -> UI () -> UI ()
nonogramView window screenRef renderApp = do
  void $ return window # set title "Nonogramas"

  -- Estilos (básicos)
  addStyles window

  -- Crear juego inicial (usar puzzle de ejemplo de 5x5)
  let puzzle = puzzleFromSample5
  gameRef <- liftIO $ newIORef (initialGame puzzle)

  backButton <- UI.button # set UI.text "← Volver al menú" # set UI.class_ "back-button"
  title <- UI.h1 # set UI.text "Nonogramas" # set UI.class_ "game-title"
  info  <- UI.div # set UI.class_ "game-info" # set UI.text "Estado: JUGANDO"
  resetBtn <- UI.button # set UI.text "Reiniciar" # set UI.class_ "game-reset-button"
  puzzleSelect <- UI.select # set (attr "id") "puzzle-select"
  -- añadir opciones de puzzle
  option5 <- UI.option # set UI.text "Ejemplo 5x5 (cruz)" # set UI.value "5"
  option10 <- UI.option # set UI.text "Ejemplo 10x10" # set UI.value "10"
  void $ element puzzleSelect #+ [ pure option5, pure option10 ]

  -- Placeholders para pistas (se actualizan en render)
  topPlaceholder <- UI.div # set UI.class_ "top-placeholder"
  leftPlaceholder <- UI.div # set UI.class_ "left-placeholder"

  -- Obtener datos del puzzle (para construir botones)
  g0 <- liftIO $ readIORef gameRef
  let (rows, cols) = puzzleSize (gamePuzzle g0)
      positions = [ (r, c) | r <- [1..rows], c <- [1..cols] ]

  -- Crear botones para tablero
  buttons <- forM positions $ \_ -> UI.button # set UI.class_ "non-cell" # set UI.text ""

  -- Render function
  let renderGame :: UI ()
      renderGame = do
        game <- liftIO $ readIORef gameRef
        let pb = gamePuzzle game
            board = gameBoard game
            (rCount, cCount) = puzzleSize pb
            sol = puzzleSolution pb

        -- Actualizar estado
        element info # set UI.text (case gameState game of
                                     Playing -> "Estado: JUGANDO"
                                     Won     -> "¡GANASTE!")

        -- ---- Construir pistas de columnas (top) ----
        topDiv <- UI.div # set UI.class_ "top-clues"
        let topClues = puzzleCols pb  -- [[Int]]
        forM_ topClues $ \cl -> do
          -- cada columna: mostrar números en columna (alineados abajo)
          inner <- UI.div # set UI.class_ "top-clue"
          -- queremos que los números aparezcan de arriba a abajo; si la lista es más corta, dejar hueco arriba
          let strs = map show cl
          spans <- forM strs $ \s -> UI.span # set UI.text s
          element inner #+ map pure spans
          element topDiv #+ [ pure inner ]

        -- ---- Construir pistas de filas (left) ----
        leftDiv <- UI.div # set UI.class_ "left-clues"
        let leftClues = puzzleRows pb
        forM_ leftClues $ \cl -> do
          inner <- UI.div # set UI.class_ "left-clue"
          let strs = map show cl
          spans <- forM strs $ \s -> UI.span # set UI.text s
          element inner #+ map pure spans
          element leftDiv #+ [ pure inner ]

        -- ---- Construir boardGrid (alineado) ----
        boardGrid <- UI.div # set UI.class_ "non-board"
        let chunksOf n [] = []
            chunksOf n xs = take n xs : chunksOf n (drop n xs)
            rowsList = chunksOf cols buttons
        forM_ rowsList $ \rowBtns -> do
          rowDiv <- UI.div # set UI.class_ "non-row"
          forM_ rowBtns $ \b -> element rowDiv #+ [ pure b ]
          element boardGrid #+ [ pure rowDiv ]

        -- Reemplazar placeholders con la estructura completa
        -- top-left corner + top clues row
        corner <- UI.div # set UI.class_ "corner"
        topRow <- UI.div # set UI.class_ "top-row" #+ [ pure corner, pure topDiv ]
        -- middle row: left clues + board
        midRow <- UI.div # set UI.class_ "mid-row" #+ [ pure leftDiv, pure boardGrid ]

        element topPlaceholder # set children []
        element topPlaceholder #+ [ pure topRow ]
        element leftPlaceholder # set children []
        element leftPlaceholder #+ [ pure midRow ]

        -- ---- Actualizar celdas con feedback correcto/incorrecto ----
        forM_ (zip buttons positions) $ \(btn, (r,c)) -> do
          let cell = (board !! (r-1)) !! (c-1)
              expected = (sol !! (r-1)) !! (c-1)
              (txt, cls, enabled, style) = renderCellUI cell expected (gameState game)
          element btn
            # set UI.text txt
            # set UI.class_ ("non-cell " ++ cls)
            # set UI.enabled (enabled && gameState game == Playing)
            # set UI.style style

        return ()

  -- Asociar eventos a botones
  forM_ (zip buttons positions) $ \(btn, pos) -> do
    on UI.click btn $ \_ -> do
      liftIO $ modifyIORef gameRef (toggleCell pos)
      g <- liftIO $ readIORef gameRef
      when (checkVictory g) $ liftIO $ modifyIORef gameRef (\gg -> gg { gameState = Won })
      renderGame

  -- Reiniciar
  on UI.click resetBtn $ \_ -> do
    g <- liftIO $ readIORef gameRef
    liftIO $ writeIORef gameRef (resetGame g)
    renderGame

  -- Seleccionar puzzle: leer explicitamente el `value` del select
  on UI.selectionChange puzzleSelect $ \_ -> do
    v <- get UI.value puzzleSelect
    case v of
      "5"  -> liftIO $ writeIORef gameRef (initialGame puzzleFromSample5)
      "10" -> liftIO $ writeIORef gameRef (initialGame puzzleFromSample10)
      _    -> return ()
    renderGame

  -- Botón volver
  on UI.click backButton $ \_ -> do
    liftIO $ writeIORef screenRef Menu
    renderApp

  -- Contenedor principal: incluimos los placeholders organizados
  controls <- UI.div # set UI.class_ "controls" #+ [ pure resetBtn, pure puzzleSelect ]

  container <- UI.div # set UI.class_ "nonogram-container"
    #+ [ pure backButton
       , pure title
       , pure info
       , pure topPlaceholder
       , pure leftPlaceholder
       , pure controls
       ]

  void $ getBody window #+ [ pure container ]

  -- Render inicial
  renderGame

-- Renderizar celda: devolver (texto, clase, enabled, estilo)
-- expected == True si la solución tiene relleno en esa posición
renderCellUI :: Cell -> Bool -> Nonogram.Types.GameState -> (String, String, Bool, [(String,String)])
renderCellUI Unknown _ _ = ("", "unknown", True, [("background-color", "#e0e0e0"), ("width","34px"), ("height","34px")])
renderCellUI Filled expected _ =
  if expected
    then ("■", "filled-correct", False, [("background-color", "#2e7d32"), ("color","#fff"), ("width","34px"), ("height","34px")]) -- verde
    else ("■", "filled-wrong", False, [("background-color", "#b71c1c"), ("color","#fff"), ("width","34px"), ("height","34px")]) -- rojo
renderCellUI MarkedEmpty expected _ =
  if not expected
    then ("X", "marked-correct", True, [("color","#2e7d32"), ("width","34px"), ("height","34px")]) -- marcar correcto (verde)
    else ("X", "marked-wrong", True, [("color","#b71c1c"), ("width","34px"), ("height","34px")]) -- marcado incorrecto (rojo)

addStyles :: Window -> UI ()
addStyles window = do
  let styles = unlines
        [ ".nonogram-container { display:flex; flex-direction: column; align-items:center; padding:20px; font-family: Arial, sans-serif; }"
        , ".top-row { display:flex; align-items:flex-end; gap:4px; margin-top:10px; }"
        , ".top-clues { display:flex; }"
        , ".top-clue { display:flex; flex-direction: column; align-items:center; justify-content:flex-end; width:34px; height:68px; }"
        , ".corner { width:60px; }"
        , ".mid-row { display:flex; gap:8px; margin-top:8px; }"
        , ".left-clues { display:flex; flex-direction: column; gap:4px; width:60px; }"
        , ".left-clue { display:flex; justify-content:flex-end; align-items:center; padding-right:6px; height:34px; }"
        , ".non-board { display:flex; flex-direction: column; gap:4px; }"
        , ".non-row { display:flex; gap:4px; }"
        , ".non-cell { width:34px; height:34px; display:flex; align-items:center; justify-content:center; border-radius:4px; border:1px solid #999; background:#e0e0e0; cursor:pointer; font-weight:bold; }"
        , ".non-cell[disabled] { cursor: default; opacity: 1; }"
        , ".filled-correct { background:#2e7d32; color: white; }"
        , ".filled-wrong { background:#b71c1c; color: white; }"
        , ".marked-correct { color:#2e7d32; }"
        , ".marked-wrong { color:#b71c1c; }"
        , ".controls { display:flex; gap:10px; margin-top:10px; }"
        , ".game-reset-button { padding:8px 12px; }"
        , ".back-button { align-self:flex-start; margin-bottom:6px; }"
        ]
  body <- getBody window
  styleEl <- UI.div # set UI.html ("<style>" ++ styles ++ "</style>")
  void $ element body #+ [pure styleEl]