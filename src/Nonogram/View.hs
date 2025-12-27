{-# LANGUAGE OverloadedStrings #-}

module Nonogram.View where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Data.IORef
import Control.Monad (void, forM_, forM, when)
import qualified Data.List as List
import Data.Char (toLower)
import Data.Maybe (fromMaybe)

import Core.Types
import Nonogram.Types
import Nonogram.Game
  ( initialGame
  , toggleCell
  , setFilled
  , setMarkedEmpty
  , setCellTo
  , resetGame
  , checkVictory
  , puzzleFromSample5
  , puzzleFromSample10
  )
import Nonogram.Puzzles ( loadPuzzlesFromFile )

-- Modo de acción
data ActionMode = ModeFill | ModeX
  deriving (Eq, Show)

-- Normaliza valores de difficulty desde JSON o embebidos a claves en inglés
normalizeDifficulty :: String -> String
normalizeDifficulty s =
  let low = map toLower s
  in if "easy" `List.isInfixOf` low || "facil" `List.isInfixOf` low || "fácil" `List.isInfixOf` low
     then "easy"
     else if "medium" `List.isInfixOf` low || "medio" `List.isInfixOf` low
     then "medium"
     else if "hard" `List.isInfixOf` low || "dificil" `List.isInfixOf` low || "difícil" `List.isInfixOf` low
     then "hard"
     else "easy" -- fallback

-- Vista principal para Nonogramas
nonogramView :: Window -> IORef Screen -> UI () -> UI ()
nonogramView window screenRef renderApp = do
  void $ return window # set title "Nonogramas"

  -- Estilos
  addStyles window

  -- Cargar puzzles (con dificultad)
  puzzlesLoaded <- liftIO $ loadPuzzlesFromFile "data/puzzles.json"
  let builtIn = [puzzleFromSample5, puzzleFromSample10]
      -- normalizar difficulties de los puzzles cargados
      allRawPuzzles = if null puzzlesLoaded then builtIn else puzzlesLoaded
      allPuzzles = map (\p -> p { puzzleDifficulty = normalizeDifficulty (puzzleDifficulty p) }) allRawPuzzles

      -- lista de dificultades en inglés
      difficulties = [("easy","Easy"), ("medium","Medium"), ("hard","Hard")]
      defaultDifficultyKey = "easy"

  -- UI: difficulty selector
  difficultySelect <- UI.select # set (attr "id") "difficulty-select"
  forM_ difficulties $ \(k,label) -> do
    opt <- UI.option # set UI.text label # set UI.value k
    void $ element difficultySelect #+ [ pure opt ]
  -- establecer default "easy"
  void $ element difficultySelect # set UI.value defaultDifficultyKey

  -- puzzle selector (se llenará según dificultad)
  puzzleSelect <- UI.select # set (attr "id") "puzzle-select"

  let puzzlesFor diff = if diff == "all" then allPuzzles else filter (\p -> puzzleDifficulty p == diff) allPuzzles

  -- inicializar con dificultad por defecto
  let currentPuzzles = puzzlesFor defaultDifficultyKey
      initialPuzzle = if null currentPuzzles then head allPuzzles else head currentPuzzles

  gameRef <- liftIO $ newIORef (initialGame initialPuzzle)

  -- poblar puzzleSelect (solo muestra tamaño "RxC")
  let formatLabel p = let (r,c) = puzzleSize p in show r ++ "x" ++ show c
      populatePuzzleSelect :: [Puzzle] -> UI ()
      populatePuzzleSelect ps = do
        element puzzleSelect # set children []
        forM_ (zip [1..] ps) $ \(i,p) -> do
          opt <- UI.option # set UI.text (formatLabel p) # set UI.value (show i)
          void $ element puzzleSelect #+ [ pure opt ]

  populatePuzzleSelect currentPuzzles

  -- UI elements
  backButton <- UI.button # set UI.text "← Back" # set UI.class_ "back-button"

  -- Handler para el botón de volver
  on UI.click backButton $ \_ -> do
    liftIO $ writeIORef screenRef Menu
    renderApp

  titleEl <- UI.h1 # set UI.text "NONOGRAM" # set UI.class_ "game-title"
  info  <- UI.div # set UI.class_ "game-info" # set UI.text ""        -- only win/lose
  errCountDiv <- UI.div # set UI.class_ "error-info" # set UI.text "Errores: 0/3"

  resetBtn <- UI.button # set UI.text "Reiniciar" # set UI.class_ "game-reset-button"

  -- mode button
  actionModeRef <- liftIO $ newIORef ModeFill
  modeButton <- UI.button # set UI.text "" # set UI.class_ "mode-button fill-mode" # set (attr "title") "Modo: Rellenar"

  -- placeholders for clues and board
  topPlaceholder <- UI.div # set UI.class_ "top-placeholder"
  leftPlaceholder <- UI.div # set UI.class_ "left-placeholder"

  -- buttons ref
  buttonsRef <- liftIO $ newIORef ([] :: [Element])

  -- Toggle mode handler
  on UI.click modeButton $ \_ -> do
    liftIO $ modifyIORef actionModeRef $ \m -> case m of
      ModeFill -> ModeX
      ModeX    -> ModeFill
    m <- liftIO $ readIORef actionModeRef
    let (title, cls) = if m == ModeFill then ("Modo: Rellenar", "fill-mode") else ("Modo: Marcar como vacío", "x-mode")
    element modeButton # set (attr "title") title
    element modeButton # set UI.class_ ("mode-button " ++ cls)

  -- Render + createButtons
  let
    renderGame :: UI ()
    renderGame = do
      game <- liftIO $ readIORef gameRef
      let pb = gamePuzzle game
          board = gameBoard game
          (rCount, cCount) = puzzleSize pb
          sol = puzzleSolution pb
          errs = errorCount game
          maxE = maxErrors game
          gameSt = gameState game

      -- only show when won/lost
      element info # set UI.text (case gameSt of
                                   Playing -> ""
                                   Won     -> "¡GANASTE!"
                                   Lost    -> "PERDISTE!")
      element errCountDiv # set UI.text ("Errores: " ++ show errs ++ "/" ++ show maxE)

      -- top clues
      topDiv <- UI.div # set UI.class_ "top-clues"
      forM_ (puzzleCols pb) $ \cl -> do
        inner <- UI.div # set UI.class_ "top-clue"
        let strs = if null cl then [""] else map show cl
        spans <- forM strs $ \s -> UI.span # set UI.text s
        element inner #+ map pure spans
        element topDiv #+ [ pure inner ]

      -- left clues
      leftDiv <- UI.div # set UI.class_ "left-clues"
      forM_ (puzzleRows pb) $ \cl -> do
        inner <- UI.div # set UI.class_ "left-clue"
        let strs = if null cl then [""] else map show cl
        spans <- forM strs $ \s -> UI.span # set UI.text s
        element inner #+ map pure spans
        element leftDiv #+ [ pure inner ]

      -- board buttons
      btns <- liftIO $ readIORef buttonsRef
      let requiredCount = rCount * cCount
      btns' <- if length btns == requiredCount
               then return btns
               else do
                 newBtns <- createButtons rCount cCount
                 liftIO $ writeIORef buttonsRef newBtns
                 return newBtns

      let chunksOf n [] = []
          chunksOf n xs = take n xs : chunksOf n (drop n xs)
          rowsList = chunksOf cCount btns'

      boardGrid <- UI.div # set UI.class_ "non-board"
      forM_ rowsList $ \rowBtns -> do
        rowDiv <- UI.div # set UI.class_ "non-row"
        forM_ rowBtns $ \b -> element rowDiv #+ [ pure b ]
        element boardGrid #+ [ pure rowDiv ]

      corner <- UI.div # set UI.class_ "corner"
      topRow <- UI.div # set UI.class_ "top-row" #+ [ pure corner, pure topDiv ]
      midRow <- UI.div # set UI.class_ "mid-row" #+ [ pure leftDiv, pure boardGrid ]

      element topPlaceholder # set children []
      element topPlaceholder #+ [ pure topRow ]
      element leftPlaceholder # set children []
      element leftPlaceholder #+ [ pure midRow ]

      -- update cells
      let positions = [ (r, c) | r <- [1..rCount], c <- [1..cCount] ]
      forM_ (zip (take (length positions) btns') positions) $ \(btn, (r,c)) -> do
        let cell = (board !! (r-1)) !! (c-1)
            expected = (sol !! (r-1)) !! (c-1)
            (txt, cls, enabled, style) = renderCellUI cell expected gameSt
        element btn
          # set UI.text txt
          # set UI.class_ ("non-cell " ++ cls)
          # set UI.enabled (enabled && gameSt == Playing)
          # set UI.style style

      return ()

    createButtons :: Int -> Int -> UI [Element]
    createButtons rCount cCount = do
      let positions = [ (r, c) | r <- [1..rCount], c <- [1..cCount] ]
      btns <- forM positions $ \_ -> UI.button # set UI.class_ "non-cell" # set UI.text ""
      forM_ (zip btns positions) $ \(btn, pos@(r,c)) -> do
        on UI.click btn $ \_ -> do
          mode <- liftIO $ readIORef actionModeRef
          let setFn = if mode == ModeFill then setFilled else setMarkedEmpty
          liftIO $ modifyIORef gameRef (setFn pos)
          renderGame
      return btns

  -- handlers para selects
  on UI.selectionChange difficultySelect $ \_ -> do
    selectedDiff <- get UI.value difficultySelect
    let ps = puzzlesFor selectedDiff
    populatePuzzleSelect ps
    let newPuzzle = if null ps then head allPuzzles else head ps
    liftIO $ writeIORef gameRef (initialGame newPuzzle)
    renderGame

  on UI.selectionChange puzzleSelect $ \_ -> do
    val <- get UI.value puzzleSelect
    let idx = read val :: Int
    diff <- get UI.value difficultySelect
    let ps = puzzlesFor diff
        newPuzzle = if idx > 0 && idx <= length ps then ps !! (idx - 1) else head ps
    liftIO $ writeIORef gameRef (initialGame newPuzzle)
    renderGame

  -- handler para reset
  on UI.click resetBtn $ \_ -> do
    liftIO $ modifyIORef gameRef resetGame
    renderGame

  -- header con selects
  headerControls <- UI.div # set UI.class_ "header-controls"
    #+ [ pure difficultySelect, pure puzzleSelect ]

  -- controles inferiores
  controls <- UI.div # set UI.class_ "controls"
    #+ [ pure modeButton, pure resetBtn ]

  -- contenedor principal que centra todo
  mainContent <- UI.div # set UI.class_ "main-content" 
    #+ [ pure titleEl
       , pure headerControls
       , pure info
       , pure errCountDiv
       , pure topPlaceholder
       , pure leftPlaceholder
       , pure controls
       ]

  container <- UI.div # set UI.class_ "nonogram-container"
    #+ [ pure backButton
       , pure mainContent
       ]

  void $ getBody window #+ [ pure container ]

  -- initial render
  renderGame

-- Renderizar celda
-- retorna: (texto, clase-css, enabled?, estilo-inline)
renderCellUI :: Cell -> Bool -> Nonogram.Types.GameState -> (String, String, Bool, [(String,String)])
renderCellUI Unknown _ Playing =
  ("", "unknown", True, [])
renderCellUI Unknown _ _ =  -- Si el juego terminó, mostrar celdas desconocidas correctamente
  ("", "unknown", False, [])
renderCellUI LockedEmpty _ _ =
  ("X", "marked-empty", False, [])
renderCellUI Filled expected _
  | expected  = ("", "filled-correct", False, [])
  | otherwise = ("X", "marked-empty", False, [])  -- ERROR: Rellenar donde debería ir X
renderCellUI MarkedEmpty _ _ =
  ("X", "marked-empty", True, [])

-- Styles 
addStyles :: Window -> UI ()
addStyles window = do
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
        , "  color: #fff;"
        , "}"
        , ".nonogram-container {"
        , "  width: fit-content;"
        , "  max-width: 100vw;"
        , "  background: rgba(255, 255, 255, 0.05);"
        , "  backdrop-filter: blur(10px);"
        , "  border-radius: 20px;"
        , "  padding: 40px;"
        , "  box-shadow: 0 20px 60px rgba(0, 0, 0, 0.5), 0 0 40px rgba(138, 43, 226, 0.2);"
        , "  border: 1px solid rgba(255, 255, 255, 0.1);"
        , "  display: flex;"
        , "  flex-direction: column;"
        , "  align-items: center;"
        , "}"
        , "/* TÍTULO */"
        , ".game-title {"
        , "  text-align: center;"
        , "  color: #fff;"
        , "  font-size: 3em;"
        , "  margin-bottom: 10px;"
        , "  text-shadow: 0 0 20px rgba(138, 43, 226, 0.8), 0 0 40px rgba(138, 43, 226, 0.5);"
        , "  font-weight: bold;"
        , "}"
        , ".game-info, .error-info {"
        , "  text-align: center;"
        , "  margin: 10px 0;"
        , "  font-size: 1.2em;"
        , "  color: #a0a0a0;"
        , "}"
        , ".game-info {"
        , "  font-size: 1.5em;"
        , "  color: #ffd700;"
        , "  text-shadow: 0 0 10px rgba(255, 215, 0, 0.5);"
        , "}"
        , "/* CLUES */"
        , ".top-placeholder, .left-placeholder {"
        , "  display: flex;"
        , "  justify-content: center;"
        , "  width: 100%;"
        , "  margin: 0 auto;"
        , "}"
        , ".top-row { display: flex; align-items: flex-end; gap: 10px; margin-top: 20px; justify-content: center; }"
        , ".corner { width: 80px; height: 96px; }"
        , ".top-clues { display: flex; gap: 6px; }"
        , ".top-clue {"
        , "  display: flex;"
        , "  flex-direction: column;"
        , "  align-items: center;"
        , "  justify-content: flex-end;"
        , "  width: 54px;"
        , "  height: 96px;"
        , "  font-size: 16px;"
        , "  color: #00d4ff;"
        , "  text-shadow: 0 0 5px rgba(0, 212, 255, 0.5);"
        , "}"
        , ".mid-row { display: flex; gap: 10px; margin-top: 10px; justify-content: center; align-items: flex-start; overflow-x: auto; }"
        , ".left-clues { display: flex; flex-direction: column; gap: 6px; width: 90px; margin-top: 20px; }"
        , ".left-clue {"
        , "  display: flex;"
        , "  justify-content: flex-end;"
        , "  align-items: center;"
        , "  padding-right: 8px;"
        , "  height: 54px;"
        , "  font-size: 16px;"
        , "  color: #00d4ff;"
        , "  text-shadow: 0 0 5px rgba(0, 212, 255, 0.5);"
        , "}"
        , "/* BOARD */"
        , ".non-board {"
        , "  display: flex;"
        , "  flex-direction: column;"
        , "  gap: 8px;"
        , "  background: rgba(0, 0, 0, 0.5);"
        , "  padding: 20px;"
        , "  border-radius: 15px;"
        , "  border: 2px solid rgba(138, 43, 226, 0.5);"
        , "  box-shadow: 0 0 30px rgba(138, 43, 226, 0.3);"
        , "  margin: 0 auto;"
        , "}"
        , ".non-row { display: flex; gap: 8px; }"
        , "/* CELDAS */"
        , ".non-cell {"
        , "  width: 54px;"
        , "  height: 54px;"
        , "  display: flex;"
        , "  align-items: center;"
        , "  justify-content: center;"
        , "  border-radius: 8px;"
        , "  border: 2px solid rgba(138, 43, 226, 0.5);"
        , "  background: rgba(26, 26, 26, 0.8);"
        , "  cursor: pointer;"
        , "  font-weight: bold;"
        , "  color: #fff;"
        , "  transition: all 0.2s ease;"
        , "  font-size: 24px;"
        , "  text-shadow: 0 0 5px rgba(138, 43, 226, 0.5);"
        , "  box-shadow: 0 0 15px rgba(138, 43, 226, 0.3);"
        , "}"
        , ".non-cell:hover:not([disabled]) {"
        , "  transform: translateY(-3px);"
        , "  box-shadow: 0 0 25px rgba(138, 43, 226, 0.5), 0 0 40px rgba(255, 107, 157, 0.3);"
        , "}"
        , ".non-cell[disabled] { cursor: default; }"
        , "/* Estados de celda */"
        , ".non-cell.unknown { background: rgba(26, 26, 26, 0.8); border-color: rgba(138, 43, 226, 0.5); }"
        , ".non-cell.filled-correct {"
        , "  background: rgba(138, 43, 226, 0.8);"
        , "  border-color: #8a2be2;"
        , "  color: #fff;"
        , "  box-shadow: 0 0 20px rgba(138, 43, 226, 0.8), 0 0 40px rgba(138, 43, 226, 0.5);"
        , "}"
        , ".non-cell.marked-empty {"
        , "  background: rgba(26, 26, 26, 0.8);"
        , "  color: #ff6b9d;"
        , "  border-color: #ff6b9d;"
        , "  box-shadow: 0 0 20px rgba(255, 107, 157, 0.5), 0 0 30px rgba(255, 107, 157, 0.3);"
        , "}"
        , "/* BOTONES */"
        , ".back-button, .game-reset-button, .mode-button {"
        , "  background: linear-gradient(135deg, rgba(138, 43, 226, 0.2) 0%, rgba(75, 0, 130, 0.3) 100%);"
        , "  border: 2px solid rgba(138, 43, 226, 0.5);"
        , "  border-radius: 15px;"
        , "  padding: 12px 24px;"
        , "  color: #fff;"
        , "  font-size: 1.1em;"
        , "  font-weight: 600;"
        , "  cursor: pointer;"
        , "  transition: all 0.3s ease;"
        , "  box-shadow: 0 10px 30px rgba(0, 0, 0, 0.3);"
        , "}"
        , ".back-button:hover, .game-reset-button:hover, .mode-button:hover {"
        , "  transform: translateY(-5px);"
        , "  box-shadow: 0 15px 40px rgba(138, 43, 226, 0.4), 0 0 30px rgba(138, 43, 226, 0.3);"
        , "  border-color: rgba(138, 43, 226, 0.8);"
        , "}"
        , ".controls { display: flex; gap: 15px; margin-top: 30px; align-items: center; justify-content: center; }"
        , ".mode-button { width: 60px; height: 50px; position: relative; border-radius: 8px; text-align: center; }"
        , ".fill-mode { background: linear-gradient(135deg, rgba(138, 43, 226, 0.3) 0%, rgba(75, 0, 130, 0.4) 100%); border-color: #8a2be2; box-shadow: 0 0 15px #8a2be2; }"
        , ".fill-mode:hover { box-shadow: 0 0 25px #8a2be2, 0 0 40px #8a2be2; }"
        , ".x-mode { background: linear-gradient(135deg, rgba(255, 107, 157, 0.3) 0%, rgba(255, 0, 130, 0.4) 100%); border-color: #ff6b9d; box-shadow: 0 0 15px #ff6b9d; }"
        , ".x-mode:hover { box-shadow: 0 0 25px #ff6b9d, 0 0 40px #ff6b9d; }"
        , ".x-mode::after {"
        , "  content: 'X';"
        , "  position: absolute;"
        , "  top: 50%;"
        , "  left: 50%;"
        , "  transform: translate(-50%, -50%);"
        , "  font-size: 24px;"
        , "  color: #fff;"
        , "  font-weight: bold;"
        , "}"
        , ".back-button { align-self: flex-start; margin-bottom: 20px; }"
        , "/* Selectors */"
        , ".header-controls { margin-bottom: 20px; display: flex; justify-content: center; gap: 15px; }"
        , ".header-controls select {"
        , "  padding: 10px 15px;"
        , "  border-radius: 8px;"
        , "  background: rgba(255, 255, 255, 0.05) !important;"
        , "  color: #fff !important;"
        , "  border: 2px solid rgba(138, 43, 226, 0.5) !important;"
        , "  font-size: 14px !important;"
        , "  box-shadow: 0 0 10px rgba(138, 43, 226, 0.3) !important;"
        , "  appearance: none !important;"
        , "  -webkit-appearance: none !important;"
        , "  -moz-appearance: none !important;"
        , "}"
        , ".header-controls select option {"
        , "  background: #0f0c29 !important;"
        , "  color: #fff !important;"
        , "  padding: 10px !important;"
        , "}"
        , ".header-controls select:focus {"
        , "  outline: none !important;"
        , "  border-color: rgba(138, 43, 226, 0.8) !important;"
        , "  box-shadow: 0 0 20px rgba(138, 43, 226, 0.5) !important;"
        , "}"
        , "@media (max-width:520px) { "
        , "  .non-cell { width:40px; height:40px; font-size: 18px; } "
        , "  .top-clue { width:40px; height:64px; } "
        , "  .left-clues { width: 60px; } "
        , "  .left-clue { font-size: 14px; height: 40px; } "
        , "  .non-board { padding: 15px; } "
        , "  .top-row, .mid-row { flex-wrap: wrap; justify-content: center; }"
        , "  .game-info { font-size: 20px; }"
        , "}"
        ]
  body <- getBody window
  styleEl <- UI.div # set UI.html ("<style>" ++ styles ++ "</style>")
  void $ element body #+ [pure styleEl]