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

-- Modo de acción (solo dos modos ahora)
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
  backButton <- UI.button # set UI.text "← Back" # set UI.class_ "back-button neon-btn"
  
  -- Handler para el botón de volver
  on UI.click backButton $ \_ -> do
    liftIO $ writeIORef screenRef Menu
    renderApp

  titleEl <- UI.h1 # set UI.text "NONOGRAM" # set UI.class_ "game-title"
  info  <- UI.div # set UI.class_ "game-info" # set UI.text ""        -- only win/lose
  errCountDiv <- UI.div # set UI.class_ "error-info" # set UI.text "Errores: 0/3"

  resetBtn <- UI.button # set UI.text "Reiniciar" # set UI.class_ "game-reset-button neon-btn"

  -- mode button - solo color, sin símbolo
  actionModeRef <- liftIO $ newIORef ModeFill
  modeButton <- UI.button # set UI.text "" # set UI.class_ "mode-button neon-btn fill-mode" # set (attr "title") "Modo: Rellenar"

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
    element modeButton # set UI.class_ ("mode-button neon-btn " ++ cls)

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
          gameBefore <- liftIO $ readIORef gameRef
          -- Solo permitir jugar si el juego está en estado Playing
          when (gameState gameBefore == Playing) $ do
            case mode of
              ModeFill -> liftIO $ modifyIORef gameRef (setFilled pos)
              ModeX    -> liftIO $ modifyIORef gameRef (setMarkedEmpty pos)
            gameAfter <- liftIO $ readIORef gameRef
            -- Verificar victoria después de cada movimiento
            when (checkVictory gameAfter && gameState gameAfter == Playing) $ do
              liftIO $ modifyIORef gameRef (\g -> g { gameState = Won })
            renderGame
      return btns

  -- initial buttons
  let (r0, c0) = puzzleSize initialPuzzle
  initialBtns <- createButtons r0 c0
  liftIO $ writeIORef buttonsRef initialBtns

  -- reset handler
  on UI.click resetBtn $ \_ -> do
    g <- liftIO $ readIORef gameRef
    liftIO $ writeIORef gameRef (resetGame g)
    let (rCount, cCount) = puzzleSize (gamePuzzle g)
    newBtns <- createButtons rCount cCount
    liftIO $ writeIORef buttonsRef newBtns
    renderGame

  -- difficulty change -> repopulate puzzleSelect (no "all" option)
  on UI.selectionChange difficultySelect $ \_ -> do
    diff <- get UI.value difficultySelect
    let ps = puzzlesFor diff
    populatePuzzleSelect ps
    case ps of
      (p:_) -> do
        liftIO $ writeIORef gameRef (initialGame p)
        newBtns <- createButtons (fst (puzzleSize p)) (snd (puzzleSize p))
        liftIO $ writeIORef buttonsRef newBtns
        renderGame
      [] -> return ()

  -- puzzle selection
  on UI.selectionChange puzzleSelect $ \_ -> do
    idxStr <- get UI.value puzzleSelect
    diff <- get UI.value difficultySelect
    let psList = puzzlesFor diff
    case reads idxStr :: [(Int, String)] of
      [(i, _)] | i >= 1 && i <= length psList -> do
        let p = psList !! (i-1)
        liftIO $ writeIORef gameRef (initialGame p)
        newBtns <- createButtons (fst (puzzleSize p)) (snd (puzzleSize p))
        liftIO $ writeIORef buttonsRef newBtns
        renderGame
      _ -> return ()

  -- Controls + container
  controls <- UI.div # set UI.class_ "controls" #+ [ pure resetBtn, pure puzzleSelect, pure modeButton ]
  headerControls <- UI.div # set UI.class_ "header-controls" #+ [ pure difficultySelect ]

  -- Contenedor principal que centra todo
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
        [ "/* ROOT / BACKGROUND */"
        , "body { margin:0; font-family: 'Orbitron', sans-serif; background: #000; color: #00ffff; }"
        , ".nonogram-container {"
        , "  min-height:100vh;"
        , "  display:flex; flex-direction: column; align-items:center; padding:50px;"
        , "  background: #000000;"
        , "  border-radius: 25px;"
        , "  box-shadow: 0 0 50px #00ffff, 0 0 80px #ff00ff;"
        , "}"
        , ""
        , "/* Contenedor principal centrado */"
        , ".main-content {"
        , "  display: flex;"
        , "  flex-direction: column;"
        , "  align-items: center;"
        , "  justify-content: center;"
        , "  width: 100%;"
        , "}"
        , ""
        , "/* TÍTULO */"
        , ".game-title {"
        , "  font-family: 'Orbitron', sans-serif; font-size: 36px; margin: 20px 0; color: #00ffff;"
        , "  text-shadow: 0 0 10px #00ffff;"
        , "  letter-spacing: 2px;"
        , "}"
        , ".game-info, .error-info { margin:10px 0; font-size:20px; color: #00ffff; text-shadow: 0 0 10px #00ffff; }"
        , ".game-info { font-size: 24px; font-weight: bold; color: #00ff00; text-shadow: 0 0 15px #00ff00; }"
        , ""
        , "/* CLUES - Posicionamiento correcto y centrado */"
        , ".top-placeholder, .left-placeholder {"
        , "  display: flex;"
        , "  justify-content: center;"
        , "  width: 100%;"
        , "  margin: 0 auto;"
        , "}"
        , ".top-row { display:flex; align-items:flex-end; gap:10px; margin-top:20px; justify-content: center; }"
        , ".corner { width:80px; height:96px; }"
        , ".top-clues { display:flex; gap:6px; }"
        , ".top-clue { display:flex; flex-direction: column; align-items:center; justify-content:flex-end; width:54px; height:96px; font-size:16px; color:#00ffff; text-shadow: 0 0 5px #00ffff; }"
        , ".mid-row { display:flex; gap:10px; margin-top:10px; justify-content: center; align-items: flex-start; }"
        , ".left-clues { display:flex; flex-direction: column; gap:6px; width:90px; margin-top: 20px; }"
        , ".left-clue { display:flex; justify-content:flex-end; align-items:center; padding-right:8px; height:54px; font-size:16px; color:#00ffff; text-shadow: 0 0 5px #00ffff; }"
        , ""
        , "/* BOARD */"
        , ".non-board { display:flex; flex-direction: column; gap:8px; background: rgba(0,0,0,0.5); padding: 20px; border-radius: 15px; border: 2px solid #00ffff; box-shadow: 0 0 30px #00ffff; margin: 0 auto; }"
        , ".non-row { display:flex; gap:8px; }"
        , ""
        , "/* CELDAS - estilo neón con bordes luminosos */"
        , ".non-cell {"
        , "  width:54px; height:54px; display:flex; align-items:center; justify-content:center;"
        , "  border-radius:8px;"
        , "  border: 2px solid #00ffff;"
        , "  background: #111111;"
        , "  cursor:pointer; font-weight:bold; font-family: 'Orbitron', sans-serif; color: #ffffff;"
        , "  transition: all 0.2s ease;"
        , "  font-size: 24px;"
        , "  text-shadow: 0 0 5px currentColor;"
        , "  box-shadow: 0 0 15px #00ffff;"
        , "}"
        , ".non-cell:hover:not([disabled]) { transform: translateY(-3px); box-shadow: 0 0 25px #00ffff, 0 0 40px #ff00ff; }"
        , ".non-cell[disabled] { cursor: default; }"
        , ""
        , "/* Estados de celda neon */"
        , ".non-cell.unknown { background: #111111; border-color: #00ffff; }"
        , ".non-cell.filled-correct { background: #00ff00; border-color: #00ff00; color: #000; box-shadow: 0 0 20px #00ff00, 0 0 40px #00ff00; }"
        , ".non-cell.marked-empty { background: #111111; color: #ff0000; border-color: #ff0000; box-shadow: 0 0 20px #ff0000, 0 0 30px #ff0000; }"
        , ""
        , "/* BOTONES estilo neón */"
        , ".neon-btn {"
        , "  padding:12px 24px; border-radius:12px;"
        , "  background: transparent;"
        , "  border: 2px solid #00ffff;"
        , "  color: #00ffff;"
        , "  font-family: 'Orbitron', sans-serif;"
        , "  font-size: 16px;"
        , "  cursor: pointer;"
        , "  text-shadow: 0 0 5px #00ffff;"
        , "  box-shadow: 0 0 15px #00ffff;"
        , "  transition: all 0.2s ease;"
        , "}"
        , ".neon-btn:hover { transform: translateY(-3px); box-shadow: 0 0 25px #00ffff, 0 0 40px #ff00ff; }"
        , ".controls { display:flex; gap:15px; margin-top:30px; align-items: center; justify-content: center; }"
        , ".game-reset-button { }"
        , ".mode-button { width:60px; height:50px; position: relative; border-radius: 8px; }"
        , ".fill-mode { background: #00ff00; border-color: #00ff00; box-shadow: 0 0 15px #00ff00; }"
        , ".fill-mode:hover { box-shadow: 0 0 25px #00ff00, 0 0 40px #00ff00; }"
        , ".x-mode { background: #ff0000; border-color: #ff0000; box-shadow: 0 0 15px #ff0000; }"
        , ".x-mode:hover { box-shadow: 0 0 25px #ff0000, 0 0 40px #ff0000; }"
        , ".x-mode::after {"
        , "  content: 'X';"
        , "  position: absolute;"
        , "  top: 50%;"
        , "  left: 50%;"
        , "  transform: translate(-50%, -50%);"
        , "  font-size: 24px;"
        , "  color: #ffffff;"
        , "  font-weight: bold;"
        , "}"
        , ".back-button { align-self:flex-start; margin-bottom:20px; }"
        , ""
        , "/* Selectors con estilo neon y texto visible */"
        , ".header-controls { margin-bottom: 20px; display: flex; justify-content: center; }"
        , ".header-controls select, .controls select {"
        , "  padding:10px 15px; margin-right:15px; border-radius:8px;"
        , "  background: #111111 !important; color:#00ffff !important;"
        , "  border: 2px solid #00ffff !important;"
        , "  font-family: 'Orbitron', sans-serif !important;"
        , "  font-size: 14px !important;"
        , "  box-shadow: 0 0 10px #00ffff !important;"
        , "  appearance: none !important;"
        , "  -webkit-appearance: none !important;"
        , "  -moz-appearance: none !important;"
        , "}"
        , ".header-controls select option, .controls select option {"
        , "  background: #000000 !important; color: #00ffff !important;"
        , "  padding: 10px !important;"
        , "}"
        , ".header-controls select:focus, .controls select:focus {"
        , "  outline: none !important;"
        , "  border-color: #ff00ff !important;"
        , "  box-shadow: 0 0 20px #ff00ff !important;"
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