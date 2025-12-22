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

-- Normaliza valores de difficulty desde JSON o embebidos a claves en español sin acentos
normalizeDifficulty :: String -> String
normalizeDifficulty s =
  let low = map toLower s
  in if "easy" `List.isInfixOf` low || "facil" `List.isInfixOf` low || "fácil" `List.isInfixOf` low
     then "facil"
     else if "medium" `List.isInfixOf` low || "medio" `List.isInfixOf` low
     then "medio"
     else if "hard" `List.isInfixOf` low || "dificil" `List.isInfixOf` low || "difícil" `List.isInfixOf` low
     then "dificil"
     else "facil" -- fallback

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

      -- fijar la lista de dificultades disponible en la UI (solo estas tres, en este orden)
      difficulties = [("facil","Fácil"), ("medio","Medio"), ("dificil","Difícil")]
      defaultDifficultyKey = "facil"

  -- UI: difficulty selector (solo facil/medio/dificil, default facil)
  difficultySelect <- UI.select # set (attr "id") "difficulty-select"
  forM_ difficulties $ \(k,label) -> do
    opt <- UI.option # set UI.text label # set UI.value k # set UI.style [("color", "#000000")]
    void $ element difficultySelect #+ [ pure opt ]
  -- establecer default "facil"
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
          opt <- UI.option # set UI.text (formatLabel p) # set UI.value (show i) # set UI.style [("color", "#000000")]
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

  -- mode button (ASCII only): "#" = fill, "X" = mark empty
  actionModeRef <- liftIO $ newIORef ModeFill
  modeButton <- UI.button # set UI.text "#" # set UI.class_ "mode-button neon-btn" # set (attr "title") "Modo: Rellenar (#) / Marcar X (X)"

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
    element modeButton # set UI.text (if m == ModeFill then "#" else "X")
    element modeButton # set (attr "title") (if m == ModeFill then "Modo: Rellenar (#)" else "Modo: Marcar X (X)")

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

      -- only show when won/lost
      element info # set UI.text (case gameState game of
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
            (txt, cls, enabled, style) = renderCellUI cell expected (gameState game)
        element btn
          # set UI.text txt
          # set UI.class_ ("non-cell " ++ cls)
          # set UI.enabled (enabled && gameState game == Playing)
          # set UI.style style

      return ()

    createButtons :: Int -> Int -> UI [Element]
    createButtons rCount cCount = do
      let positions = [ (r, c) | r <- [1..rCount], c <- [1..cCount] ]
      btns <- forM positions $ \_ -> UI.button # set UI.class_ "non-cell" # set UI.text ""
      forM_ (zip btns positions) $ \(btn, pos@(r,c)) -> do
        on UI.click btn $ \_ -> do
          mode <- liftIO $ readIORef actionModeRef
          case mode of
            ModeFill -> liftIO $ modifyIORef gameRef (setFilled pos)
            ModeX    -> liftIO $ modifyIORef gameRef (setMarkedEmpty pos)
          g <- liftIO $ readIORef gameRef
          when (checkVictory g) $ liftIO $ modifyIORef gameRef (\gg -> gg { gameState = Won })
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

  container <- UI.div # set UI.class_ "nonogram-container"
    #+ [ pure backButton
       , pure titleEl
       , pure headerControls
       , pure info
       , pure errCountDiv
       , pure topPlaceholder
       , pure leftPlaceholder
       , pure controls
       ]

  void $ getBody window #+ [ pure container ]

  -- initial render
  renderGame

-- Renderizar celda
-- retorna: (texto, clase-css, enabled?, estilo-inline)
renderCellUI :: Cell -> Bool -> Nonogram.Types.GameState -> (String, String, Bool, [(String,String)])
renderCellUI Unknown _ _ =
  ("", "unknown", True, [])
renderCellUI LockedEmpty _ _ =
  ("X", "locked-empty", False, [])
renderCellUI Filled expected _
  | expected  = ("", "filled-correct", False, [])
  | otherwise = ("X", "filled-wrong", False, [])
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
        , "/* TÍTULO */"
        , ".game-title {"
        , "  font-family: 'Orbitron', sans-serif; font-size: 36px; margin: 20px 0; color: #00ffff;"
        , "  text-shadow: 0 0 10px #00ffff;"
        , "  letter-spacing: 2px;"
        , "}"
        , ".game-info, .error-info { margin:10px 0; font-size:20px; color: #00ffff; text-shadow: 0 0 10px #00ffff; }"
        , ""
        , "/* CLUES */"
        , ".top-row { display:flex; align-items:flex-end; gap:10px; margin-top:20px; }"
        , ".corner { width:80px; }"
        , ".top-clues { display:flex; gap:6px; }"
        , ".top-clue { display:flex; flex-direction: column; align-items:center; justify-content:flex-end; width:54px; height:96px; font-size:16px; color:#00ffff; text-shadow: 0 0 5px #00ffff; }"
        , ".left-clues { display:flex; flex-direction: column; gap:6px; width:90px; }"
        , ".left-clue { display:flex; justify-content:flex-end; align-items:center; padding-right:8px; height:54px; font-size:16px; color:#00ffff; text-shadow: 0 0 5px #00ffff; }"
        , ""
        , "/* BOARD */"
        , ".non-board { display:flex; flex-direction: column; gap:8px; background: rgba(0,0,0,0.5); padding: 20px; border-radius: 15px; border: 2px solid #00ffff; box-shadow: 0 0 30px #00ffff; }"
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
        , ".non-cell.filled-wrong { background: #ff0000; border-color: #ff0000; color: #fff; box-shadow: 0 0 20px #ff0000, 0 0 40px #ff0000; }"
        , ".non-cell.marked-empty { background: #111111; color: #ffaa00; border-color: #ffaa00; box-shadow: 0 0 20px #ffaa00; }"
        , ".non-cell.locked-empty { background: #222222; color: #888888; border-color: #888888; border-style: dashed; }"
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
        , ".controls { display:flex; gap:15px; margin-top:30px; align-items: center; }"
        , ".game-reset-button { }"
        , ".mode-button { width:60px; height:50px; font-size:24px; }"
        , ".back-button { align-self:flex-start; margin-bottom:20px; }"
        , ""
        , "/* Selects */"
        , ".header-controls select, .controls select {"
        , "  padding:10px 15px; margin-right:15px; border-radius:8px;"
        , "  background: #111111; color:#00ffff;"
        , "  border: 2px solid #00ffff;"
        , "  font-family: 'Orbitron', sans-serif;"
        , "  font-size: 14px;"
        , "  box-shadow: 0 0 10px #00ffff;"
        , "}"
        , ".header-controls option, .controls option {"
        , "  background: #000; color: #00ffff;"
        , "}"
        , "@media (max-width:520px) { .non-cell { width:40px; height:40px; font-size: 18px; } .top-clue { width:40px; height:64px; } }"
        ]
  body <- getBody window
  styleEl <- UI.div # set UI.html ("<style>" ++ styles ++ "</style>")
  void $ element body #+ [pure styleEl]