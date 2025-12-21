module Nonogram.Game
  ( initialGame
  , toggleCell
  , setCellTo
  , setFilled
  , setMarkedEmpty
  , resetGame
  , checkVictory
  , validateRow
  , validateCol
  , puzzleFromSample5
  , puzzleFromSample10
  ) where

import Nonogram.Types
import Data.List (group)
import Data.Maybe (fromMaybe)

-- Crear tablero vacío (todas las celdas Unknown)
mkEmptyBoard :: Size -> [[Cell]]
mkEmptyBoard (rows, cols) = replicate rows (replicate cols Unknown)

-- Inicializar juego con Puzzle dado (maxErrors por defecto 3)
initialGame :: Puzzle -> Game
initialGame puzzle =
  let size = puzzleSize puzzle
  in Game
     { gameBoard  = mkEmptyBoard size
     , gamePuzzle = puzzle
     , gameState  = Playing
     , startTime  = Nothing
     , errorCount = 0
     , maxErrors  = 3
     }

-- AUTOCOMPLETE: si una fila/col tiene ya exactamente las celdas rellenas correctas,
-- convierte el resto Unknown -> LockedEmpty (X bloqueada)
autoCompleteLines :: Game -> Game
autoCompleteLines game@Game{ gameBoard = board, gamePuzzle = puzzle } =
  let (rows, cols) = puzzleSize puzzle
      sol = puzzleSolution puzzle

      processRow r oldRow =
        let solRow = sol !! (r-1)
            trueCols = [ c | (c, b) <- zip [1..] solRow, b ]
            filledCols = [ c | c <- [1..cols], oldRow !! (c-1) == Filled ]
            filledCorrect = all (`elem` trueCols) filledCols
            neededCount = length trueCols
        in if filledCorrect && length filledCols == neededCount
           then [ if cell == Unknown then LockedEmpty else cell | cell <- oldRow ]
           else oldRow

      processColumns brd =
        foldl (\b c -> processCol c b) brd [1..cols]

      processCol c brd =
        let solCol = map (!! (c-1)) sol
            trueRows = [ r | (r,b) <- zip [1..] solCol, b ]
            filledRows = [ r | r <- [1..rows], (brd !! (r-1) !! (c-1)) == Filled ]
            filledCorrect = all (`elem` trueRows) filledRows
            neededCount = length trueRows
        in if filledCorrect && length filledRows == neededCount
           then [ let oldRow = row
                  in take (c-1) oldRow ++ [ if (oldRow !! (c-1)) == Unknown then LockedEmpty else (oldRow !! (c-1)) ] ++ drop c oldRow
                | row <- brd ]
           else brd

      afterRows = [ processRow r (board !! (r-1)) | r <- [1..rows] ]
      afterCols = processColumns afterRows
  in game { gameBoard = afterCols }

-- Alternar celda en posición (fila,col)
-- Ciclo: Unknown -> Filled -> MarkedEmpty (X) -> Unknown
-- LockedEmpty no se puede cambiar
-- Si el usuario marca X donde debe ir Filled, contamos error y REVELAMOS (Filled).
toggleCell :: Position -> Game -> Game
toggleCell (r, c) game@Game{ gameBoard = board, gamePuzzle = puzzle, errorCount = errs, maxErrors = maxE } =
  let (rows, cols) = puzzleSize puzzle
  in if r < 1 || r > rows || c < 1 || c > cols
     then game
     else
       let zeroR = r - 1
           zeroC = c - 1
           oldRow = board !! zeroR
           oldCell = oldRow !! zeroC
           intendedNew = case oldCell of
             LockedEmpty -> LockedEmpty
             Unknown     -> Filled
             Filled      -> MarkedEmpty
             MarkedEmpty -> Unknown

           expected = (puzzleSolution puzzle !! zeroR) !! zeroC

           -- If user tries to put X where should be Filled -> reveal and count error
           -- If user tries to put Filled where shouldn't be -> convert to X and count error
           actualNew = case intendedNew of
             MarkedEmpty | expected -> Filled
             Filled | not expected -> MarkedEmpty
             _ -> intendedNew

           actionError = case intendedNew of
             Filled -> not expected
             MarkedEmpty -> expected
             _ -> False

           newRow = take zeroC oldRow ++ [actualNew] ++ drop (zeroC + 1) oldRow
           newBoard = take zeroR board ++ [newRow] ++ drop (zeroR + 1) board

           newErrs = if actionError then errs + 1 else errs
           provisionalGame = game { gameBoard = newBoard, errorCount = newErrs }

           autoed = autoCompleteLines provisionalGame

           finalGame = if newErrs >= maxE
                       then autoed { gameState = Lost }
                       else if checkVictory autoed
                            then autoed { gameState = Won }
                            else autoed
       in finalGame

-- Establecer una celda a un estado concreto (Filled / MarkedEmpty)
-- Si el usuario marca X donde debe ser Filled -> reveal Filled and count error.
setCellTo :: Position -> Cell -> Game -> Game
setCellTo (r, c) newCell game@Game{ gameBoard = board, gamePuzzle = puzzle, errorCount = errs, maxErrors = maxE } =
  let (rows, cols) = puzzleSize puzzle
  in if r < 1 || r > rows || c < 1 || c > cols
     then game
     else
       let zeroR = r - 1
           zeroC = c - 1
           oldRow = board !! zeroR
           oldCell = oldRow !! zeroC
           -- no cambiamos LockedEmpty
           intendedNew = case oldCell of
             LockedEmpty -> LockedEmpty
             _ -> newCell

           expected = (puzzleSolution puzzle !! zeroR) !! zeroC

           actualNew = case intendedNew of
             MarkedEmpty | expected -> Filled
             _ -> intendedNew

           actionError = case intendedNew of
             Filled -> not expected
             MarkedEmpty -> expected
             _ -> False

           newRow = take zeroC oldRow ++ [actualNew] ++ drop (zeroC + 1) oldRow
           newBoard = take zeroR board ++ [newRow] ++ drop (zeroR + 1) board

           newErrs = if actionError then errs + 1 else errs
           provisionalGame = game { gameBoard = newBoard, errorCount = newErrs }
           autoed = autoCompleteLines provisionalGame
           finalGame = if newErrs >= maxE
                       then autoed { gameState = Lost }
                       else if checkVictory autoed
                            then autoed { gameState = Won }
                            else autoed
       in finalGame

setFilled :: Position -> Game -> Game
setFilled pos = setCellTo pos Filled

setMarkedEmpty :: Position -> Game -> Game
setMarkedEmpty pos = setCellTo pos MarkedEmpty

-- Reiniciar el juego (tablero a Unknown, estado Playing)
resetGame :: Game -> Game
resetGame g@Game{ gamePuzzle = puzzle } = g { gameBoard = mkEmptyBoard (puzzleSize puzzle), gameState = Playing, errorCount = 0 }

-- Calcular pistas de una línea (fila o columna) a partir del estado actual del tablero
lineCluesFromCells :: [Cell] -> [Int]
lineCluesFromCells cells =
  let nums = map (\c -> if c == Filled then 1 else 0) cells
      groups = group nums
  in [ length g | g@(x:_) <- groups, x == 1 ]

-- Obtener fila/col (1-based)
getRow :: [[Cell]] -> Int -> [Cell]
getRow board r = board !! (r - 1)

getCol :: [[Cell]] -> Int -> [Cell]
getCol board c = map (!! (c - 1)) board

-- Verifica si el puzzle está resuelto comparando con la solución (solo Filled == True)
checkVictory :: Game -> Bool
checkVictory Game{ gameBoard = board, gamePuzzle = puzzle } =
  let sol = puzzleSolution puzzle
      rows = fst (puzzleSize puzzle)
      cols = snd (puzzleSize puzzle)
  in and [ let expected = (sol !! (r-1)) !! (c-1)
               actualIsFilled = (board !! (r-1)) !! (c-1) == Filled
           in expected == actualIsFilled
         | r <- [1..rows], c <- [1..cols]
         ]

-- VALIDACIÓN ASISTIDA (se mantiene en el backend pero UI no la usa)
validateRow :: Int -> Game -> Game
validateRow r game@Game{ gameBoard = board, gamePuzzle = puzzle, errorCount = errs, maxErrors = maxE } =
  let (rows, cols) = puzzleSize puzzle
  in if r < 1 || r > rows then game
     else
       let zeroR = r - 1
           solRow = puzzleSolution puzzle !! zeroR
           oldRow = board !! zeroR

           (newRow, addedErrors) = unzip $
             [ let expected = solRow !! (c-1)
                   cur = oldRow !! (c-1)
                   (newCell, err) = case (cur, expected) of
                     (_, True)  -> (Filled, 0)
                     (Filled, False) -> (MarkedEmpty, 1)
                     (MarkedEmpty, True) -> (MarkedEmpty, 1)
                     (x, _) -> (x, 0)
               in (newCell, err)
             | c <- [1..cols]
             ]
           newBoard = take zeroR board ++ [newRow] ++ drop (zeroR+1) board
           totalAdded = sum addedErrors
           newErrs = errs + totalAdded
           provisionalGame = game { gameBoard = newBoard, errorCount = newErrs }
           autoed = autoCompleteLines provisionalGame
       in if newErrs >= maxE
          then autoed { gameState = Lost }
          else if checkVictory autoed
               then autoed { gameState = Won }
               else autoed

validateCol :: Int -> Game -> Game
validateCol c game@Game{ gameBoard = board, gamePuzzle = puzzle, errorCount = errs, maxErrors = maxE } =
  let (rows, cols) = puzzleSize puzzle
  in if c < 1 || c > cols then game
     else
       let zeroC = c - 1
           sol = puzzleSolution puzzle
           processRow idx oldRow =
             let expected = (sol !! idx) !! zeroC
                 cur = oldRow !! zeroC
                 (newRow, err) = case (cur, expected) of
                   (_, True) -> (take zeroC oldRow ++ [Filled] ++ drop (zeroC+1) oldRow, 0)
                   (Filled, False) -> (take zeroC oldRow ++ [MarkedEmpty] ++ drop (zeroC+1) oldRow, 1)
                   (MarkedEmpty, True) -> (take zeroC oldRow ++ [MarkedEmpty] ++ drop (zeroC+1) oldRow, 1)
                   _ -> (oldRow, 0)
             in (newRow, err)
           pairs = [ processRow i (board !! i) | i <- [0..(rows-1)] ]
           newBoard = map fst pairs
           totalAdded = sum (map snd pairs)
           newErrs = errs + totalAdded
           provisionalGame = game { gameBoard = newBoard, errorCount = newErrs }
           autoed = autoCompleteLines provisionalGame
       in if newErrs >= maxE
          then autoed { gameState = Lost }
          else if checkVictory autoed
               then autoed { gameState = Won }
               else autoed

-- Exportar accesos a puzzles de ejemplo
puzzleFromSample5 :: Puzzle
puzzleFromSample5 = samplePuzzle5

puzzleFromSample10 :: Puzzle
puzzleFromSample10 = samplePuzzle10