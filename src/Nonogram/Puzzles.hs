{-# LANGUAGE OverloadedStrings #-}
module Nonogram.Puzzles
  ( loadPuzzlesFromFile
  ) where

import Nonogram.Types (Puzzle(..), Size)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Control.Exception (catch, SomeException)
import Data.List (transpose, group)
import Data.Maybe (fromMaybe)

-- JSON esperado: { "solution": [[0,1,...], ...], "difficulty": "easy" (opcional) }
data PuzzleJson = PuzzleJson
  { pSolution :: [[Int]]
  , pDifficulty :: Maybe String
  } deriving (Show)

instance FromJSON PuzzleJson where
  parseJSON = withObject "PuzzleJson" $ \o -> do
    sol  <- o .: "solution"
    diff <- o .:? "difficulty"
    return PuzzleJson { pSolution = sol, pDifficulty = diff }

-- calcula pistas (listas de longitudes de bloques True) para una línea booleana
cluesFromBoolLine :: [Bool] -> [Int]
cluesFromBoolLine bs =
  let groups = group bs
      trues = [ length g | g@(x:_) <- groups, x ]
  in if null trues then [] else trues

toPuzzle :: PuzzleJson -> Puzzle
toPuzzle pj =
  let solInts = pSolution pj
      solBools = map (map (/= 0)) solInts
      h = length solBools
      w = if null solBools then 0 else length (head solBools)
      rowsClues = map cluesFromBoolLine solBools
      colsClues = map cluesFromBoolLine (transpose solBools)
      diff = fromMaybe "easy" (pDifficulty pj)
  in Puzzle { puzzleRows = rowsClues
            , puzzleCols = colsClues
            , puzzleSize = (h, w)
            , puzzleSolution = solBools
            , puzzleDifficulty = diff
            }

-- Cargar lista de puzzles desde un archivo JSON que contenga un array de puzzles
loadPuzzlesFromFile :: FilePath -> IO [Puzzle]
loadPuzzlesFromFile path = do
  content <- BL.readFile path `catch` handler
  case eitherDecode content of
    Left err -> do
      putStrLn $ "Error parsing puzzles JSON: " ++ err
      return []
    Right objs -> return (map toPuzzle objs)
  where
    handler :: SomeException -> IO BL.ByteString
    handler e = do
      putStrLn $ "No se pudo leer " ++ path ++ ": " ++ show e
      return "[]"