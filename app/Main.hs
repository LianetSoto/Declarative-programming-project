{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Data.IORef
import Control.Monad (void)

import Core.Types
import Menu.MainMenu
import TicTacToe.View
import Nonogram.View
import MinesWeeper.View

main :: IO ()
main = do
  let config = defaultConfig
        { jsStatic = Just "static" }  
  startGUI config setup

setup :: Window -> UI ()
setup window = do
  void $ return window # set title "Minijuegos en Haskell"

  -- Estado global de navegación
  screenRef <- liftIO $ newIORef Menu

  -- Función central de renderizado
  let render :: UI ()
      render = do
        body <- getBody window
        element body # set children []

        screen <- liftIO $ readIORef screenRef
        case screen of
          Menu      -> menuView window screenRef render
          TicTacToe -> ticTacToeView window screenRef render
          -- Minesweeper -> minesweeperView window screenRef render
          Nonogram    -> nonogramView window screenRef render
          Minesweeper -> minesweeperView window screenRef render
          -- Nonogram    -> nonogramView window screenRef render

  -- Render inicial
  render
