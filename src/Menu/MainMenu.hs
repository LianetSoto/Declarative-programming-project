{-# LANGUAGE OverloadedStrings #-}

module Menu.MainMenu where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Data.IORef
import Core.Types
import Control.Monad (void)

menuView :: Window -> IORef Screen -> UI () -> UI ()
menuView window screenRef render = do

  -- GLOBAL STYLES
  void $ UI.addStyleSheet window
    "https://fonts.googleapis.com/css2?family=Orbitron:wght@400;600&display=swap"

  let neonStyle =
        [ ("background", "rgba(0,0,0,0.85)")
        , ("font-family", "'Orbitron', sans-serif")
        , ("min-height", "100vh")
        , ("display", "flex")
        , ("justify-content", "center")
        , ("align-items", "center")
        , ("background-image", "url('../static/menu_bg.jpg')")
        , ("background-size", "cover")
        , ("background-position", "center")
        ]

  body <- getBody window
  element body # set children []
  element body # set UI.style neonStyle

  -- TITLE
  title <- UI.h1
    # set UI.text "HASKELL MINI GAMES"
    # set UI.style
        [ ("color", "#00ffff")
        , ("text-shadow", "0 0 10px #00ffff")
        , ("margin-bottom", "30px")
        ]

  -- BUTTON STYLE
  let buttonStyle color =
        [ ("background", "transparent")
        , ("color", color)
        , ("border", "2px solid " ++ color)
        , ("padding", "15px 40px")
        , ("font-size", "18px")
        , ("margin", "10px 0")       
        , ("cursor", "pointer")
        , ("border-radius", "12px")
        , ("text-shadow", "0 0 8px " ++ color)
        , ("box-shadow", "0 0 15px " ++ color)
        ]

  -- BUTTONS
  btnTtt <- UI.button
    # set UI.text "Tic Tac Toe"
    # set UI.style (buttonStyle "#ff00ff")

  btnMs <- UI.button
    # set UI.text "Minesweeper"
    # set UI.style (buttonStyle "#00ff00")

  btnNg <- UI.button
    # set UI.text "Nonograms"
    # set UI.style (buttonStyle "#00ffff")

  btnHelp <- UI.button
    # set UI.text "Help"
    # set UI.style (buttonStyle "#ffaa00")

  -- EVENTS
  on UI.click btnTtt $ \_ -> liftIO (writeIORef screenRef TicTacToe) >> render
  on UI.click btnMs  $ \_ -> liftIO (writeIORef screenRef Minesweeper) >> render
  on UI.click btnNg  $ \_ -> liftIO (writeIORef screenRef Nonogram) >> render

  -- CONTAINER 
  menuBox <- UI.div
    # set UI.style
        [ ("background", "rgba(0,0,0,0.75)")
        , ("padding", "50px")
        , ("border-radius", "20px")
        , ("text-align", "center")
        , ("box-shadow", "0 0 40px #00ffff")
        , ("display", "flex")
        , ("flex-direction", "column")  
        , ("align-items", "center")
        ]
    #+ map pure [title, btnTtt, btnMs, btnNg, btnHelp]

  void $ element body #+ [pure menuBox]
