module Menu.MainMenu where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Data.IORef
import Core.Types
import Control.Monad (void)

menuView :: Window -> IORef Screen -> UI () -> UI ()
menuView window screenRef render = do
  title <- UI.h1 # set UI.text "Menú Principal"

  btnTtt <- UI.button # set UI.text "Tic Tac Toe"
  btnMs  <- UI.button # set UI.text "Buscaminas"
  btnNg  <- UI.button # set UI.text "Nonogramas"

  on UI.click btnTtt $ \_ -> do
    liftIO $ writeIORef screenRef TicTacToe
    render

  on UI.click btnMs $ \_ -> do
    liftIO $ writeIORef screenRef Minesweeper
    render

  on UI.click btnNg $ \_ -> do
    liftIO $ writeIORef screenRef Nonogram
    render

  container <- UI.div #. "menu"
    #+ map pure [title, btnTtt, btnMs, btnNg]

  void $ getBody window #+ [pure container]
