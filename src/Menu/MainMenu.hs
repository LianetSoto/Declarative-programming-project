{-# LANGUAGE OverloadedStrings #-}

module Menu.MainMenu where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Data.IORef
import Core.Types
import Control.Monad (void)

menuView :: Window -> IORef Screen -> UI () -> UI ()
menuView window screenRef render = do
    void $ return window # set title "Haskell Mini Games"

    ---------------------------------------------------------
    -- LIMPIAR BODY PRIMERO
    ---------------------------------------------------------
    body <- getBody window
    element body # set children []

    ---------------------------------------------------------
    -- INSERTAR CSS COMO EN LA VISTA DE BUSCAMINAS
    ---------------------------------------------------------
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
          , "}"
          , ".container { max-width: 800px; width: 100%; }"
          , ".menu-container {"
          , "  background: rgba(255, 255, 255, 0.05);"
          , "  backdrop-filter: blur(10px);"
          , "  border-radius: 20px;"
          , "  padding: 40px;"
          , "  box-shadow: 0 20px 60px rgba(0, 0, 0, 0.5), 0 0 40px rgba(138, 43, 226, 0.2);"
          , "  border: 1px solid rgba(255, 255, 255, 0.1);"
          , "}"
          , "h1 {"
          , "  text-align: center;"
          , "  color: #fff;"
          , "  font-size: 3em;"
          , "  margin-bottom: 10px;"
          , "  text-shadow: 0 0 20px rgba(138, 43, 226, 0.8), 0 0 40px rgba(138, 43, 226, 0.5);"
          , "  font-weight: bold;"
          , "}"
          , ".subtitle {"
          , "  text-align: center;"
          , "  color: #a0a0a0;"
          , "  margin-bottom: 40px;"
          , "  font-size: 1.1em;"
          , "}"
          , ".games-grid {"
          , "  display: grid;"
          , "  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));"
          , "  gap: 20px;"
          , "  margin-bottom: 25px;"
          , "}"
          , ".game-button {"
          , "  background: linear-gradient(135deg, rgba(138, 43, 226, 0.2) 0%, rgba(75, 0, 130, 0.3) 100%);"
          , "  border: 2px solid rgba(138, 43, 226, 0.5);"
          , "  border-radius: 15px;"
          , "  padding: 30px 20px;"
          , "  color: #fff;"
          , "  font-size: 1.1em;"
          , "  font-weight: 600;"
          , "  cursor: pointer;"
          , "  transition: all 0.3s ease;"
          , "  box-shadow: 0 10px 30px rgba(0, 0, 0, 0.3);"
          , "}"
          , ".game-button:hover {"
          , "  transform: translateY(-5px);"
          , "  box-shadow: 0 15px 40px rgba(138, 43, 226, 0.4), 0 0 30px rgba(138, 43, 226, 0.3);"
          , "  border-color: rgba(138, 43, 226, 0.8);"
          , "  background: linear-gradient(135deg, rgba(138, 43, 226, 0.3) 0%, rgba(75, 0, 130, 0.4) 100%);"
          , "}"
          , ".game-icon {"
          , "  width: 60px;"
          , "  height: 60px;"
          , "  margin: 0 auto 15px;"
          , "  display: block;"
          , "  filter: drop-shadow(0 0 10px rgba(138, 43, 226, 0.6));"
          , "}"
          , ".help-button {"
          , "  display: block;"
          , "  width: 100%;"
          , "  background: linear-gradient(135deg, rgba(255, 215, 0, 0.2) 0%, rgba(255, 140, 0, 0.3) 100%);"
          , "  border: 2px solid rgba(255, 215, 0, 0.5);"
          , "  border-radius: 15px;"
          , "  padding: 20px;"
          , "  color: #ffd700;"
          , "  font-size: 1.1em;"
          , "  font-weight: 600;"
          , "  cursor: pointer;"
          , "  transition: all 0.3s ease;"
          , "  box-shadow: 0 10px 30px rgba(0, 0, 0, 0.3);"
          , "}"
          , ".help-button:hover {"
          , "  transform: translateY(-3px);"
          , "  box-shadow: 0 15px 40px rgba(255, 215, 0, 0.3);"
          , "  border-color: rgba(255, 215, 0, 0.8);"
          , "}"
          ]

    styleEl <- UI.div # set UI.html ("<style>" ++ styles ++ "</style>")
    -- ahora sí: el body ya estaba limpio, así que el <style> no se borra
    void $ element body #+ [pure styleEl]

    ---------------------------------------------------------
    -- CONTENEDORES
    ---------------------------------------------------------
    container     <- UI.div # set UI.class_ "container"
    menuContainer <- UI.div # set UI.class_ "menu-container"

    title <- UI.h1
      # set UI.text "HASKELL MINI GAMES"

    subtitle <- UI.p
      # set UI.text "Selecciona un juego para comenzar"
      # set UI.class_ "subtitle"

    gamesGrid <- UI.div # set UI.class_ "games-grid"

    ---------------------------------------------------------
    -- BOTÓN TIC TAC TOE
    ---------------------------------------------------------
    btnTtt <- UI.button # set UI.class_ "game-button"
    iconTtt <- UI.div # set UI.html
      "<svg class='game-icon' viewBox='0 0 100 100' xmlns='http://www.w3.org/2000/svg'>\
      \ <line x1='33' y1='10' x2='33' y2='90' stroke='#8a2be2' stroke-width='3'/>\
      \ <line x1='67' y1='10' x2='67' y2='90' stroke='#8a2be2' stroke-width='3'/>\
      \ <line x1='10' y1='33' x2='90' y2='33' stroke='#8a2be2' stroke-width='3'/>\
      \ <line x1='10' y1='67' x2='90' y2='67' stroke='#8a2be2' stroke-width='3'/>\
      \ <line x1='15' y1='15' x2='28' y2='28' stroke='#ff6b9d' stroke-width='4' stroke-linecap='round'/>\
      \ <line x1='28' y1='15' x2='15' y2='28' stroke='#ff6b9d' stroke-width='4' stroke-linecap='round'/>\
      \ <circle cx='50' cy='50' r='10' fill='none' stroke='#00d4ff' stroke-width='4'/>\
      \ <line x1='72' y1='72' x2='85' y2='85' stroke='#ff6b9d' stroke-width='4' stroke-linecap='round'/>\
      \ <line x1='85' y1='72' x2='72' y2='85' stroke='#ff6b9d' stroke-width='4' stroke-linecap='round'/>\
      \</svg>"
    element btnTtt #+ [pure iconTtt, UI.string "Tic Tac Toe"]

    ---------------------------------------------------------
    -- BOTÓN BUSCAMINAS
    ---------------------------------------------------------
    btnMs <- UI.button # set UI.class_ "game-button"
    iconMs <- UI.div # set UI.html
      "<svg class='game-icon' viewBox='0 0 100 100' xmlns='http://www.w3.org/2000/svg'>\
      \ <circle cx='40' cy='60' r='20' fill='#1a1a1a'/>\
      \ <path d='M 50 45 Q 55 35, 60 30 Q 63 27, 66 25' stroke='#555' stroke-width='3.5' fill='none' stroke-linecap='round'/>\
      \ <circle cx='67' cy='23' r='5' fill='#ff6b00'/>\
      \ <circle cx='67' cy='23' r='7' fill='#ff8c00' opacity='0.6'/>\
      \ <circle cx='67' cy='23' r='9' fill='#ffa500' opacity='0.3'/>\
      \ <rect x='58' y='73' width='5' height='9' rx='1.5' fill='#8a2be2'/>\
      \ <line x1='60.5' y1='73' x2='60.5' y2='40' stroke='#8a2be2' stroke-width='4' stroke-linecap='round'/>\
      \ <path d='M 60.5 40 Q 75 43, 82 47 Q 76 51, 60.5 56' fill='#ff0000'/>\
      \</svg>"
    element btnMs #+ [pure iconMs, UI.string "Buscaminas"]

    ---------------------------------------------------------
    -- BOTÓN NONOGRAMA
    ---------------------------------------------------------
    btnNg <- UI.button # set UI.class_ "game-button"
    iconNg <- UI.div # set UI.html
      "<svg class='game-icon' viewBox='0 0 100 100' xmlns='http://www.w3.org/2000/svg'>\
      \ <text x='35' y='15' fill='#00d4ff' font-size='12' font-weight='bold'>2</text>\
      \ <text x='55' y='15' fill='#00d4ff' font-size='12' font-weight='bold'>1</text>\
      \ <text x='75' y='15' fill='#00d4ff' font-size='12' font-weight='bold'>3</text>\
      \ <rect x='30' y='25' width='15' height='15' rx='3' fill='#8a2be2' opacity='0.8'/>\
      \ <rect x='50' y='25' width='15' height='15' rx='3' fill='none' stroke='#8a2be2' stroke-width='2'/>\
      \ <rect x='70' y='25' width='15' height='15' rx='3' fill='#8a2be2' opacity='0.8'/>\
      \ <rect x='30' y='45' width='15' height='15' rx='3' fill='#8a2be2' opacity='0.8'/>\
      \ <rect x='50' y='45' width='15' height='15' rx='3' fill='#8a2be2' opacity='0.8'/>\
      \ <rect x='70' y='45' width='15' height='15' rx='3' fill='#8a2be2' stroke='#8a2be2' stroke-width='2'/>\
      \ <rect x='30' y='65' width='15' height='15' rx='3' fill='none' stroke='#8a2be2' stroke-width='2'/>\
      \ <rect x='50' y='65' width='15' height='15' rx='3' fill='none' stroke='#8a2be2' stroke-width='2'/>\
      \ <rect x='70' y='65' width='15' height='15' rx='3' fill='#8a2be2' opacity='0.8'/>\
      \</svg>"
    element btnNg #+ [pure iconNg, UI.string "Nonograma"]

    ---------------------------------------------------------
    -- BOTÓN AYUDA
    ---------------------------------------------------------
    btnHelp <- UI.button
      # set UI.text "Ayuda"
      # set UI.class_ "help-button"

    ---------------------------------------------------------
    -- ENSAMBLAR UI
    ---------------------------------------------------------
    element gamesGrid     #+ map pure [btnTtt, btnMs, btnNg]
    element menuContainer #+ map pure [title, subtitle, gamesGrid, btnHelp]
    element container     #+ [pure menuContainer]
    element body          #+ [pure container]

    ---------------------------------------------------------
    -- EVENTOS
    ---------------------------------------------------------
    on UI.click btnTtt $ \_ -> liftIO (writeIORef screenRef TicTacToe) >> render
    on UI.click btnMs  $ \_ -> liftIO (writeIORef screenRef Minesweeper) >> render
    on UI.click btnNg  $ \_ -> liftIO (writeIORef screenRef Nonogram) >> render

    let helpText = unlines
          [""
          , "Tic Tac Toe:"
          , "- Objetivo: consigue tres en raya (horizontal, vertical o diagonal)."
          , "- Controles: haz clic en una casilla para colocar tu ficha cuando sea tu turno."
          , "- Modo: pulsa 'Modo' para alternar Humano vs Humano y Humano vs Computadora."
          , "- Si juegas contra la computadora, ésta mueve automáticamente (pequeña pausa)."
          , "- 'Nuevo Juego' reinicia el tablero; '← Volver al Menú' regresa al menú principal."
          , ""
          , "Buscaminas:"
          , "- Objetivo: descubre todas las casillas sin minas."
          , "- Controles: usa el botón 'Modo' para cambiar entre 'Revelar' y 'Bandera'."
          , "  En 'Revelar' un clic descubre la casilla; en 'Bandera' un clic coloca/quita una bandera."
          , "- 'Nuevo Juego' genera un tablero nuevo; 'Dificultad' abre opciones (Fácil/Medio/Difícil)."
          , "- Pista: el número en una casilla indica cuántas minas hay alrededor."
          , ""
          , "Nonograma:"
          , "- Objetivo: rellena las celdas para reconstruir la imagen según las pistas de filas y columnas."
          , "- Controles: pulsa el botón de modo para alternar 'Rellenar' y 'Marcar como vacío (X)'."
          , "  Luego haz clic en una celda para aplicar la acción seleccionada."
          , "- Usa los selectores para elegir dificultad y puzzle; 'Reiniciar' vuelve al estado inicial."
          , "- '← Back' regresa al menú principal."
          ]

    on UI.click btnHelp $ \_ -> do
      overlay <- UI.div # set UI.style
        [ ("position","fixed")
        , ("top","0")
        , ("left","0")
        , ("width","100%")
        , ("height","100%")
        , ("background","rgba(0,0,0,0.6)")
        , ("display","flex")
        , ("align-items","center")
        , ("justify-content","center")
        , ("z-index","9999")
        ]
      modal   <- UI.div # set UI.style
        [ ("background","linear-gradient(180deg,#1b1b2f,#0f0f1a)")
        , ("color","#fff")
        , ("padding","24px")
        , ("border-radius","12px")
        , ("max-width","720px")
        , ("width","90%")
        , ("box-shadow","0 10px 30px rgba(0,0,0,0.7)")
        , ("max-height","80vh")
        , ("overflow","auto")
        ]
      titleEl <- UI.h2  # set UI.text "Ayuda - Haskell Mini Games" # set UI.style [("margin-top","0")]
      content <- UI.pre # set UI.text helpText # set UI.style
        [ ("white-space","pre-wrap")
        , ("font-family","inherit")
        , ("background","transparent")
        , ("border","none")
        ]
      closeBtn <- UI.button # set UI.text "Cerrar" # set UI.class_ "help-button" # set UI.style [("margin-top","12px"), ("display","block"), ("margin-left","auto")]
      element modal #+ map pure [titleEl, content, closeBtn]
      element overlay #+ [pure modal]
      element body #+ [pure overlay]
      on UI.click closeBtn $ \_ -> delete overlay

    return ()
