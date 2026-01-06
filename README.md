Minijuegos en Haskell es una aplicación de escritorio-web ligera y elegante que reúne tres minijuegos clásicos bajo una misma interfaz moderna: Tic-Tac-Toe (Cero y Cruz), Buscaminas y Nonogramas. Diseñado con principios funcionales y una arquitectura modular, el proyecto demuestra cómo construir experiencias interactivas robustas y mantenibles usando Haskell y Threepenny-GUI.

Características principales
------------------------------
- Implementación puramente funcional: los estados y las reglas del juego son expresados con claridad, inmutabilidad y composabilidad, lo que facilita razonamiento y pruebas.
- Experiencias de juego completas:
  - Tic-Tac-Toe con IA basada en minimax para partidas retadoras y didácticas.
  - Buscaminas con generación procedural de tableros y revelado en cascada (algoritmo flood-fill) para una jugabilidad fluida.
  - Nonogramas con pistas numéricas, marcado de celdas y herramientas de validación/ayuda para resolver puzles.
- Interfaz unificada y responsiva construida con Threepenny-GUI: ejecutable localmente y accesible desde el navegador.
- Código modular y bien organizado: ideal para continuar desarrollos, integrar nuevas características o usar como material docente.
- Menú central desde donde se selecciona cualquiera de los tres minijuegos
    
Requisitos
----------
- GHC (recomendado 8.10+)
- Cabal (cabal-install) o Stack
- Dependencias gestionadas por Cabal/Stack (Threepenny-GUI, random, array, entre otras)

Instalación y ejecución
-----------------------
Clona el repositorio y ejecuta con Cabal:

git clone https://github.com/LianetSoto/Declarative-programming-project.git
cd Declarative-programming-project

Con cabal
cabal v2-update
cabal v2-build
cabal v2-run

O con Stack:

stack setup
stack build
stack run

La aplicación abrirá una ventana local (Threepenny) y podrás acceder a la interfaz en tu navegador (por defecto en http://localhost:8023).

Estructura del repositorio
-----------------------------------
- app/Main.hs — Punto de entrada y enrutamiento entre vistas.
- src/Menu — Vista del menú principal con estilo y navegación.
- src/TicTacToe — Lógica, IA y vista del juego Tic-Tac-Toe.
- src/MinesWeeper — Generación de tableros, lógica (flood-fill) y vista del Buscaminas.
- src/Nonogram — Representación y lógica de Nonogramas (interacción y validación).
- static/ — Recursos estáticos (CSS, imágenes y scripts).
