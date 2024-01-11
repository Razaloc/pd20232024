-- PD: miniproyecto
-- Juego de aventuras.
-- Universidad de Sevilla
-- Rafael Garcia y Elena Ayora
-- =====================================================================
import System.IO
import Juego

-- ---------------------------------------------------------------------

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    nuevoJuego
