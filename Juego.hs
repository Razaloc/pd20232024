module Juego
( nuevoJuego
) where

import Data.Array
import Data.Ratio
import Data.Char
import System.Environment
import System.Directory
import System.IO
import Matrices
import Monstruos
import Partida

-- Lee digito para seleccionar acción (por implementar), lee un segundo dígito para descartar en "enter"
leeDigito :: String -> IO Int
leeDigito c = do
    hSetBuffering stdout NoBuffering
    putStr c 
    x <- getChar 
    y <- getChar 
    putStr "\n"
    if isDigit x 
    then do return (read [x])
    else do putStrLn "ERROR: Entrada incorrecta"
            leeDigito c

-- Guardado de partida en un archivo para posteriormente poder cargarla (carga no implementada)
guardaPartida :: (Mazmorra, Aventurero) -> IO ()
guardaPartida (m, a) = writeFile "guarda.txt" $ textoPartida (m, a)



-- El sistema de batalla resume y simula una batalla por turnos en la cual cada uno hace el daño de su arma o propio al otro hasta que muere el enemigo. 
lucha :: Aventurero -> Monstruo -> Aventurero
lucha av monst = nuevoAventurero' (obtenerNombreAventurero av) (obtenerPosicionAventurero av) (sufre (obtenerVidaAventurero av) (obtenerMochilaAventurero av) monst) (obtenerMochilaAventurero av)

sufre :: Int -> Objeto -> Monstruo -> Int
sufre pV (obj, valor) monst = pV - ((obtenerPuntosAtaqueMonstruo monst) * (division valor monst))

division :: Int -> Monstruo -> Int
division valor monst = (obtenerPuntosVidaMonstruo monst) `div` valor

-- Hacemos que la dificultad del monstruo dependa de la posición considerando el numero x+y y el tamaño de la lista de mosntruos.
enemigoDigno :: Partida -> [Monstruo] -> Monstruo
enemigoDigno p@(m,a) ms = ms !! ((ubicacion * (length ms) `div` ((numColumnas m )+ (numColumnas m)))-1)
    where ubicacion = (fst (obtenerPosicionAventurero a)) + (fst (obtenerPosicionAventurero a))

-- Nuevo juego (con opcion a carga de partida por implementar)
nuevoJuego :: IO ()
nuevoJuego = do
    putStrLn "1. Nuevo juego"
    putStrLn "2. Cerrar"
    o <- leeDigito "Elija una opción: "
    if o == 2 then do
        putChar '\n'
        --cargarPartida
        else do 
            putChar '\n'
            n <- leeDigito "Tu objetivo será llegar desde el extremo Noroeste de la mazmorra hasta su extremo Sureste. Elije un tamaño de la mazmorra que quieres visitar: "
            juego ((nuevaMazmorra n), nuevoAventurero)

--Ejecución de un turno
juego :: Partida -> IO ()
juego partida@(mazmorra, aventurero) = do
    guardaPartida partida
    putStrLn "\n\n\n\n\n\n\n\n\n\n\n"
    -- Comienza un turno normal
    -- Resuelve la sala
    if esResueltaSala partida 
        then do
            putStrLn "Ya no hay nada nuevo en esta sala. Ya habías pasado por aquí."
            if not (finalizado partida) then do
                seleccionDestino partida
                else do
                    putStrLn "Enhorabuena has ganado. FIN DEL JUEGO."
        else do 
            -- Muestra el contenido de la sala
            putStrLn "¡Ocurre un encuentro!"
            -- De momento solo hay opcion de enemigo pero podría ser algo positivo.
            let enemigo = enemigoDigno partida listaMonstruos
            imprimeMonstruo (obtenerArteMonstruo enemigo)
            putStrLn ((obtenerNombreMonstruo enemigo)++ ". Vida: " ++ (show (obtenerPuntosVidaMonstruo enemigo)) ++ " Ataque: " ++ (show (obtenerPuntosAtaqueMonstruo enemigo)))
            eleccion <- leeDigito "Elige en que dirección deseas moverte: usa las flechas para elegir: \n 1.Atacar \n 2.Huir \n"
            if eleccion == 1 then do 
                putStrLn "¡Comienza el ataque!"
                --Lucha
                let aventureroHerido = lucha aventurero enemigo
                if (obtenerVidaAventurero aventureroHerido) <= 0 then do
                    putStrLn "Estas muerto. FIN DEL JUEGO"
                    nuevoJuego
                else do 
                    let recompensadoPartida = obtieneObjeto aventureroHerido (obtenerRecompensaMonstruo enemigo)
        -- postEscuentroExito -->estado de la partidadespuésdelencuentro con lucha, se modifica el estado de la sala cambia a 0
                    let postEncuentroExito = resuelveSala (mazmorra, recompensadoPartida)
                    putStrLn ("Obtienes la recompensa:                " ++ (fst (obtenerRecompensaMonstruo enemigo)) ++ ". Con una fuerza de: " ++(show (snd (obtenerRecompensaMonstruo enemigo))))
                    if not (finalizado postEncuentroExito) then do
                        seleccionDestino postEncuentroExito
                        else do
                            putStrLn "Enhorabuena has ganado. FIN DEL JUEGO."

            else do 
        -- postEscuentro -->estado de la partidadespuésdelencuentro sins lucha, se modifica el estado de la sala cambia a 0
                let postEncuentroHuida = resuelveSala partida
                putStrLn "\n\n\n\n\n\n\n\n\n\n\n"
                putStrLn "Huiste del monstruo pero no obtendrás recompensa en esta sala."
        -- Comprueba la condición de victoria
                if not (finalizado postEncuentroHuida) then do
                    seleccionDestino postEncuentroHuida
                    else do
                        putStrLn "Enhorabuena has ganado. FIN DEL JUEGO."

-- Consulta la direccion del movimiento y llama al siguiente turno
seleccionDestino :: Partida -> IO ()
seleccionDestino partida = do 
    imprimeMazmorra partida
    imprimeAventurero partida
    n <- leeDigito "Elige en que dirección deseas moverte: usa las flechas para elegir: \n 1.Norte \n 2.Sur \n 3.Este \n 4.Oeste \n"
    -- postMovimiento -->estado de la partidadespués del movimiento, la posicion del aventurero cambia
    let postMovimiento = mueve partida n
    if posicionValida postMovimiento then do
        juego postMovimiento 
    else do 
        putStrLn "Jugada no válida, inténtelo de nuevo."
        juego partida
