module Juego
( nuevoJuego
) where

import Data.Char
import System.Environment
import System.Directory
import System.IO
import Matrices
import Monstruos
import Partida

-- Lee digito para seleccionar acción (por implementar), lee un segundo dígito para descartar en "enter". Recursivo.
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

salasResueltas :: Partida -> Int
salasResueltas p@(m,a) = sum [ 1 | x <- (concat(matrizLista m)), x == 'O']

titulo :: Int -> String
titulo n = case n of 
    0 -> "¿¿Cero salas??"
    1 -> "Solo has resuelto una sala, puedes hacerlo mejor."
    2 -> "Has resuelto dos salas, estás por buen camino."
    3 -> "Tres salas... Ni las tortugas van tan lentas, date brio"
    4 -> "Genial, ya empiezas a ir por buen camino, cuatro salas"
    5 -> "Cinco salas... Un cinco suele ser un aprobado, pero esto no se aplica si eres un héroe"
    6 -> "Seis salas, estas progresando adecuadamente, sigue así"
    7 -> "Se nota que eres un buen estratega, sigue así, siete salas"
    8 -> "Ocho salas resueltas, nada más"
    9 -> "Dale amigo/a, estas ya a nada de ser considerado un aventurero de rango E, nueve salas resueltas"
    10 -> "Diez salas resueltas. Ojala yo en mis examenes, eres un buen héroe"
    11 -> "Por lo menos sabemos que no moriras por culpa de un slime, once salas resueltas."
    _ -> "¡¡ Eres increible has resuelto más de once salas !!"
    
tituloArma :: Objeto -> String
tituloArma (_ ,1) = "¿Todavías estas con el palo de la fregona? ... cobarde ..."
tituloArma (_ ,2) = "Conseguiste palo de babas al matar el slime, seras conocido como el señor de los mocos"
tituloArma (_ ,5) = "Terminaste la partida con daga envenenada, ni tan mal, pero hay cosas mejores"
tituloArma (_ ,8) = "Ganaste con la espada de hueso, relagalasela a tu enamorada, seguro que le encanta (jaja no, por favor no)"
tituloArma (_ ,10) = "Terminaste la partida con el baston Descomunal, ya sabes, usalo para volver a casa"
tituloArma (_ ,13) = "Ganaste la partida con las garras de lobo, apartir de ahora eres El gran hombre lobo pecho peludo que no usa tutú"
tituloArma (_ ,16) = "Conseguiste la lanza envenenada, apartir de ahora seras conocido como Pescador en quiebra"
tituloArma (_ ,25) = "Terminaste la partida con el 'Hacha del minotauro' ahora eres el señor del hacha, puedes ir a cortar leña"
tituloArma (_ ,999) = "Venciste al dragón y conseguiste la espada del elegido, tu título es RandomX"
tituloArma _ = ""





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
            eleccion <- leeDigito "Elige una opcion: \n 1.Atacar \n 2.Huir \n"
            if eleccion == 1 then do 
                putStrLn "¡Comienza el ataque!"
                --Lucha
                let aventureroHerido = lucha aventurero enemigo
                if (obtenerVidaAventurero aventureroHerido) <= 0 then do
                    putStrLn "Estas muerto. FIN DEL JUEGO."
                    putStrLn (titulo (salasResueltas partida))
                    nuevoJuego
                else do 
                    let recompensado = obtieneObjeto aventureroHerido (obtenerRecompensaMonstruo enemigo)
        -- postEscuentroExito -->estado de la partidadespuésdelencuentro con lucha, se modifica el estado de la sala cambia a 0
                    let postEncuentroExito = resuelveSala (mazmorra, recompensado)
                    putStrLn ("Obtienes la recompensa:                " ++ (fst (obtenerRecompensaMonstruo enemigo)) ++ ". Con una fuerza de: " ++(show (snd (obtenerRecompensaMonstruo enemigo))))
                    if not (finalizado postEncuentroExito) then do
                        seleccionDestino postEncuentroExito
                        else do
                            putStrLn "Enhorabuena has ganado. FIN DEL JUEGO."
                            putStrLn (titulo (salasResueltas postEncuentroExito))
                            putStrLn (tituloArma (obtenerMochilaAventurero recompensado))
                            nuevoJuego
                            

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
                        putStrLn (titulo (salasResueltas postEncuentroHuida))
                        putStrLn (tituloArma (obtenerMochilaAventurero aventurero))
                        nuevoJuego

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
