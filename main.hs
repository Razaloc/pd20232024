-- PD: miniproyecto
-- Juego de aventuras.
-- Universidad de Sevilla
-- Rafael Garcia y Elena Ayora
-- =====================================================================
{-# LANGUAGE OverloadedStrings #-}
import Data.Array
import Data.Ratio
import Data.Char
import System.Environment
import System.Directory
import System.IO
import Matrices
import Monstruos

-- ---------------------------------------------------------------------

type Partida = (Mazmorra, Aventurero)
-- El tipo mazmorra representa el estado actual de la partida, es una matriz de ceros y unos que representan si una sala está resuelta o no.
type Mazmorra = Matriz Char
-- Tipo posición, coordenadas dentro de la mazmorra.
type Pos = (Int, Int)

type Objeto = (String, Int)
-- data player 
data Aventurero = Aventurero 
  {nombre::String
  ,posicion::Pos
  ,vida::Int 
  ,nivel::Float 
  ,mochila::Objeto} deriving (Show)

-- Crea un nuevo aventurero por defecto
nuevoAventurero :: Aventurero
nuevoAventurero = Aventurero {nombre = "Heroe", posicion = (1,1), vida = 100, nivel = 0, mochila = ("Palo",1)}

-- Recibe un numero y crea una mazmorra no resuelta de ese tamaño, indices de (1,1) a (n,n)
nuevaMazmorra :: Int -> Mazmorra
nuevaMazmorra n = (listArray ((1,1), (f, c)) (concat xss)) // [((1,1), 'O')]
   where f = length xss
         c = (length.head) xss 
         xss = replicate n . replicate n $ '?'

-- Imprime aventurero
imprimeAventurero :: Partida -> IO ()
imprimeAventurero (_,a) = putStrLn (show a) 

-- Aventurero en formato texto para guardado
textoAventurero :: Aventurero -> String
textoAventurero a = (nombre a) ++ "\n" ++ (show (posicion a)) ++ "\n" ++ (show (vida a)) ++ "\n" ++ (show (nivel a)) ++ "\n" ++ (show (mochila a) )


-- Imprime el estado actual de la mazmorra en pantalla
imprimeMazmorra :: Partida -> IO ()
imprimeMazmorra (m,a) = putStr (unlines [unwords [show (mx ! (x, y)) | x <- [1..n]] | y <- [1..n]])
    where n = (numColumnas m)
          mx = m // [((posicion a), 'X')]

-- Mazmorra en formato texto para guardado
textoMazmorra :: Mazmorra -> String
textoMazmorra m =  show (matrizLista m) ++ "\n" 

-- Texto de la info esencial de la partida
textoPartida :: Partida-> String
textoPartida (m,a) = (textoMazmorra m) ++ (textoAventurero a)


-- Comprueba condición de victoria
finalizado :: Partida -> Bool
finalizado (m, _) =  m ! (x,x) == 'O'
    where x = numColumnas m

-- Resuelve sala, cambia el valor (x, y) de la matriz a 0
resuelveSala :: Partida -> Partida
resuelveSala (mazmorra, aventurero) = (mazmorra // [((posicion aventurero), 'O')], aventurero)

--Consulta si la sala ha sido resuelta
esResueltaSala :: Partida -> Bool
esResueltaSala  (m,a) =  m ! (posicion a) == 'O'

-- Cambia el valor x,y de la matriz. De momento sin utilidad. 
cambiaValorSala :: Pos -> Mazmorra -> Char -> Mazmorra
cambiaValorSala (x,y) ar a = ar // [((x,y), a)]

-- Lee digito para seleccionar acción (por implementar)
leeDigito :: String -> IO Int
leeDigito c = do
    hSetBuffering stdout NoBuffering
    putStr c 
    x <- getChar 
    putStr "\n"
    if isDigit x 
    then do return (read [x])
    else do putStrLn "ERROR: Entrada incorrecta"
            leeDigito c

-- Util para parsear listas
separa2 :: String -> [Int]
separa2 s = [read [c] | c <- s, c /= ',']

-- Guardado de partida en un archivo para posteriormente poder cargarla (carga no implementada)
guardaPartida :: (Mazmorra, Aventurero) -> IO ()
guardaPartida (m, a) = writeFile "guarda.txt" $ textoPartida (m, a)

-- Actualiza la posición del aventurero
actualizaAventurero :: Aventurero -> Pos -> Aventurero
actualizaAventurero a (x,y) =  a {posicion = (x,y)}  

-- Actualiza la posición del aventurero
obtieneObjeto :: Aventurero -> Objeto -> Aventurero
obtieneObjeto a o =  a {mochila = o}  

-- Suma de dos tuplas que representan posiciones o movimientos
suma :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
suma (x, y) (u, v) = (x+u, y+v)

-- Funciones de gestion del movimiento del aventurero
mueveNorte :: Partida -> Partida
mueveNorte (m,aventurero) = (m,(aventurero { posicion = (suma (posicion aventurero) (0,-1))}))

mueveSur :: Partida -> Partida
mueveSur (m,aventurero) = (m,(aventurero { posicion = (suma (posicion aventurero) (0,1))}))

mueveEste :: Partida -> Partida
mueveEste (m,aventurero) = (m,(aventurero { posicion = (suma (posicion aventurero) (1,0))}))

mueveOeste :: Partida -> Partida
mueveOeste (m,aventurero) = (m,(aventurero { posicion = (suma (posicion aventurero) (-1,0))}))

-- Verificar que la posición no se sale de los márgenes de la mazmorra
posicionValida :: Partida -> Bool
posicionValida (m,a)
    | fst(posicion a) <= 0 = False
    | fst(posicion a) > z = False 
    | snd(posicion a) <= 0 = False 
    | snd(posicion a) > z = False 
    | otherwise = True
    where z = numColumnas m

-- Cambia la posicion del aventurero y devuelve el nuevo estado de la partida 
mueve :: Partida -> Int -> Partida
mueve p@(m,a) n
    | n == 1 =  (m, a{posicion = posicion (snd (mueveNorte p))})
    | n == 2 =  (m, a{posicion = posicion (snd (mueveSur p))})
    | n == 3 =  (m, a{posicion = posicion (snd (mueveEste p))})
    | n == 4 =  (m, a{posicion = posicion (snd (mueveOeste p))})
    | otherwise = p

-- El sistema de batalla resume y simula una batalla por turnos en la cual cada uno hace el daño de su arma o propio al otro hasta que muere el enemigo. 
lucha :: Aventurero -> Monstruo -> Aventurero
lucha av monst = nuevoAventurero {nombre = (nombre av), posicion = (posicion av), vida = (sufre (vida av) (mochila av) monst), mochila = (mochila av)}

sufre :: Int -> Objeto -> Monstruo -> Int
sufre pV (obj, valor) monst = pV - ((obtenerPuntosAtaqueMonstruo monst) * (division valor monst))

division :: Int -> Monstruo -> Int
division valor monst = (obtenerPuntosVidaMonstruo monst) `div` valor

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
            n <- leeDigito "Elije un tamaño de la mazmorra que quieres visitar: "
            juego ((nuevaMazmorra n), nuevoAventurero)

--Ejecución de un turno
juego :: Partida -> IO ()
juego partida@(mazmorra, aventurero) = do
    guardaPartida partida
    putStrLn "\n\n\n\n\n\n\n\n\n\n\n"
    imprimeMazmorra partida
    imprimeAventurero partida
    -- Comienza un turno normal
    -- Resuelve la sala
    if esResueltaSala partida 
        then do
            putStrLn "Ya no hay nada nuevo en esta sala. Ya habías pasado por aquí."
            siguienteTurno partida

        else do 
            -- Muestra el contenido de la sala
            putStrLn "¡Ocurre un encuentro!"
            -- De momento solo hay opcion de enemigo pero podría ser algo positivo.
            let enemigo = head listaMonstruos
            imprimeMonstruo (obtenerArteMonstruo enemigo)
            putStrLn ((obtenerNombreMonstruo enemigo)++ ". Vida: " ++ (show (obtenerPuntosVidaMonstruo enemigo)) ++ " Ataque: " ++ (show (obtenerPuntosAtaqueMonstruo enemigo)))
            eleccion <- leeDigito "Elige en que dirección deseas moverte: usa las flechas para elegir: \n 1.Atacar \n 2.Huir \n"
            if eleccion == 1 then do 
                putStrLn "¡Comienza el ataque!"
                --Lucha
                let aventureroHerido = lucha aventurero enemigo
                if (vida aventureroHerido) <= 0 then do
                    putStrLn "Estas muerto. FIN DEL JUEGO"
                    nuevoJuego
                else do 
                    let recompensadoPartida = obtieneObjeto aventureroHerido (obtenerRecompensaMonstruo enemigo)
        -- postEscuentro -->estado de la partidadespuésdelencuentro, se modifica el estado de la sala cambia a 0
                    let postEncuentroExito = resuelveSala (mazmorra, recompensadoPartida)
                    putStrLn ("Obtienes la recompensa:                " ++ (fst (obtenerRecompensaMonstruo enemigo)) ++ ". Con una fuerza de: " ++(show (snd (obtenerRecompensaMonstruo enemigo))))
                    siguienteTurno postEncuentroExito

            else do 
        -- postEscuentro -->estado de la partidadespuésdelencuentro, se modifica el estado de la sala cambia a 0
                let postEncuentroHuida = resuelveSala partida
                putStrLn "\n\n\n\n\n\n\n\n\n\n\n"
                putStrLn "Huiste del monstruo pero no obtendrás recompensa en esta sala."
                if not (finalizado partida) then do
                    siguienteTurno postEncuentroHuida
                    else do
                        putStrLn "Enhorabuena has ganado. FIN DEL JUEGO."


siguienteTurno :: Partida -> IO ()
siguienteTurno partida = do 
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
    

{- 



    -- Comprueba la condición de victoria
    


-}

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    nuevoJuego
