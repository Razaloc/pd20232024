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

-- ---------------------------------------------------------------------

type Partida = (Mazmorra, Aventurero)
-- El tipo mazmorra representa el estado actual de la partida, es una matriz de ceros y unos que representan si una sala está resuelta o no.
type Mazmorra = Matriz Char
-- Tipo posición, coordenadas dentro de la mazmorra.
type Pos = (Int, Int)

type Objeto = String
-- data player 
data Aventurero = Aventurero 
  {nombre::String
  ,posicion::Pos
  ,vida::Int 
  ,nivel::Float 
  ,mochila::[Objeto]} deriving (Show)

nuevoAventurero :: Aventurero
nuevoAventurero = Aventurero {nombre = "Alex", posicion = (1,1), vida = 100, nivel = 0, mochila = []}

-- Recibe un numero y crea una mazmorra no resuelta de ese tamaño, indices de (1,1) a (n,n)
nuevaMazmorra :: Int -> Mazmorra
nuevaMazmorra n = listArray ((1,1), (f, c)) (concat xss)
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

separa2 :: String -> [Int]
separa2 s = [read [c] | c <- s, c /= ',']

cargarPartida :: IO ()
cargarPartida = do 
    putChar '\n'
    existe <- doesFileExist "guarda.txt"
    if existe then do
        contenido <- readFile "guarda.txt"
        let lineas  = lines contenido
        let mazmorra = read (head lineas)
        let
            nombre = lineas !! 1
            posicion = ([ x | x <- separa2 (lineas !! 2)]!!1, [ x | x <- separa2 (lineas !! 2)]!!3)  
            vida = read (lineas !! 3)
            nivel = read (lineas !! 4)
            mochila = read (lineas !! 5)
        juego (listaMatriz mazmorra, nuevoAventurero {nombre = nombre, posicion = posicion, vida = vida, nivel = nivel, mochila = mochila})
        else do
            putStrLn "No hay ninguna partida guardada." 
            nuevoJuego


guardaPartida :: (Mazmorra, Aventurero) -> IO ()
guardaPartida (m, a) = writeFile "guarda.txt" $ textoPartida (m, a)


actualizaAventurero :: Aventurero -> Pos -> Aventurero
actualizaAventurero a (x,y) =  a {posicion = (x,y)}  

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

posicionValida :: Partida -> Bool
posicionValida (m,a)
    | fst(posicion a) <= 0 = False
    | fst(posicion a) > z = False 
    | snd(posicion a) <= 0 = False 
    | snd(posicion a) > z = False 
    | otherwise = True
    where z = numColumnas m

mueve :: Partida -> Int -> Partida
mueve p@(m,a) n
    | n == 1 =  (m, a{posicion = posicion (snd (mueveNorte p))})
    | n == 2 =  (m, a{posicion = posicion (snd (mueveSur p))})
    | n == 3 =  (m, a{posicion = posicion (snd (mueveEste p))})
    | n == 4 =  (m, a{posicion = posicion (snd (mueveOeste p))})
    | otherwise = p

-- Nuevo juego (con opcion a carga de partida por implementar)
nuevoJuego :: IO ()
nuevoJuego = do
    putStrLn "1. Nuevo juego"
    putStrLn "2. Cargar partida de archivo"
    o <- leeDigito "Elija una opción: "
    if o == 2 then do
        putChar '\n'
        cargarPartida
        else do 
            putChar '\n'
            n <- leeDigito "Elije un tamaño de la mazmorra que quieres visitar: "
            juego ((nuevaMazmorra n), nuevoAventurero)

juego :: Partida -> IO ()
juego partida@(mazmorra, aventurero) = do
    guardaPartida partida
    putStrLn "\n\n\n\n\n\n\n\n\n\n\n"
    imprimeAventurero partida
    imprimeMazmorra partida
    -- Comienza un turno normal
    -- Display contenido de sala
    -- Resuelve la sala
    if esResueltaSala partida 
        then do putStrLn "Ya no hay nada nuevo en esta sala. Ya habías pasado por aquí."
        else do putStrLn "¡Ocurre un encuentro!"

    -- postEscuentro -->estado de la partidadespuésdelencuentro, se modifica el estado de la sala cambia a 0
    let postEncuentro = resuelveSala partida
    -- Comprueba la condición de victoria
    if not (finalizado postEncuentro) then do
        n <- leeDigito "Elige en que dirección deseas moverte: usa las flechas para elegir: \n 1.Norte \n 2.Sur \n 3.Este \n 4.Oeste \n"

    -- postMovimiento -->estado de la partidadespués del movimiento, la posicion del aventurero cambia
        let postMovimiento = mueve postEncuentro n
        if posicionValida postMovimiento then do juego postMovimiento 
        else do putStrLn "Jugada no válida, inténtelo de nuevo."
                juego postEncuentro
    else do
        putStrLn "Enhorabuena has ganado. FIN DEL JUEGO."


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    nuevoJuego
