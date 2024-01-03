-- PD: miniproyecto
-- Juego de aventuras.
-- Universidad de Sevilla
-- =====================================================================

import Data.Array
import Data.Ratio
import Data.Char
import System.Environment
import System.Directory
import System.IO
import Matrices

-- ---------------------------------------------------------------------
type Mazmorra = Matriz Int
-- data player 


-- Recibe un numero y crea una mazmorra no resuelta de ese tamaño, indices de (1,1) a (n,n)
nuevaMazmorra :: Int -> Mazmorra
nuevaMazmorra n = listArray ((1,1), (f, c)) (concat xss)
   where f = length xss
         c = (length.head) xss 
         xss = replicate n . replicate n $ 1

-- Imprime el estado actual de la mazmorra
imprimeMazmorra :: Mazmorra -> IO ()
imprimeMazmorra arr = putStr (unlines [unwords [show (arr ! (x, y)) | x <- [1..n]] | y <- [1..n]])
    where n = (numColumnas arr)




-- Comprueba condición de victoria
finalizado :: Mazmorra -> Bool
finalizado m =  m ! (x,x) == 0
    where x = numColumnas m

-- Resuelve sala, cambia el valor x,y de la matriz a 0
resuelveSala :: (Int, Int) -> Mazmorra -> Mazmorra
resuelveSala (x,y) ar = ar // [((x,y), 0)]






--separa2 :: String -> [Int]
--separa2 s = [read [c] | c <- s, c /= ',']

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

-- (por implementar) lee partida guardada
leerPartida :: IO (Int, [Int])
leerPartida = undefined
--leerPartida = do
--    putChar '\n'
--    putStrLn "Indique el nombre del archivo: "
--    f <- getLine -- para leer una línea entera (coge una cadena de caracteres)
--    existe <- doesFileExist f
--    if existe then do
--        contenido <- readFile f
--        let lineas  = lines contenido
--        let jugador = read (head lineas)
--        let tablero = [ read x | x <- separa2 (last lineas)] -- separa2 (last lineas)
--        return (jugador, tablero)
--        else do putStrLn "El archivo indicado no existe, intételo de nuevo"
--                leerPartida



-- Nuevo juego (por implementar)
nuevoJuego :: IO ()
--nuevoJuego = undefined
nuevoJuego = do
    putStrLn "1. Nuevo juego"
    putStrLn "2. Cargar partida de archivo"
    o <- leeDigito "Elija una opción: "
    if o == 2 then do
--        (j,t) <- leerPartida
        putChar '\n'
--        juego t j
        else do 
            putChar '\n'
              --n <- leeDigito "Elija una opción: "
              -- let  = (nuevaMazmorra n)
              -- let player = playerInicial
--            turno mazmorra player

turno :: Mazmorra -> Int -> IO ()
turno m p = juego m p 

juego :: Mazmorra -> Int -> IO ()
juego = undefined
--juego t j = do
--    escribeTablero t
--    if finalizado t
--    then do putStrLn $ "J " ++ (show (siguiente j)) ++ "ha ganado"
--    else do
--        putStrLn $ "J " ++ (show j)
--        f <- leeDigito "Escribe una fila: "
--        n <- leeDigito "Elige cuantas estrellas retiras: "
--        if valida t f n
--        then do juego (jugada t f n) (siguiente j)
--        else do putStrLn "Jugada no válida, inténtelo de nuevp. "
--                juego t j


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    nuevoJuego
