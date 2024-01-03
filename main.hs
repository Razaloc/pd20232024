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


-- ---------------------------------------------------------------------
-- Tipos de los vectores y de las matrices                            --
-- ---------------------------------------------------------------------

-- Los vectores son tablas cuyos índices son números naturales.
type Vector a = Array Int a
 
-- Las matrices son tablas cuyos índices son pares de números
-- naturales. 
type Matriz a = Array (Int,Int) a

-- ---------------------------------------------------------------------

-- Recibe un numero y crea una mazmorra no resuelta de ese tamaño, indices de (1,1) a (n,n)
nuevaMazmorra :: Num a => Int -> Matriz a
nuevaMazmorra n = listArray ((1,1), (f, c)) (concat xss)
   where f = length xss
         c = (length.head) xss 
         xss = replicate n . replicate n $ 1

-- Imprime el estado actual de la mazmorra
imprimeMazmorra :: (Show a, Num a) => Array (Int, Int) a -> IO ()
imprimeMazmorra arr = putStr (unlines [unwords [show (arr ! (x, y)) | x <- [1..n]] | y <- [1..n]])
    where n = (numColumnas arr)

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

-- Esto debería ir en el modulo de matriz
numColumnas:: Num a => Matriz a -> Int
numColumnas = snd . snd . bounds

-- Esto debería ir en el modulo de matriz
separa :: Int -> [a] -> [[a]]
separa _ [] = []
separa n xss = (take n xss):separa n (drop n xss)

-- Esto debería ir en el modulo de matriz
matrizLista :: Num a => Matriz a -> [[a]]
matrizLista p = (separa (numColumnas p) (elems p))

-- Comprueba condición de victoria
finalizado :: (Num a, Eq a) => Matriz a -> Bool
finalizado m =  m ! (x,x) == 0
    where x = numColumnas m

-- Resuelve sala, cambia el valor x,y de la matriz a 0
resuelveSala :: (Num a) => (Int, Int) -> Matriz a -> Matriz a
resuelveSala (x,y) ar = ar // [((x,y), 0)]






--separa2 :: String -> [Int]
--separa2 s = [read [c] | c <- s, c /= ',']



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
          --n <- leeDigito "Elija una opción: "
          -- (nuevaMazmorra n)
--        (j,t) <- leerPartida
        putChar '\n'
--        juego t j
        else do 
            putChar '\n'
--            nim

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    nuevoJuego
