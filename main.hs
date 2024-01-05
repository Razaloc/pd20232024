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
-- El tipo mazmorra representa el estado actual de la partida, es una matriz de ceros y unos que representan si una sala está resuelta o no.
type Mazmorra = Matriz Int
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
         xss = replicate n . replicate n $ 1

-- Imprime aventurero
imprimeAventurero :: Aventurero -> IO ()
imprimeAventurero a = putStrLn (show a) 

-- Aventurero en formato texto para guardado
textoAventurero :: Aventurero -> String
textoAventurero a = (nombre a) ++ "\n" ++ (show (posicion a)) ++ (show (vida a)) ++ "\n" ++ (show (nivel a)) ++ "\n" ++ (show (mochila a) )


-- Imprime el estado actual de la mazmorra en pantalla
imprimeMazmorra :: Mazmorra -> IO ()
imprimeMazmorra arr = putStr (unlines [unwords [show (arr ! (x, y)) | x <- [1..n]] | y <- [1..n]])
    where n = (numColumnas arr)

-- Mazmorra en formato texto para guardado
textoMazmorra :: Mazmorra -> String
textoMazmorra m =  show (matrizLista m)

-- Texto de la info esencial de la partida
textoPartida :: (Mazmorra, Aventurero) -> String
textoPartida (m,a) = (textoMazmorra m) ++ "/n" ++ (textoAventurero a)


-- Comprueba condición de victoria
finalizado :: Mazmorra -> Bool
finalizado m =  m ! (x,x) == 0
    where x = numColumnas m

-- Resuelve sala, cambia el valor (x, y) de la matriz a 0
resuelveSala :: Pos -> Mazmorra -> Mazmorra
resuelveSala (x,y) ar = ar // [((x,y), 0)]

--Consulta si la sala ha sido resuelta
esResueltaSala :: Pos -> Mazmorra -> Bool
esResueltaSala (x,y) m =  m ! (x,y) == 0

-- Cambia el valor x,y de la matriz. De momento sin utilidad. 
cambiaValorSala :: Pos -> Mazmorra -> Int -> Mazmorra
cambiaValorSala (x,y) ar a = ar // [((x,y), a)]


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
--cargarPartida ::  IO (Mazmorra, Aventurero)
--cargarPartida = do
--    putChar '\n'
--    putStrLn "Indique el nombre del archivo: "
--    f <- getLine -- para leer una línea entera (coge una cadena de caracteres)
--    existe <- doesFileExist f
--    if existe then do
--        contenido <- readFile f
--        let lineas  = lines contenido
--        let aventurero = read (head lineas)
--        let heroe = Aventurero {nombre = nombre , vida = vida, nivel = nivel, mochila = mochila}
--        return (mazmorra, heroe)
--        else do putStrLn "El archivo indicado no existe, intételo de nuevo"
--                cargarPartida


guardaPartida :: (Mazmorra, Aventurero) -> IO ()
guardaPartida (m, a) = writeFile "guarda.txt" $ textoPartida (m, a)


actualizaAventurero :: Aventurero -> Pos -> Aventurero
actualizaAventurero a (x,y) =  a {posicion = (x,y)}  


suma (x, y) (u, v) = (x+u, y+v)

-- Nuevo juego (por implementar)
nuevoJuego :: IO ()
--nuevoJuego = undefined
nuevoJuego = do
    putStrLn "1. Nuevo juego"
    putStrLn "2. Cargar partida de archivo"
    o <- leeDigito "Elija una opción: "
    if o == 2 then do
--        (maz,avent) <- cargarPartida
        putChar '\n'
--        juego maz avent
        else do 
            putChar '\n'
            n <- leeDigito "Elije un tamaño de la mazmorra que quieres visitar: "
            juego (nuevaMazmorra n) nuevoAventurero

--imposiblePosicion :: Pos -> Mazmorra -> Bool
--imposiblePosicion (x, y) m
--    | x <= 0 = False
--    | x > z = False 
--    | y <= 0 = False 
--    | y > z = False 
--    | otherwise = True
--    where z = numColumnas m

--regulate ::  IO Int ->  Int 
--regulate n = case n of
--    1 -> 1
--    2 -> 2
--    3 -> 3
--    4 -> 4
--    otherwise  -> 0
--        

-- Indica el movimiento deseado, ante un input no valido el movimiento será (0,0)
--movimiento :: Int -> Pos
--movimiento o = 
--    case o of
--        1 -> (1,0)
--        2 -> (1,0)
--        3 -> (1,0)
--        4 -> (1,0)
--        otherwise  -> (0,0)

--preguntaMovimiento ::  IO Int
--preguntaMovimiento = do
--    putStrLn "Indica hacia donde quieres moverte: " 
--    putStrLn "1. Norte"
--    putStrLn "2. Sur"
--    putStrLn "3. Este"
--    putStrLn "4. Oeste"
--    o <- leeDigito "Elija una opción: "
--    return o

juego :: Mazmorra -> Aventurero -> IO ()
juego mazmorra aventurero = do
    imprimeAventurero aventurero
    imprimeMazmorra mazmorra
    -- Comienza un turno normal
    -- Display contenido de sala
    -- Resuelve la sala
    let postMazmorra = resuelveSala (posicion aventurero) mazmorra
    -- Comprueba la condición de victoria
    if not (finalizado mazmorra) then do
        putStrLn "Elige en que dirección deseas moverte: "

-- movimiento recibe aventurero y deuelve aventurero al que hay que comprobar si la posición es válida.
--        let movimienDeseado = movimiento (regulate (preguntaMovimiento))
--        let postAventurero = actualizaAventurero aventurero movimienDeseado 
--        validar posicion 
--        if imposiblePosicion (posicion postAventurero) mazmorra then do
--            putStrLn "No puedes moverte en esa dirección, intentalo de nuevo"
--            juego postMazmorra aventurero
--            else 
--                juego postMazmorra postAventurero
        else
            putStrLn "Enhorabuena has ganado"
--    if (esResueltaSala posicion mazmorra) then do
--        let actualizadoAventurero = movimiento aventurero
--        putStrLn "Esta sala ya ha sido resuelta, ¿hacia donde deseas ir?"
--        else do
--            resuelveSala posicion mazmorra
--            putStrLn "¡Explorando una nueva sala encontraste algo!"



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
