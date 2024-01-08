module Partida
( Partida,
Mazmorra,
Aventurero, 
Objeto, 
Pos,
nuevoAventurero,
nuevoAventurero',
nuevaMazmorra,
imprimeAventurero,
imprimeMazmorra,
textoPartida,
textoMazmorra,
textoAventurero,
finalizado,
esResueltaSala,
resuelveSala,
cambiaValorSala,
actualizaAventurero,
obtieneObjeto,
posicionValida,
mueve,
obtenerPosicionAventurero,
obtenerMochilaAventurero,
obtenerVidaAventurero,
obtenerNombreAventurero
) where

import Matrices
import Data.Array
import Data.Ratio

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
  ,mochila::Objeto} deriving (Show)

-- FUNCIONES INFORMACIÓN DEL AVENTURERO
obtenerNombreAventurero :: Aventurero -> String
obtenerNombreAventurero = nombre

obtenerPosicionAventurero :: Aventurero -> Pos
obtenerPosicionAventurero = posicion

obtenerVidaAventurero :: Aventurero -> Int
obtenerVidaAventurero = vida

obtenerMochilaAventurero :: Aventurero -> Objeto
obtenerMochilaAventurero = mochila

-- Crea un nuevo aventurero por defecto
nuevoAventurero :: Aventurero
nuevoAventurero = Aventurero {nombre = "Heroe", posicion = (1,1), vida = 100, mochila = ("Palo",1)}

nuevoAventurero' :: String -> Pos -> Int -> Objeto -> Aventurero
nuevoAventurero' nombre posicion vida mochila = Aventurero {nombre = nombre, posicion = posicion, vida = vida, mochila = mochila}

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
textoAventurero a = (nombre a) ++ "\n" ++ (show (posicion a)) ++ "\n" ++ (show (vida a)) ++ "\n" ++ (show (mochila a) )


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

