module Matrices
    ( Vector, 
    Matriz, 
    numColumnas, 
    separa, 
    matrizLista,
    primeraPosicion,
    ultimaPosicion
    ) where


import Data.Array
import Data.Ratio


--Esta parte debe ir al moduo de las matrices
-- ---------------------------------------------------------------------
-- Tipos de los vectores y de las matrices                            --
-- ---------------------------------------------------------------------

-- Los vectores son tablas cuyos índices son números naturales.
type Vector a = Array Int a
 
-- Las matrices son tablas cuyos índices son pares de números
-- naturales. 
type Matriz a = Array (Int,Int) a

-- Esto debería ir en el modulo de matriz
numColumnas :: (Num a) => Matriz a -> Int
numColumnas = snd . snd . bounds

-- Esto debería ir en el modulo de matriz
separa :: Int -> [a] -> [[a]]
separa _ [] = []
separa n xss = (take n xss):separa n (drop n xss)

-- Esto debería ir en el modulo de matriz
matrizLista :: (Num a) => Matriz a -> [[a]]
matrizLista p = (separa (numColumnas p) (elems p))

