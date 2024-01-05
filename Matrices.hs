module Matrices
( Vector
, Matriz
, numColumnas
, separa
, matrizLista
, listaMatriz
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

numColumnas :: (Num a) => Matriz a -> Int
numColumnas = snd . snd . bounds

separa :: Int -> [a] -> [[a]]
separa _ [] = []
separa n xss = (take n xss):separa n (drop n xss)

matrizLista :: (Num a) => Matriz a -> [[a]]
matrizLista p = (separa (numColumnas p) (elems p))

listaMatriz :: Num a => [[a]] -> Matriz a
listaMatriz xss = listArray ((1,1), (f, c)) (concat xss)
   where f = length xss
         c = (length.head) xss 