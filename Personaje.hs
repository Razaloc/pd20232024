module Personaje
    ( Personaje,
    crearPersonaje,
    obtenerPosicion,
    obtenerPuntosVida,
    obtenerObjetos,
    obtenerPuntosAtaque
    ) where

-- importo la librería Random, para general un valor aleatorio para los puntos de ataque del personaje
import System.Random

data Personaje = Personaje
    { posicion :: (Int, Int),
      puntosVida :: Int,
      objetos :: [String],
      puntosAtaque :: Int
    } deriving (Show)


-- Función para crear al personaje
-- (en principio los puntos de ataque van a ser estáticos, pero lo ideal sería que fuera aleatorio)
crearPersonaje :: Personaje
crearPersonaje = Personaje { posicion = (0, 0), puntosVida = 100, objetos = [], puntosAtaque = 10 }


-- FUNCIONES INFORMACIÓN DEL PERSONAJE
-- Posición en la que está (sala de la mazmorra)
obtenerPosicion :: Personaje -> (Int, Int)
obtenerPosicion = posicion

-- Puntos de vida que tiene el persona
obtenerPuntosVida :: Personaje -> Int
obtenerPuntosVida = puntosVida

-- Objetos que ha ido recolectando el personaje en las diferentes salas
obtenerObjetos :: Personaje -> [String]
obtenerObjetos = objetos

-- Puntos de ataques del personaje
obtenerPuntosAtaque :: Personaje -> Int
obtenerPuntosAtaque = puntosAtaque