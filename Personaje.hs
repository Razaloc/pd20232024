module Personaje
    ( Personaje,
    crearPersonaje,
    obtenerPosicion,
    obtenerPuntosVida,
    obtenerObjetos
    ) where

data Personaje = Personaje
    { posicion :: (Int, Int),
    puntosVida :: Int,
    objetos :: [String]
    }

-- Función para crear al personaje
crearPersonaje :: Personaje
crearPersonaje = Personaje { posicion = (0, 0), puntosVida = 100, objetos = [] }

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
