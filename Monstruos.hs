-- Monstruos que aparecen en la mazmorra

module Monstruos
    ( Monstruo, 
    crearMonstruo, 
    obtenerNombreMonstruo,
    obtenerPuntosVidaMonstruo, 
    obtenerPuntosAtaqueMonstruo,
    obtenerArteMonstruo,
    obtenerRecompensaMonstruo, 
    listaMonstruos,
    imprimeMonstruo
    ) where
-- Definición del tipo de dato: Monstruo
data Monstruo = Monstruo
    { nombreMonstruo :: String,
      puntosVidaMonstruo :: Int,
      puntosAtaqueMonstruo :: Int,
      arte :: String,
      recompensa :: (String, Int)

    } deriving (Show)


crearMonstruo :: String -> Int -> Int -> String -> (String, Int) -> Monstruo
crearMonstruo nombre vida ataque dibujo premio= Monstruo
    { nombreMonstruo = nombre,
      puntosVidaMonstruo = vida,
      puntosAtaqueMonstruo = ataque,
      arte = dibujo,
      recompensa = premio
    }


-- FUNCIONES INFORMACIÓN DE LOS MONSTRUOS
obtenerNombreMonstruo :: Monstruo -> String
obtenerNombreMonstruo = nombreMonstruo

obtenerPuntosVidaMonstruo :: Monstruo -> Int
obtenerPuntosVidaMonstruo = puntosVidaMonstruo

obtenerPuntosAtaqueMonstruo :: Monstruo -> Int
obtenerPuntosAtaqueMonstruo = puntosAtaqueMonstruo

obtenerArteMonstruo :: Monstruo -> String
obtenerArteMonstruo = arte

obtenerRecompensaMonstruo :: Monstruo -> (String, Int)
obtenerRecompensaMonstruo = recompensa

-- Lista de monstruos (20 monstruos)
listaMonstruos :: [Monstruo]
listaMonstruos =
    [ crearMonstruo "Esqueleto Decrépito" 15 5 esqueletoArte ("Espada de hueso", 5)
    , crearMonstruo "Araña Venenosa" 12 8 aranaArte ("Daga envenenada", 8)
--    , crearMonstruo "Ogro Descomunal" 30 12
--    , crearMonstruo "Fantasma Susurrante" 18 7
--    , crearMonstruo "Gárgola Petrificante" 25 10
--    , crearMonstruo "Hombre Lobo Hambriento" 22 9
--    , crearMonstruo "Elemental de Fuego" 35 15
--    , crearMonstruo "Nigromante Oscuro" 20 8
--    , crearMonstruo "Quimera Alada" 28 14
--    , crearMonstruo "Demonio del Abismo" 40 20
--    , crearMonstruo "Serpiente Gigante" 15 6
--    , crearMonstruo "Goblin Astuto" 10 4
--    , crearMonstruo "Cíclope Furioso" 32 18
--    , crearMonstruo "Banshee Gélida" 25 11
--    , crearMonstruo "Escorpión Venenoso" 18 7
--    , crearMonstruo "Minotauro Desorientado" 28 13
--    , crearMonstruo "Hidra de las Sombras" 45 22
--    , crearMonstruo "Orco Frenético" 20 10s
--    , crearMonstruo "Cazador de Almas" 30 16
--    , crearMonstruo "Dragón de Pesadilla" 50 25
    ]
--  imprimeMonstruo (arte (listaMonstruos !!0))
imprimeMonstruo :: String -> IO ()
imprimeMonstruo monst = putStrLn monst

esqueletoArte = "      .-.\n\
                  \     (o.o)\n\
                  \      |=|\n\
                  \     __|__\n\
                  \   //.=|=.\\\\\n\
                  \  // .=|=. \\\\\n\
                  \  \\\\ .=|=. //\n\
                  \   \\\\(_=_)//\n\
                  \    (:| |:)\n\
                  \     || ||\n\
                  \     () ()\n\
                  \     || ||\n\
                  \     || ||\n\
                  \    ==' '=="

aranaArte = ""