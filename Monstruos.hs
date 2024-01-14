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
    [ crearMonstruo "Limo de Moco Asqueroso" 2 1 limoArte ("palo con babas", 2)
    , crearMonstruo "Araña Venenosa" 5 3 aranaArte ("Daga envenenada", 5)
    , crearMonstruo "Esqueleto Decrepito" 7 5 esqueletoArte ("Espada de hueso", 8)
    , crearMonstruo "Ogro Descomunal" 10 7 ogreArte ("Baston Descomunal", 10)
    , crearMonstruo "Hombre Lobo Hambriento" 12 8 hloboArte ("Garras de lobo", 13)
    , crearMonstruo "Serpiente Gigante" 14 9 serpienteArte ("Lanza envenenada", 16)
    , crearMonstruo "Minotauro Desorientado" 16 10 minotauroArte ("Baston Descomunal", 10)
    , crearMonstruo "Dragón de Pesadilla" 20 10 dragonArte ("Espada del elegido", 999)
    ]
--  imprimeMonstruo (arte (listaMonstruos !!0))
imprimeMonstruo :: String -> IO ()
imprimeMonstruo monst = putStrLn monst

limoArte = "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣀⠤⠄⠒⠀⠉⠉⠉⠉⠉⠁⠐⠢⢄⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀\n\
\⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡠⠔⠊⠁⠀⣠⣤⠖⠒⢖⣶⣖⠒⠒⠂⠤⣀⠀⠈⠑⠤⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀\n\
\⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⠔⠁⠀⢀⡤⠒⢿⣏⡀⢀⣀⣜⡿⠃⠀⠀⠀⠀⠀⠉⠢⢄⠀⠈⢲⡀⠀⠀⠀⠀⠀⠀⠀⠀\n\
\⠀⠀⠀⠀⠀⠀⠀⠀⡠⠊⠀⢀⡠⠊⠁⠀⠀⠀⠉⠉⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠑⣆⠀⠈⢳⡀⠀⠀⠀⠀⠀⠀\n\
\⠀⠀⠀⠀⠀⠀⢠⠊⠀⢀⠔⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠻⣇⠀⠀⢇⠀⠀⠀⠀⠀\n\
\⠀⠀⠀⠀⠀⡔⠁⢀⠔⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠘⢧⠀⠀⢧⠀⠀⠀⠀\n\
\⠀⠀⠀⢀⠎⠀⡠⢡⣴⠛⣷⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠘⣆⠀⠘⣆⠀⠀⠀\n\
\⠀⠀⢀⠎⠀⡔⢰⣿⣧⣴⣿⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⢆⠀⠈⢦⠀⠀\n\
\⠀⠀⡎⠀⡜⢠⣿⣿⣿⣿⠏⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⣿⣷⣄⠈⢆⠀⢇⠀⠀\n\
\⠀⡸⠀⣸⠀⠘⠿⠿⠟⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣇⣸⣿⡇⠘⡄⠈⡄⠀\n\
\⢀⠃⢠⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⣶⣷⣄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⡇⠀⢱⠀⢱⠀\n\
\⠸⠀⡜⠀⠀⠀⠀⠀⠀⠀⠀⠀⣼⣿⣿⣿⣿⣿⣶⣤⣄⣀⣀⣀⣠⣤⣤⣴⣶⣧⡀⠀⠀   ⠙⠛⠛⠋⠀⠀⡇⠀⡇⠀⠀\n\
\⡇⠀⠇⠀⠀⠀⠀⠀⠀⠀⠀⣼⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡇⢱⠀⠀\n\
\⡇⢸⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡇⠘⠀⠀\n\
\⠇⠸⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⣿⡿⠟⠛⠉⠉⠉⠛⠛⢿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡇⠀⠇⠀\n\
\⢸⠀⡇⠀⠀⠀⠀⠀⠀⠀⠀⠱⣻⣿⣿⣿⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⣿⣿⣿⢧⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⠀⢰\n\
\⠀⢇⠹⡀⠀⠀⠀⠀⠀⠀⠀⠀⠙⢝⢿⣿⣧⣀⠀⠀⠀⠀⠀⠀⠀⣀⣴⣿⡿⡣⠋⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡆⠀⣹\n\
\⠀⠈⢢⠱⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠑⠬⣛⠻⠿⢷⣶⣶⣶⣾⡿⠟⢛⡥⠊⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⠄⠀⡎⠀\n\
\⠀⠀⠀⠑⢌⠢⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⠁⠒⠒⠒⠐⠒⠒⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⠔⠋⢠⠞⠁⠀\n\
\⠀⠀⠀⠀⠀⠑⠢⣑⠤⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⡠⠔⠁⣀⡴⠊⠀⠀⠀\n\
\⠀⠀⠀⠀⠀⠀⠀⠀⠉⠒⠬⢒⠤⢄⣀⣀⣀⣀⣀⣀⣀⣀⠠⠤⠤⠤⠀⠤⠤⠤⠔⠂⠉⢀⡴⠋⠀⠀⠀⠀⠀⠀⠀⠀⠀\n\
\⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⠑⠒⠒⠦⠤⠤⠤⠤⠤⠤⠤⠤⠤⠤⠤⠤⠤⠤⠚⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"

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

aranaArte = "  / _ \\\n\
\\\_\\(_)/_/\n\
\ _//o\\\\_ \n\
\  /   \\"


ogreArte = "                           __,='`````'=/__\n\
\                          '//  (o) \\(o) \\ `'         _,-,\n\
\                          //|     ,_)   (`\\      ,-'`_,-\\\\\n\
\                        ,-~~~\\  `'==='  /-,      \\==```` \\__\n\
\                       /        `----'     `\\     \\       \\/\n\
\                    ,-`                  ,   \\  ,.-\\       \\\n\
\                   /      ,               \\,-`\\`_,-`\\_,..--'\\\n\
\                  ,`    ,/,              ,>,   )     \\--`````\\\n\
\                  (      `\\`---'`  `-,-'`_,<   \\      \\_,.--'`\n\
\                   `.      `--. _,-'`_,-`  |    \\\n\
\                    [`-.___   <`_,-'`------(    /\n\
\                    (`` _,-\\   \\ --`````````|--`\n\
\                     >-`_,-`\\,-` ,          |\n\
\                   <`_,'     ,  /\\          /\n\
\                    `  \\/\\,-/ `/  \\/`\\_/\\V\\_/\n\
\                       (  ._. )    ( .__. )\n\
\                       |      |    |      |\n\
\                        \\,---_|    |_---./\n\
\                        ooOO(_)    (_)OOoo"

minotauroArte = "                            .      .\n\
\                            |\\____/|\n\
\                           (\\|----|/)\n\
\                            \\ 0  0 /\n\
\                             |    |\n\
\                          ___/\\../\\____\n\
\                         /     --       \\\n\
\                        /  \\         /   \\\n\
\                       |    \\___/___/(   |\n\
\                       \\   /|  }{   | \\  )\n\
\                        \\  ||__}{__|  |  |\n\
\                         \\  |;;;;;;;\\  \\ / \\_______\n\
\                          \\ /;;;;;;;;| [,,[|======'\n\
\                            |;;;;;;/ |     /\n\
\                            ||;;|\\   |\n\
\                            ||;;/|   /\n\
\                            \\_|:||__|\n\
\                             \\ ;||  /\n\
\                             |= || =|\n\
\                             |= /\\ =|\n\
\                             /_/  \\_\\"

serpienteArte = "\n\
\    --..,_                     _,.--.\n\
\       `'.'.                .'`__ o  `;__.      \n\
\          '.'.            .'.'`  '---'`  `\n\
\            '.`'--....--'`.'\n\
\              `'--....--'`\n\
\"

hloboArte = "                        ,     ,\n\
\                        |\\---/|\n\
\                       /  , , |\n\
\                  __.-'|  / \\ /\n\
\         __ ___.-'        ._O|\n\
\      .-'  '        :      _/\n\
\     / ,    .        .     |\n\
\    :  ;    :        :   _/\n\
\    |  |   .'     __:   /\n\
\    |  :   /'----'| \\  |\n\
\    \\  |\\  |      | /| |\n\
\     '.'| /       || \\ |\n\
\     | /|.'       '.l \\_\n\
\     || ||             '-'\n\
\     '-''-'"

dragonArte = "                \\||/\n\
\                |  @___oo\n\
\      /\\  /\\   / (__,,,,|\n\
\     ) /^\\) ^\\/ _)\n\
\     )   /^\\/   _)\n\
\     )   _ /  / _)\n\
\ /\\  )/\\/ ||  | )_)\n\
\<  >      |(,,) )__)\n\
\ ||      /    \\)___)\\\n\
\ | \\____(      )___) )___\n\
\  \\______(_______;;; __;;;"
