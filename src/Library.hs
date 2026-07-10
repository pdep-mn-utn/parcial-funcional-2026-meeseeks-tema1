module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-- =====================================================================
-- PUNTO 1: I'm Mr. Meeseeks!
-- =====================================================================
    
-- MODELO
data Meeseeks = Meeseeks {
    nombre            :: String,
    color             :: String,
    horasDeExistencia :: Number,
    tareaActual       :: String,
    historial         :: [Intento]
} deriving (Show, Eq)

data Intento = Intento {
    tarea        :: String,
    horasQueTomo :: Number
} deriving (Show, Eq)

type Caja = [Meeseeks]

colorQueMerece :: Meeseeks -> String
colorQueMerece meeseeks
  | horasDeExistencia meeseeks < 1  && (even . length . nombre) meeseeks = "celeste"
  | horasDeExistencia meeseeks <= 5  = "azul"
  | horasDeExistencia meeseeks <= 24 = "violeta"
  | otherwise                        = "negro"

estaSufriendo :: Meeseeks -> Bool
estaSufriendo meeseeks = 
    color meeseeks /= colorQueMerece meeseeks || (elem "golf" . words . tareaActual) meeseeks


-- =====================================================================
-- PUNTO 2: Look at me!
-- =====================================================================

esCajaAgotadora :: Number -> Caja -> Bool
esCajaAgotadora horas = all (any ((> horas) . horasQueTomo) . historial)

nombresDeMartirizados :: Number -> Caja -> [String]
nombresDeMartirizados intentos = 
    map nombre . filter ((> intentos) . length . historial) . filter estaSufriendo

-- =====================================================================
-- PUNTO 3: Can do!
-- =====================================================================

type Interaccion = Meeseeks -> Meeseeks

-- Abstracción común: las interacciones modifican las horas de existencia
modificarHoras :: (Number -> Number) -> Interaccion
modificarHoras f meeseeks = meeseeks {
    horasDeExistencia = ( f . horasDeExistencia ) meeseeks 
}

pedirAyuda :: String -> Interaccion
pedirAyuda nuevaTarea meeseeks = modificarHoras (+ 1) $ meeseeks {
    tareaActual = nuevaTarea
}

golpear :: Interaccion
golpear meeseeks = modificarHoras (max 1 . (*2)) $ meeseeks {
    nombre = nombre meeseeks ++ " (enojado)"
}

agradecer :: Interaccion
agradecer meeseeks = modificarHoras (\_ -> 0) $ meeseeks {
    tareaActual = "ninguna",
    historial   = historial meeseeks ++ [intentoActual]
}
  where intentoActual = Intento {
            tarea        = tareaActual meeseeks,
            horasQueTomo = horasDeExistencia meeseeks
        }

frustrarse :: Interaccion
frustrarse = golpear . golpear

-- REPL:
-- agradecer . frustrarse . golpear . pedirAyuda "abrir un frasco" $ meeseeks

-- =====================================================================
-- PUNTO 4: Existence is pain!
-- =====================================================================

aplicarSecuencia :: [Interaccion] -> Meeseeks -> Meeseeks
aplicarSecuencia interacciones meeseeksInicial =
    foldl (flip ($)) meeseeksInicial interacciones

apretarBoton :: [Interaccion] -> Caja -> Caja
apretarBoton castigos = (map . aplicarSecuencia) castigos

hastaColapsar :: [Interaccion] -> Caja -> Caja
hastaColapsar castigos caja
  | all estaSufriendo caja = caja
  | otherwise              = (hastaColapsar castigos . apretarBoton castigos) caja
