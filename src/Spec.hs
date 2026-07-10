module Spec where
import PdePreludat
import Library
import Test.Hspec

meeseeksFeliz :: Meeseeks
meeseeksFeliz = Meeseeks {
    nombre            = "Mr. Meeseeks",
    color             = "celeste",
    horasDeExistencia = 0.5,
    tareaActual       = "destapar una cerveza",
    historial         = []
}

meeseeksRecienSalido :: Meeseeks
meeseeksRecienSalido = Meeseeks {
    nombre            = "Mr. Meeseeks New",
    color             = "celeste",
    horasDeExistencia = 0.5,
    tareaActual       = "abrir un frasco",
    historial         = []
}

jerryGolfista :: Meeseeks
jerryGolfista = Meeseeks {
    nombre            = "Mr. Meeseeks Golfer",
    color             = "azul",
    horasDeExistencia = 3,
    tareaActual       = "sacar dos golpes al golf",
    historial         = []
}

elDelArmario :: Meeseeks
elDelArmario = Meeseeks {
    nombre            = "Mr. Meeseeks Locker",
    color             = "negro",
    horasDeExistencia = 27,
    tareaActual       = "ayudar a Beth",
    historial         = [
        Intento { tarea = "abrir la puerta",       horasQueTomo = 12 },
        Intento { tarea = "encontrar las llaves",  horasQueTomo = 8  }
    ]
}

meeseeksFelizNuevo :: Meeseeks
meeseeksFelizNuevo = meeseeksFeliz { horasDeExistencia = 0 }

meeseeksNuevo :: Meeseeks
meeseeksNuevo = meeseeksRecienSalido { horasDeExistencia = 0 }

-- Menos de 1 hora pero nombre de longitud impar: no merece celeste
meeseeksNombreImpar :: Meeseeks
meeseeksNombreImpar = meeseeksRecienSalido { nombre = "Mr. Meeseeks Jr" }

meeseeksUnaHora :: Meeseeks
meeseeksUnaHora = meeseeksRecienSalido { horasDeExistencia = 1 }

meeseeksCincoHoras :: Meeseeks
meeseeksCincoHoras = meeseeksRecienSalido { horasDeExistencia = 5 }

meeseeksVeinticuatroHoras :: Meeseeks
meeseeksVeinticuatroHoras = meeseeksRecienSalido { horasDeExistencia = 24 }

meeseeksDesajustado :: Meeseeks
meeseeksDesajustado = meeseeksRecienSalido { horasDeExistencia = 10 }

meeseeksTorturadoConGolf :: Meeseeks
meeseeksTorturadoConGolf = Meeseeks {
    nombre            = "Mr. Meeseeks Tortured",
    color             = "violeta",
    horasDeExistencia = 10,
    tareaActual       = "jugar golf",
    historial         = [
        Intento { tarea = "algo",      horasQueTomo = 5 },
        Intento { tarea = "otra cosa", horasQueTomo = 3 }
    ]
}

cajaDeLaFamilia :: Caja
cajaDeLaFamilia = [meeseeksRecienSalido, jerryGolfista, elDelArmario]


correrTests :: IO ()
correrTests = hspec $ do
  describe "Punto 1b - colorQueMerece" $ do
    it "celeste si lleva menos de 1 hora y el nombre tiene longitud par" $
      colorQueMerece meeseeksRecienSalido `shouldBe` "celeste"
    it "celeste tambien con 0 horas y nombre par" $
      colorQueMerece meeseeksFelizNuevo `shouldBe` "celeste"
    it "NO es celeste si lleva menos de 1 hora pero el nombre tiene longitud impar (cae en azul)" $
      colorQueMerece meeseeksNombreImpar `shouldBe` "azul"
    it "azul en el borde inferior de 1 hora" $
      colorQueMerece meeseeksUnaHora `shouldBe` "azul"
    it "azul para exactamente 3 horas" $
      colorQueMerece jerryGolfista `shouldBe` "azul"
    it "azul en el borde superior de 5 horas" $
      colorQueMerece meeseeksCincoHoras `shouldBe` "azul"
    it "violeta para 10 horas" $
      colorQueMerece meeseeksTorturadoConGolf `shouldBe` "violeta"
    it "violeta en el borde de 24 horas" $
      colorQueMerece meeseeksVeinticuatroHoras `shouldBe` "violeta"
    it "negro para mas de 24 horas" $
      colorQueMerece elDelArmario `shouldBe` "negro"

  describe "Punto 1c - estaSufriendo" $ do
    it "sufre si el color actual no coincide con el que merece" $
      meeseeksDesajustado `shouldSatisfy` estaSufriendo
    it "sufre si la tarea contiene la palabra golf" $
      jerryGolfista `shouldSatisfy` estaSufriendo
    it "no sufre cuando color y horas coinciden y no hay golf" $
      meeseeksFeliz `shouldNotSatisfy` estaSufriendo

  describe "Punto 2a - esCajaAgotadora" $ do
    it "es agotadora si todos tienen algún intento que superó el umbral" $
      [elDelArmario] `shouldSatisfy` esCajaAgotadora 10
    it "no es agotadora si algún Meeseeks no tiene historial" $
      cajaDeLaFamilia `shouldNotSatisfy` esCajaAgotadora 5
    it "caja vacía es vacuamente agotadora" $
      ([] :: Caja) `shouldSatisfy` esCajaAgotadora 5

  describe "Punto 2b - nombresDeMartirizados" $ do
    it "no devuelve nadie si los sufridos no superan el umbral de intentos" $
      nombresDeMartirizados 1 cajaDeLaFamilia `shouldMatchList` []
    it "devuelve el nombre del sufrido que supera el umbral de intentos" $
      nombresDeMartirizados 1 [meeseeksTorturadoConGolf] `shouldMatchList` ["Mr. Meeseeks Tortured"]

  describe "Punto 3 - pedirAyuda" $ do
    it "cambia la tarea actual" $
      (tareaActual . pedirAyuda "limpiar la cocina") meeseeksRecienSalido `shouldBe` "limpiar la cocina"
    it "suma 1 hora de existencia" $
      (horasDeExistencia . pedirAyuda "limpiar la cocina") meeseeksRecienSalido `shouldBe` 1.5

  describe "Punto 3 - golpear" $ do
    it "agrega el sufijo enojado al nombre" $
      (nombre . golpear) meeseeksRecienSalido `shouldBe` "Mr. Meeseeks New (enojado)"
    it "duplica las horas de existencia" $
      (horasDeExistencia . golpear) jerryGolfista `shouldBe` 6
    it "deja 1 hora si el Meeseeks recién salió de la caja con 0 horas" $
      (horasDeExistencia . golpear) meeseeksNuevo `shouldBe` 1

  describe "Punto 3 - agradecer" $ do
    it "resetea la tarea a ninguna" $
      (tareaActual . agradecer) meeseeksRecienSalido `shouldBe` "ninguna"
    it "resetea las horas a 0" $
      (horasDeExistencia . agradecer) meeseeksRecienSalido `shouldBe` 0
    it "agrega un intento al historial" $
      (length . historial . agradecer) meeseeksRecienSalido `shouldBe` 1
    it "guarda la tarea previa en el intento" $
      (tarea . head . historial . agradecer) meeseeksRecienSalido `shouldBe` "abrir un frasco"
    it "guarda las horas previas en el intento" $
      (horasQueTomo . head . historial . agradecer) meeseeksRecienSalido `shouldBe` 0.5

  describe "Punto 3 - frustrarse" $ do
    it "produce el mismo resultado que golpear dos veces" $
      (horasDeExistencia . frustrarse) jerryGolfista
        `shouldBe` (horasDeExistencia . golpear . golpear) jerryGolfista
    it "añade el sufijo enojado dos veces" $
      (nombre . frustrarse) jerryGolfista
        `shouldBe` "Mr. Meeseeks Golfer (enojado) (enojado)"

  describe "Punto 4a - aplicarSecuencia" $ do
    it "aplica las interacciones en orden (primera de la lista primero)" $
      (tareaActual . aplicarSecuencia [pedirAyuda "tarea A", pedirAyuda "tarea B"]) meeseeksFeliz
        `shouldBe` "tarea B"
    it "lista vacia de interacciones deja el Meeseeks igual" $
      aplicarSecuencia [] meeseeksFeliz `shouldBe` meeseeksFeliz
    it "equivale a aplicar la composicion de las interacciones" $
      aplicarSecuencia [pedirAyuda "golf", golpear] meeseeksFeliz
        `shouldBe` (golpear . pedirAyuda "golf") meeseeksFeliz

  describe "Punto 4a - hastaColapsar (solo casos que convergen)" $ do
    it "si todos ya sufren, devuelve la caja sin cambios" $
      hastaColapsar [golpear] [jerryGolfista, meeseeksTorturadoConGolf]
        `shouldBe` [jerryGolfista, meeseeksTorturadoConGolf]
    it "al terminar, todos los Meeseeks estan sufriendo" $
      hastaColapsar [pedirAyuda "jugar al golf"] cajaDeLaFamilia
        `shouldSatisfy` all estaSufriendo
    it "converge en un solo ciclo si el castigo hace sufrir a todos de una" $
      hastaColapsar [pedirAyuda "jugar al golf"] cajaDeLaFamilia
        `shouldBe` map (pedirAyuda "jugar al golf") cajaDeLaFamilia
    it "no modifica una caja vacia" $
      hastaColapsar [golpear] ([] :: Caja) `shouldBe` []
