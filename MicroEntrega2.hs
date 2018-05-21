--     TP Funcional 2018 - Microprocesador - Segunda entrega
--     Paradigmas de Programación
--     Realizado por: 
--					del Burgo, María Abril
--					Gagliardi, Diego
--					Laye, Matías
--					Rodríguez Cary, Hernán

module MicroEntrega2 where
import Text.Show.Functions

--PUNTO 3.1
type Instruccion = Microprocesador -> Microprocesador
type Valor = Int
type Posicion = Int

data Microprocesador = Microprocesador { memoriaDeDatos :: [Int], acumuladorA :: Int, acumuladorB :: Int, programCounter :: Int, etiqueta :: String, memoriaDeProgramas :: [Instruccion] } deriving(Show)

xt8088 = Microprocesador { memoriaDeDatos = [], acumuladorA = 0, acumuladorB = 0, programCounter = 0, etiqueta = "", memoriaDeProgramas = [] }

--ENTREGA 1
--PUNTO 3.2.1
nop :: Instruccion
nop unMicrocontrolador = unMicrocontrolador {programCounter = programCounter unMicrocontrolador + 1} 

--PUNTO 3.2.2
avanzarTresVecespc :: Instruccion
avanzarTresVecespc = nop.nop.nop
listaAvanzarTresVecespc = [nop, nop, nop]

--PUNTO 3.3.1
lodv :: Valor -> Instruccion
lodv val = nop.cargarValorEnAcumuladorA val

cargarValorEnAcumuladorA :: Valor -> Instruccion
cargarValorEnAcumuladorA val unMicrocontrolador = unMicrocontrolador {acumuladorA = val}

swap :: Instruccion
swap = nop.intercambiarValores

intercambiarValores :: Instruccion
intercambiarValores unMicrocontrolador = unMicrocontrolador {acumuladorA = acumuladorB unMicrocontrolador, acumuladorB = acumuladorA unMicrocontrolador}

add :: Instruccion
add = nop.sumarValores

sumarValores :: Instruccion
sumarValores unMicrocontrolador = unMicrocontrolador {acumuladorA = acumuladorB unMicrocontrolador + acumuladorA unMicrocontrolador, acumuladorB = 0}

--PUNTO 3.4.1
divide :: Instruccion
divide = nop.dividirAcumuladores

dividirAcumuladores :: Instruccion
dividirAcumuladores unMicrocontrolador
    | acumuladorB unMicrocontrolador == 0 = unMicrocontrolador {etiqueta = "ERROR DIVISION BY ZERO"} 
    | otherwise =  unMicrocontrolador { acumuladorA = quot (acumuladorA unMicrocontrolador) (acumuladorB unMicrocontrolador), acumuladorB = 0} 

str :: Posicion -> Valor -> Instruccion
str addr val = nop.guardarValorEnMemoria addr val

guardarValorEnMemoria :: Posicion -> Valor -> Instruccion
guardarValorEnMemoria addr val unMicrocontrolador = unMicrocontrolador { memoriaDeDatos = ( take addr (memoriaDeDatos unMicrocontrolador) ) ++ [val] ++ ( drop addr (memoriaDeDatos unMicrocontrolador) )}

lod :: Posicion -> Instruccion
lod addr = nop.cargaraDesdeMemoria addr

cargaraDesdeMemoria :: Posicion -> Instruccion
cargaraDesdeMemoria addr unMicrocontrolador = unMicrocontrolador { acumuladorA = (memoriaDeDatos unMicrocontrolador) !! (addr-1) }

-- CASOS DE PRUEBA 
fp20 = Microprocesador {memoriaDeDatos=[], acumuladorA = 7, acumuladorB = 24, programCounter = 0, etiqueta = "", memoriaDeProgramas=[]}

at8086 = Microprocesador {memoriaDeDatos=[1..20], acumuladorA = 0, acumuladorB = 0, programCounter = 0, etiqueta = "", memoriaDeProgramas=[]}

dividir12por4 :: Instruccion
dividir12por4 = divide . lod 1 . swap . lod 2 . str 2 4 . str 1 12
listaDividir12Por4 = [divide,(lod 1),swap,(lod 2),(str 2 4),(str 1 12)]

testxt8088 = Microprocesador { memoriaDeDatos= replicate 1024 0, acumuladorA = 0, acumuladorB = 0, programCounter = 0, etiqueta = "", memoriaDeProgramas = []}



--ENTREGA 2
-- PUNTO 1 
cargarPrograma :: [Instruccion] -> Microprocesador -> Microprocesador
cargarPrograma listaDeInstrucciones unMicrocontrolador = unMicrocontrolador {memoriaDeProgramas = memoriaDeProgramas unMicrocontrolador ++ listaDeInstrucciones}

listaFuncionesDeSumar10Mas22 = [(lodv 10), swap, (lodv 22), add]

sumar10mas22 :: Instruccion
sumar10mas22 = ejecutarPrograma.cargarPrograma listaFuncionesDeSumar10Mas22

listaFuncionesDividir2por0 = [(str 1 2), (str 2 0), (lod 2), swap, (lod 1), divide]

dividir2por0 :: Instruccion
dividir2por0 = ejecutarPrograma.cargarPrograma listaFuncionesDividir2por0

-- PUNTO 2
ejecutarPrograma :: Instruccion
ejecutarPrograma unMicrocontrolador = ejecutarInstrucciones (memoriaDeProgramas unMicrocontrolador) unMicrocontrolador

ejecutarInstrucciones :: [Instruccion] -> Instruccion
ejecutarInstrucciones [] unMicrocontrolador= unMicrocontrolador
ejecutarInstrucciones (x:xs) (Microprocesador  memoriaDeDatos acumuladorA acumuladorB programCounter [] memoriaDeProgramas) = (ejecutarInstrucciones xs.x) (Microprocesador  memoriaDeDatos acumuladorA acumuladorB programCounter [] memoriaDeProgramas)
ejecutarInstrucciones _ unMicrocontrolador = unMicrocontrolador

-- PUNTO 3 / ejecuta una serie de instrucciones en caso de que el acumulador A no tenga el valor 0.
ifnz :: [Instruccion] -> Instruccion
ifnz listaDeInstrucciones (Microprocesador memoriaDeDatos 0 acumuladorB programCounter etiqueta memoriaDeProgramas) = (Microprocesador memoriaDeDatos 0 acumuladorB programCounter etiqueta memoriaDeProgramas)
ifnz listaDeInstrucciones unMicrocontrolador = ejecutarInstrucciones listaDeInstrucciones unMicrocontrolador

-- PUNTO 4

aplicar :: b->(b->c)->c
aplicar valor funcion = funcion valor
depurar :: Microprocesador->[Instruccion] -> [Instruccion]
depurar unMicrocontrolador  = filter (not.estanDatosEnCero.(aplicar unMicrocontrolador)) 
estanDatosEnCero :: Microprocesador -> Bool
estanDatosEnCero (Microprocesador memoriaDeDatos acumuladorA acumuladorB programCounter etiqueta memoriaDeProgramas) = acumuladorA == 0 && acumuladorB == 0 && all (==0) memoriaDeDatos


-- PUNTO 5
memoriaEstaOrdenada :: Microprocesador -> Bool
memoriaEstaOrdenada = listaOrdenada.memoriaDeDatos

listaOrdenada :: Ord a => [a] -> Bool
listaOrdenada [] = True
listaOrdenada [_] = True
listaOrdenada (x:y:xs) = (x <= y) && listaOrdenada (y:xs) 

-- PUNTO 6
-- item 1
superMicroprocesador = Microprocesador { memoriaDeDatos = [0..], acumuladorA = 0, acumuladorB = 0, programCounter = 0, etiqueta = "", memoriaDeProgramas = [] }
-- item 2. la memoria de datos muestra en consola su contenido hasta que se llene la memoria
-- item 3. no muestra True o False ya que esta analizando una lista infinita
-- item 4. Al utilizar una lista de números infinita, el programa no podrá concluirse ya que nunca llega a terminar de procesar dicha lista

-- CASOS DE PRUEBA
microDesorden = Microprocesador { memoriaDeDatos = [2,5,1,0,6,9], acumuladorA = 0, acumuladorB = 0, programCounter = 0, etiqueta = "", memoriaDeProgramas = [] }
