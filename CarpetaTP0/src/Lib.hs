import Data.List

-- DEFINIENDO LOS DATA-- (En notación Record que además te crea las funciones)
data Sustancia 
    = Elemento{
        nombre :: String , 
        simboloQuimico :: String , 
        numeroAtomico :: Int,
        grupo :: Grupo 
    } 
    | Compuesto{
       nombre :: String ,
       componentes :: [Componente] ,
       grupo :: Grupo
    } 
    deriving (Show)

data Componente 
    = Componente {
        sustancia :: Sustancia,
        cantidadMoleculas :: Int
    }
    deriving (Show)

--Acota los valores de Grupo
data Grupo = Metal | NoMetal | Halogeno | GasNoble deriving(Show, Eq) 

--Para punto 2 (ya que permite agregar a futuro más criterios)
data Criterio = Electricidad | Calor deriving(Show, Eq) 



-------------------------------------------RESOLUCIÓN------------------------------------------------------

---------------------
----PUNTO 1 a y b----
---------------------
hidrogeno:: Sustancia 
hidrogeno = Elemento "Hidrogeno" "H" 1 NoMetal

oxigeno:: Sustancia
oxigeno = Elemento "Oxigeno" "O" 8 NoMetal

componente :: Sustancia -> Int -> Componente
componente unaSustancia cantidadMoleculas = Componente unaSustancia cantidadMoleculas

cloro :: Sustancia
cloro = Elemento "Cloro" "Cl" 17 Halogeno

sodio :: Sustancia
sodio = Elemento "Sodio" "Na" 11 NoMetal

agua :: Sustancia
agua = Compuesto "Agua" [ componente hidrogeno 2 , componente oxigeno 1 ] NoMetal

---------------
----PUNTO 2----
---------------

--el criterio esta definido en la parte de definicion de Data (linea 31)
conduceBien :: Sustancia -> Criterio -> Bool
conduceBien sustanciaConduce criterio = (grupo sustanciaConduce == Metal) 
    || (grupo sustanciaConduce == GasNoble && criterio == Electricidad) 
    || (grupo sustanciaConduce == Halogeno && criterio == Calor)

---------------
----PUNTO 3----
---------------
esVocal :: Char -> Bool
esVocal unaLetra = elem unaLetra "aAeEiIoOuU"

esConsonante :: Char -> Bool
esConsonante  =  not . esVocal

--Le pasas una palabra y te dice si la ultima letra es consonante
ultimaLetraEsConsonante:: String -> Bool
ultimaLetraEsConsonante = esConsonante . last

-- Le pasas una palabra y te dice si la anteultima letra es consonante
anteUltimaLetraEsConsonante:: String -> Bool
anteUltimaLetraEsConsonante nombre = ultimaLetraEsConsonante . init $ nombre

-- Le pasas una PALABRA y la devuelve sin la ultima letra
eliminarUltimaLetra:: String -> String
eliminarUltimaLetra = init 

-- Le pasas una sustancia y te devuelve el nombre sin las dos ultimas letras
eliminarDosUltimasLetras:: String -> String
eliminarDosUltimasLetras = init . eliminarUltimaLetra

-- (listo) 
nombreDeUnion :: String -> String
nombreDeUnion nombreSustancia 
    | ultimaLetraEsConsonante nombreSustancia          = nombreSustancia ++ "uro"
    | anteUltimaLetraEsConsonante nombreSustancia      = eliminarUltimaLetra nombreSustancia ++ "uro"
    | otherwise                                        = eliminarDosUltimasLetras nombreSustancia ++ "uro"

---------------
----PUNTO 4----
---------------

combinar :: String -> String -> String
combinar nombreSustancia1 nombreSustancia2 = (nombreDeUnion nombreSustancia1) ++ " de " ++ (nombreSustancia2)

--------------
---PUNTO 5----
--------------

mezclarComponentes :: [Componente] -> Sustancia
mezclarComponentes unosComponentes = Compuesto ( mezclarNombresDeComponentes unosComponentes ) unosComponentes NoMetal

--hecho con Fold
mezclarNombresDeComponentes :: [Componente] -> String 
mezclarNombresDeComponentes unosComponentes = foldl1 combinar (map (nombre . sustancia) $ unosComponentes)

---------------
----PUNTO 6----
---------------

formula :: Sustancia -> String
formula (Elemento  _  simboloQuimico _  _ ) = simboloQuimico
formula (Compuesto _  componentes    _    ) = "(" ++ formulaDeComponentes componentes ++ ")"

formulaDeComponentes :: [Componente] -> String
formulaDeComponentes unosComponentes = concatMap representacionDeComponente unosComponentes 

representacionDeComponente :: Componente -> String
representacionDeComponente unComponente 
                    | cantidadMoleculas unComponente == 1 =  formula . sustancia $ unComponente
                    | otherwise                           = (formula . sustancia $ unComponente) ++ (show (cantidadMoleculas unComponente))
