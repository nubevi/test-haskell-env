import Data.List
import Text.Show.Functions

type Fuerza = Float
type Punteria = Float
type Experiencia = Float
type NivelProfesionalismo = Float

type Habilidad = (Fuerza, Punteria, Experiencia)
experiencia (_, _ , e) = e
punteria (_, p, _) = p
fuerza (f, _, _) = f

type ObjetoMagico = Heroe -> Heroe

data Heroe = Heroe {
    nombre :: String,
    nivelDeHambre :: Int,
    nombrePaisNatal :: String,
    esMiedoso :: Bool,
    habilidadDeCombate :: Habilidad,
    objetoMagico :: ObjetoMagico,
    poder :: Float
} deriving (Show)

varaMagica :: ObjetoMagico
varaMagica unHeroe = unHeroe {poder = 10 * experiencia (habilidadDeCombate unHeroe)}

arco :: NivelProfesionalismo -> ObjetoMagico
arco profesionalismo unHeroe = unHeroe {poder = profesionalismo * 
                                punteria (habilidadDeCombate unHeroe) * 
                                fuerza (habilidadDeCombate unHeroe)}

anilloUnico :: ObjetoMagico
anilloUnico unHeroe = unHeroe {poder = 2^10}

gandalf = Heroe {
    nivelDeHambre = 5,
    nombrePaisNatal = "Valinor",
    esMiedoso = False,
    objetoMagico = varaMagica,
    poder = 0,
    habilidadDeCombate = (520, 85, 2500),
    nombre = "Gandalf"
}

legolas = Heroe {
    nivelDeHambre = 15,
    nombrePaisNatal = "Bosque Negro",
    esMiedoso = False,
    objetoMagico = (arco 10),
    poder = 0,
    habilidadDeCombate = (400, 99.9, 1500),
    nombre = "Legolas"
}

limiteHambre = 1

tieneHambre :: Heroe -> Bool
tieneHambre = (mayorA limiteHambre) . nivelDeHambre

mayorA :: Int -> Int -> Bool
mayorA limite valorAComparar = valorAComparar > limite

cambiarObjetoMagico :: ObjetoMagico -> Heroe -> Heroe
cambiarObjetoMagico nuevoObjeto unHeroe = unHeroe {objetoMagico = nuevoObjeto}

desayunar :: Heroe -> Heroe
desayunar = disminuirHambreEn 10

disminuirHambreEn :: Int -> Heroe -> Heroe
disminuirHambreEn cant unHeroe = unHeroe {nivelDeHambre = max 0 (subtract 10 (nivelDeHambre unHeroe))}

distanciaMaxima = 500 
largoDelPaisOrigen = genericLength.nombrePaisNatal

distanciaQuePuedeCaminar :: Heroe -> Int
distanciaQuePuedeCaminar unHeroe = min distanciaMaxima (largoDelPaisOrigen unHeroe - cansancio (unHeroe))

cansancio :: Heroe -> Int
cansancio unHeroe | esMiedoso unHeroe = 3 * nivelDeHambre unHeroe
                  | otherwise = 2 * nivelDeHambre unHeroe

nivelCombateTotal :: Heroe -> Float
nivelCombateTotal unHeroe = sumaDeHabilidades unHeroe + poderObjeto unHeroe

poderObjeto :: Heroe -> Float
poderObjeto  = poder . usarObjeto

usarObjeto unHeroe = (objetoMagico unHeroe) unHeroe

sumaDeHabilidades :: Heroe -> Float
sumaDeHabilidades unHeroe = fuerza (habilidadDeCombate unHeroe) + experiencia (habilidadDeCombate unHeroe) + punteria (habilidadDeCombate unHeroe)
