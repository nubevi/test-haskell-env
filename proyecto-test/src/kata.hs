module Kata where
import PdePreludat

type Nombre = String
type Letra = Char
type Limite = Number

vocales = ['a', 'e', 'i', 'o', 'u']

esVocal :: Letra -> Bool
esVocal = flip elem vocales

esParticular :: Nombre -> Bool
esParticular nombre = comienzaConVocal nombre || gradoDeParticularidadMayorA gradoParticularidadLimite nombre

comienzaConVocal :: Nombre -> Bool
comienzaConVocal = esVocal.head

gradoParticularidadLimite = 7

gradoDeParticularidadMayorA :: Limite -> Nombre -> Bool
gradoDeParticularidadMayorA valLimite = (>valLimite).length

esMagico :: Nombre -> Bool
esMagico nombre = esParticular nombre || terminaEnConsonante nombre

terminaEnConsonante :: Nombre -> Bool
terminaEnConsonante = not . comienzaConVocal