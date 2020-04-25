module Library where
import PdePreludat
-- Práctica Clase 04 (parapaaaaaaa parapaaaaaaa) Jurassic Park
data Dinosaurio = Dinosaurio {
    nombre :: String,
    altura :: Metros,
    peso :: Kg,
    dieta :: [Comida]
    --evolucion :: Dinosaurio -> Dinosaurio -- se pueden usar funciones en los datas
} deriving (Show, Eq)
type Kg = Number
type Metros = Number
data Comida = Planta | Carne | Insecto | Huevo deriving (Show, Eq)
-- queremos evitar esto, declarando el tipo Comida => "Plantas" "planta" "palntas" "Plantas" "pla"
trex = Dinosaurio "TRex" 6 10000 [Carne]
triceratops = Dinosaurio "Triceratops" 3 8000 [Planta]
omni = Dinosaurio "Omni" 3 8000 [Planta, Carne]
velociraptor = Dinosaurio "Velociraptoor" 2 4500 [Carne]

dinos = [trex, triceratops, omni, velociraptor]
-- Les Luthiers - "En el puesto ubicado en el hall del teatro"
--1 - Funciones Utiles
esCarnivoro = elem Carne . dieta
esHerbivoro = elem Planta . dieta
esOmnivoro dino = esCarnivoro dino && esHerbivoro dino
esOmnivoro' dino = and [esCarnivoro dino, esHerbivoro dino]
esOmnivoro'' dino = and.map ($ dino) $ [esCarnivoro, esHerbivoro]

--2 - Obtener dinosaurios herviboros de una lista
herviboros = filter esHerbivoro

--3 - Pesos de todos los dinosaurios de una lista
pesos = map peso

--4 - Saber si dada una lista hay un omnivoro
hayOmnivoro = any esOmnivoro

--5 - Conocer los dinosaurios pesados sabiendo que: pesado es mayor a 5000 kg
pesados = filter ((>5000).peso)

--6 - Conoce los Carnivoros pesados de una lista de dinosaurios
--carnivorosPesados :: [Dinosaurio] -> [Dinosaurio]
carnivorosPesados = filter esCarnivoro.pesados

--7 - Seleccionar de una lista de dinosaurios aquellos que cumplen muchas condiciones
-- por ejemplo: ser herviboro y pesado, ser omnivoro y medir mas de 2m, etc..
cumplenCondiciones :: [Dinosaurio -> Bool] -> [Dinosaurio] -> [Dinosaurio]
cumplenCondiciones condiciones dinosaurios = 
    filter (allReves condiciones.aplicarA) dinosaurios
cumplenCondiciones' condiciones dinosaurios = 
    filter (flip all condiciones. flip ($)) dinosaurios
cumplenCondiciones'' condiciones = filter (flip all condiciones. flip ($))

allReves condiciones funcion = all funcion condiciones
aplicarA cosa f = f cosa

darVueltaF funcion x1 x2 = funcion x2 x1 -- Esto es FLIP :) 
--esOmnivoro'' dino = and.map ($ dino) $ [esCarnivoro, esHerbivoro]
-- cumplenCondiciones [esHerbivoro, ((>5000).peso)] dinos
-- cumplenCondiciones [esOmnivoro, ((>2).altura)] dinos
-- cumplenCondiciones [((>4).altura), esCarnivoro, ((=='T').head.nombre)] dinos


--8 - TAREA: Hacer carnivorosPesados usando cumplenCondiciones :) 
--carnivorosPesados = 


{-
Repaso Orden superior
map     (a -> b)        [a] => [b] => length [a] == length [b]

filter  (a -> Bool)     [a] => [a'] => length [a] >= length [a']

any     (a -> Bool)     [a] => Bool
all     (a -> Bool)     [a] => Bool


-}






