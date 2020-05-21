module Library where
import PdePreludat

-- Desafio 1

zippear :: [a] -> [b] -> [(a,b)]
zippear [] [] = []
zippear (x:resto) [] = []
zippear [] (x:resto) = []
zippear (x:resto) (y:otroresto) = ((x,y): zippear resto otroresto)

-- Desafio 2

data ArbolBinario = Hoja | Rama ArbolBinario Number ArbolBinario deriving Eq

-- Implementado Show para que se pueda mostrar bonito por consola el arbol
instance Show ArbolBinario where
  show arbol = init $ unlines (showArbol arbol)
    where showArbol (Rama ramaIzquierda valor ramaDerecha)
            = show valor : (showRamas ramaIzquierda ramaDerecha)
                where
                    showRamas izquierda derecha =
                        ((pad "+- " "|  ") (showArbol derecha)) ++ ((pad "`- " "   ") (showArbol izquierda))
                    pad first rest = zipWith (++) (first : repeat rest)
          showArbol (Hoja) = []

ordenado :: ArbolBinario -> Bool
ordenado Hoja = True
ordenado (Rama Hoja _ Hoja) = True
ordenado (Rama (Rama subarbol valor1 Hoja) valor2 Hoja) = valor1 <= valor2 && ordenado (Rama subarbol valor1 Hoja)
ordenado (Rama Hoja valor1 (Rama Hoja valor2 subarbol)) = valor1 <= valor2 && ordenado (Rama Hoja valor2 subarbol)
ordenado (Rama subarbol1 valor2 subarbol2) = (aIzquierda subarbol1 subarbol2 valor2 || aDerecha subarbol1 subarbol2 valor2) && ordenado subarbol1 && ordenado subarbol2 && 
  valoresOrdenados subarbol1 subarbol2 valor2

valoresOrdenados :: ArbolBinario -> ArbolBinario -> Number -> Bool
valoresOrdenados arbol1 arbol2 valor
  |arbol1 == Hoja = valor <= valorDe arbol2
  |arbol2 == Hoja = valorDe arbol1 <= valor
  |arbol1 == Hoja && arbol2 == Hoja = True
  |otherwise = valorDe arbol1 <= valor && valor <= valorDe arbol2

aIzquierda :: ArbolBinario -> ArbolBinario -> Number -> Bool
aIzquierda Hoja arbol2 valor = False
aIzquierda arbol1 arbol2 valor = any (\a -> a >= (elMayorDeLaLista(listaDe arbol1))) (valor : listaDe arbol2)

aDerecha :: ArbolBinario -> ArbolBinario -> Number -> Bool
aDerecha arbol1 Hoja valor = False
aDerecha arbol1 arbol2 valor = any(\a -> a<= (elMenorDeLaLista (listaDe arbol2))) (valor: listaDe arbol1)

listaDe :: ArbolBinario -> [Number]
listaDe Hoja = []
listaDe (Rama Hoja valor Hoja) = [valor]
listaDe (Rama subarbol1 valor subarbol2) = (valor : listaDe subarbol1 ++ listaDe subarbol2)

elMayorDeLaLista :: [Number] -> Number
elMayorDeLaLista [] = 0
elMayorDeLaLista lista = foldl1 mayor lista

elMenorDeLaLista :: [Number] -> Number
elMenorDeLaLista [] = 0
elMenorDeLaLista lista = foldl1 menor lista

mayor :: Number -> Number -> Number
mayor numero1 numero2
  | numero1 < numero2 = numero2
  | otherwise = numero1

menor :: Number -> Number -> Number
menor numero1 numero2
  | numero1>numero2 = numero2
  | otherwise = numero1

valorDe :: ArbolBinario -> Number
valorDe Hoja = 0
valorDe (Rama _ valor _) = valor