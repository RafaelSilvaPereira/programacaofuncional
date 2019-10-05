{-
- Dada uma tupla, divide o primeiro pelo segundo usando pattern matching.
- Ela deve ser indefinida quando o denominador for zero.
-}
divTuple (_, 0) = undefined
divTuple (x, y) = x / y

{-
 - Calcula o somatorio entre dois numeros a e b (a < b). Procure usar alguma funcao pronta sobre listas. 
 - Ex: somatorio 0 1 = 1
 -     somatorio 1 3 = 6
-}
somatorio a b = sum [a .. b]

{-
 - Calcula o somatorio (recursivo) entre dois numeros a e b (a < b).
 - Ex: somatorio 0 1 = 1
 -     somatorio 1 3 = 6
-}
somatorioRec a b = recursiveSum [a .. b] 
recursiveSum [] = 0
recursiveSum (h:t) = h + recursiveSum t

somatorioRec2 a b | (a + 1 == b) = a + b | otherwise = a + somatorio (a + 1) b



-- Defina a funcao que eleva um membro ao quadrado
square x = x * x 

-- Soma os quadrados de dois numeros.
sumSquares x y = square x + square y

-- Defina uma funcao de alta ordem que aceita uma função (Int -> Int) e aplica a funcao a dois numeros
higherOrderSum:: (Int -> Int -> Int) -> Int -> Int -> Int
higherOrderSum f a b = (f a b)

-- Defina a soma dos auqdrados em termos de higherOrderSum
hoSumSquares =  higherOrderSum sumSquares 

--Implemente a funcao mapFilter que primeiro aplica o map de uma funcao f a uma lista e depois aplica a funcao filter
-- a lista resultante. Procure usar a composicao de funcoes
mapFilter f p xs = [y | y <- [f x | x <- xs], p y]
