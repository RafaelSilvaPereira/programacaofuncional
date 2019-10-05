{-
- Usando os predicados not,and e or prontos de Haskell, implemente os predcados (funcoes) xor (or exclusivo),
- impl (implicacao A => B é equivalente a (not A or B)) e equiv (A <=> B é definido como A => B and B => A)
- Procure usar casamento de padroes e reutilizar as funcoes.
-}
xor a b = not ((a || not b)  || ( not a ||  b )) 
impl a b = not a || b
equiv a b = a `impl` b && b `impl` a

{-
A funcao square esta implementada e eleva ao quadrado um determinado numero
-}
square x = x*x

{-
- Implemente a funcao potencia, que retorna o resultado de x elevado a y 
-}
pow x y = x^y


{-
- Implemente a funcao fatorial que calcula o fatorial de um numero 
-}
fatorial 1 = 1
fatorial 2 = 2
fatorial n = n * fatorial (n - 1)

{-
- Determina se um numero eh primo ou nao. Preocupe-se apenas em resolver o problema.
- Nao precisa usar conhecimentos mais sofisticados da teoria dos numeros. Voce pode trabalhar com listas.
-}
isPrime :: Int -> Bool
isPrime n = ((length [x | x <- [2 .. (n - 1)], mod n x == 0 ]) == 0)

{-
- Calcula um termo da sequencia de Fibonnacci. Voce pode trabalhar com listas. 
-}
fib 0 = 0
fib 1 = 1
fib n =  fib (n - 1) + fib (n - 2) 

{-
- Calcula um MDC de dois numeros usando o algoritmo de Euclides. 
-}
mdc :: Int -> Int -> Int
mdc x y 
    | y == 0 = x
    | otherwise = mdc y (x `mod` y)
{-

- Calcula um MMC de dois numeros. 
-}
mmc :: Int -> Int -> Int
mmc x 0 = x
mmc 0 y = y
mmc x y =  ((x * y) `div` (mdc x y))



{-
- Determina se dois numeros inteiros positivos sao co-primos. Dois numeros sao co-primos se 
- o mdc deles for igual a 1. Ex: coprimo 35 64 = True 
-}
coprimo x y = mdc x y == 1



{-
- Calcula a conjectura de Goldbach, 
'que diz que um numero par maior que 2 
pode ser escrito 
como a soma de dois numeros primos. Ex: 28 = 5 + 23.
-}
goldbach x 
    | x > 2 = head[(z,y) | z <- [x, x-1 .. 2], y <- [2 .. z], isPrime y, isPrime z, y + z == x] 
    | otherwise = (-1, -1)


goldbach_2 x = [ (y,z)| y <- filter isPrime [1..(x-1)], z <- filter isPrime [1..(x-1)], y + z == x ] 