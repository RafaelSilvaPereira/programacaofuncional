--Exemplos de expressoes lambda
square = \x -> x*x

--Implemente as funções anteriormente escritas usando expressões lambda
--consulte suas implementacoes anteriores para a documentacao dessas funcoes
pow  = \x y -> x ^ y

fatorial = \x -> 
    let y = if x == 1 then 1 else x * fatorial (x -1)
    in y

isPrime  = \n -> ((length [x | x <- [2 .. (n - 1)], mod n x == 0 ]) == 0) 
fib  = \x -> 
    let y = if x <= 1 then x else fib (x - 1) + fib (x - 2) in y

mdc  = \x y -> if y == 0 then x else mdc y (x `mod` y)
mmc  = \x y -> (x*y) `div` (mdc x y)
coprimo  = \x y -> mdc x y == 1
goldbach x = undefined

--Implemente as funções sobre listas escritas previsamente usando expressões lambda
--consulte suas implementacoes anteriores para a documentacao dessas funcoes
meuLast  = \(h:t) -> if null t then h else meuLast t
penultimo  = \(h:h2:t) ->  if null t then h else penultimo t
elementAt  = \i (h:t) -> if i == 0 then h else elementAt (i-1) t
meuLength  = \(h:t) -> if null t then 1 else 1 + meuLength t
meuReverso :: [a] -> [a]
meuReverso  = \(h:t) -> if not (null t) then meuReverso t ++ [h] else [h]
isPalindrome  = \xs -> xs == meuReverso xs
compress xs = undefined
compact xs = undefined
encode xs = undefined
split xs i = undefined
slice xs imin imax = undefined
insertAt el pos xs = undefined
sort xs = undefined
mySum xs = undefined
maxList xs = undefined
buildPalindrome xs = undefined
mean  = \xs -> (sum xs) `div` length xs
myAppend  = \xs ys -> foldr (:) xs ys 