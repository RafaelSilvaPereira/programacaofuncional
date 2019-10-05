module MultisetList ()
where

{- 
- Um multi-conjunto (ou bag) é uma estrutura que representa uma coleção de objetos que 
- permite duplicadas. Entretanto, as duplicatas são armazenadas como a quantidade de 
- ocorréncias do mesmo elemento no multi-conjunto. Exemplo, a coleção {a,b,c,c,c,b} poderia 
- ser representada como sendo {(a,1), (b,2), (c,3)}. A ideia de multi-conjunto pode ser 
- implementada de diversas formas. Uma delas é usando a implementacao de Data.List, onde 
- cada elemento da lista consiste do dado em si e sua quantidade (um par). 
- Eh recomendavel que voce consulte a documentacao de Data.List
-}
import Data.List as List

{-
- Insere um elemento na estrutura. Caso o elemento ja existe, sua quantidade na estrutura sera incrementada.
-}



insert' elem [] = [(elem, 1)]
insert' elem (h:t)
    | (fst h) == elem = ((fst h), ((snd h) + 1)) : t
    | otherwise = h : (insert' elem t)

{-
- Remove um elemento da estrutura, levando em consideracao a manipulacao de sua quantidade na estrutura. 
- Caso a quantidade atinja 0 (ou menos), o elemento deve realmente ser removido da estrutura
-}
remove' _ [] = []
remove' elem (h:t) 
    | (fst h) == elem && (snd h) == 1 = t
    | (fst h) == elem && (snd h) > 1 = (fst h, ((snd h) - 1)) : t
    | otherwise = h : remove' elem t

{-
- Busca um elemento na estrutura retornando sua quantidade. Caso o elemento nao exista, retorna 0 como a quantidade.
-}
search elem [] = 0
search elem (h:t) 
    | (fst h) == elem = snd h
    | otherwise = search elem t

{-
- Faz a uniao deste Bag com otherBag. A uniao consiste em ter os elementos dos dois Bas com suas maiores quantidades.
- Por exemplo, A = {(a,1),(c,3)}, B = {(b,2),(c,1)}. A.union(B) deixa A = {(a,1),(c,3),(b,2)}
-}
union' bag1 bag2 = dif bag1 bag2 ++ [k | (a,a0) <- bag1, (b,b0) <- bag2, a==b, let k = (a, if a0 > b0 then a0 else b0)] ++ dif bag2 bag1

{-
- Faz a intersecao deste Bag com otherBag. A intersecao consiste em ter os elementos que estao em ambos os bags com suas 
- menores quantidades. Por exemplo, Seja A = {(a,3),(b,1)} e B = {(a,1)}. Assim, A.intersection(B) deixa A = {(a,1)}
- Caso senhum elemento de A esteja contido em B ent�o a intersecao deixa A vazio.
-}
intersection' bag1 bag2 = [k | (a,a0) <- bag1, (b,b0) <- bag2, a==b, let k = (a,if a0 < b0 then a0 else b0)]

{-
- Faz a diferenca deste Bag com otherBag. A diferenca A \ B entre bags eh definida como segue:
- contem os elementos de A que nao estao em B
- contem os elementos x de A que estao em B mas com sua quantidade subtraida (qtde em A - qtde em B). 
Caso essa quantidade seja negativa o elemento deve serremovido do Bag. 
Por exemplo, seja A = {(a,3),(b,1)} e B = {(b,2),(a,1)}. Assim, A.minus(B) deixa A = {(a,2)}.
-}


dif [] _ = []
dif bag [] = bag
dif ((a,a0):t) bag2
    | (a `search` bag2) == 0 = (a, a0) :  dif t bag2
    | otherwise = dif t bag2

minus bag [] = bag
minus [] bag = []
minus bag1 bag2 = [k | (a,a0) <- bag1, (b,b0) <- bag2, a==b, let k = (a, a0-b0)] ++ dif bag1 bag2
{-
- Testa se este Bag esta incluso em otherBag. Para todo elemento deste bag, sua quantidade
- deve ser menor or igual a sua quantidade em otherBag.
-}
inclusion bag1 bag2 = bag1 == (intersection' bag1 bag2)

{-
- Realiza a soma deste Bag com otherBag. A soma de dois bags contem os elementos 
- dos dois bags com suas quantidades somadas. 
-}
sum' bag1 bag2 = [k | (a,a0) <- bag1, (b, b0) <- bag2, a==b, let k = (a, a0 + b0)]

{-
- Retorna a quantidade total de elementos no Baga
-}

size [] = 0
size ((a,a0):t) = a0 + size t 

x = zip "abcde" [1,2,3,5,6]
y = zip "deafg" [32,3,2,1,3]
w = zip "abcd" [10,20,30,40]
z = zip "abd" [1,2,2]
a = zip "ka" [3,2,3]
b = zip "ek" [1,1,5]
