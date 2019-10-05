module MultisetMap ()
where

{- 
- Um multi-conjunto (ou bag) é uma estrutura que representa uma coleção de objetos que 
- permite duplicadas. Entretanto, as duplicatas são armazenadas como a quantidade de 
- ocorréncias do mesmo elemento no multi-conjunto. Exemplo, a coleção {a,b,c,c,c,b} poderia 
- ser representada como sendo {(a,1), (b,2), (c,3)}. A ideia de multi-conjunto pode ser 
- implementada de diversas formas. Uma delas é usando a implementacao de Data.Map, onde 
- cada elemento da lista consiste do dado em si mapeado para sua quantidade. 
- Eh recomendavel que voce consulte a documentacao de Data.Map
-}
import Data.Map as Map

{-
- Insere um elemento na estrutura. Caso o elemento ja existe, sua quantidade na estrutura sera incrementada.
-}

-- insert key bag  
--     | Map.notMember key bag =   Map.alter f key bag
--     | otherwise = Map.alter f (item + 1) bag 
--     where 
--         f _ = Just 1
--         item = toT (Map.lookup key bag)

toT (Just a) = a
toT Nothing  = 0


insert' key bag = Map.alter f key bag 
    where 
        f _ = if Map.member key bag 
            then Just (toT(Map.lookup key bag) + 1 ) 
            else Just 1



{-
- Remove um elemento da estrutura, levando em consideracao a manipulacao de sua quantidade na estrutura. 
- Caso a quantidade atinja 0 (ou menos), o elemento deve realmente ser removido da estrutura
-}
remove' key bag 
    | Map.member key bag && ((toT(Map.lookup key bag))  == 1) = Map.delete key bag
    | Map.member key bag = Map.alter f key bag 
    | otherwise = Map.delete key bag
    where 
    f _ = Just (toT(Map.lookup key bag) - 1 ) 
    zero = Just 0
        


{-
 - Busca um elemento na estrutura retornando sua quantidade. Caso o elemento nao exista, retorna 0 como a quantidade.
-}
search' key bag = toT $ Map.lookup key bag 

{-
 - Faz a uniao deste Bag com otherBag. A uniao consiste em ter os elementos dos dois Bags com suas maiores quantidades.
 - Por exemplo, A = {(a,1),(c,3)}, B = {(b,2),(c,1)}. A.union(B) deixa A = {(a,1),(c,3),(b,2)}
-}

aux2 [] _ _ = empty
aux2 (h:t) bag1 bag2 =
    if v1 > v2 
        then Map.fromList [(h,v1)] `Map.union` aux2 t bag1 bag2 
        else Map.fromList [(h,v2)] `Map.union` aux2 t bag1 bag2
    where
        v1 = toT (Map.lookup h bag1)
        v2 = toT (Map.lookup h bag2)

aux3 [] _ _ = empty
aux3 (h:t) bag1 bag2 =
    if v1 < v2 
        then Map.fromList [(h,v1)] `Map.union` aux3 t bag1 bag2 
        else Map.fromList [(h,v2)] `Map.union` aux3 t bag1 bag2
    where
        v1 = toT (Map.lookup h bag1)
        v2 = toT (Map.lookup h bag2)

union' bag1 bag2 = (aux2 (keys (Map.difference bag1 (dif12 `Map.union` dif21))) bag1 bag2) `Map.union` dif12 `Map.union` dif21
    where
        keys1 = Map.keys bag1
        keys2 = Map.keys bag2
        dif12 = Map.difference bag1 bag2
        dif21 = Map.difference bag2 bag1
{-
 - Faz a intersecao deste Bag com otherBag. A intersecao consiste em ter os elementos que estao em ambos os bags com suas 
 - menores quantidades. Por exemplo, Seja A = {(a,3),(b,1)} e B = {(a,1)}. Assim, A.intersection(B) deixa A = {(a,1)}
 - Caso senhum elemento de A esteja contido em B ent�o a intersecao deixa A vazio.
-}
intersection' bag1 bag2 =  
    (aux3 (keys (Map.difference bag1 (dif12 `Map.union` dif21))) bag1 bag2)
    where
        keys1 = Map.keys bag1
        keys2 = Map.keys bag2
        dif12 = Map.difference bag1 bag2
        dif21 = Map.difference bag2 bag1


{-
 - Faz a diferenca deste Bag com otherBag. A diferenca A \ B entre bags eh definida como segue:
- contem os elementos de A que nao estao em B
- contem os elementos x de A que estao em B mas com sua quantidade subtraida (qtde em A - qtde em B). 
Caso essa quantidade seja negativa o elemento deve serremovido do Bag. 
     Por exemplo, seja A = {(a,3),(b,1)} e B = {(b,2),(a,1)}. Assim, A.minus(B) deixa A = {(a,2)}.
-}

aux [] _ _ = empty
aux4 (h:t) bag1 bag2 
        | flag = Map.fromList [(h, v1 - v2)] 
        | otherwise = aux4 t bag1 bag2
    where
        v1 = toT(Map.lookup h bag1)
        v2 = toT(Map.lookup h bag2)
        flag = v1 - v2 /= 0
        
minus' bag1 bag2 = aux4 differenceKeys bag1 bag2
    where 
        differenceKeys = keys (bag1 `Map.difference` bag2)

{-
 - Testa se este Bag esta incluso em otherBag. Para todo elemento deste bag, sua quantidade
 - deve ser menor or igual a sua quantidade em otherBag.
-}
inclusion' bag1 bag2 = bag1 == (MultisetMap.intersection' bag1 bag2)

{-
 - Realiza a soma deste Bag com otherBag. A soma de dois bags contem os elementos dos dois bags com suas quantidades somadas. 
-}
aux5 [] _ _ = empty
aux5 (h:t) bag1 bag2 = fromList [(h, v1 + v2)] `Map.union` aux5 t bag1 bag2
    where 
        v1 = toT (Map.lookup h bag1)
        v2 = toT (Map.lookup h bag2) 


sum' bag1 bag2 = (dif12 `Map.union` dif21) `Map.union` (aux5 keys bag1 bag2  )  
    where
        keys = Map.keys (intersection' bag1 bag2)
        dif12 = Map.difference bag1 bag2
        dif21 = Map.difference bag2 bag1


{-
 - Retorna a quantidade total de elementos no Bag
-}
size' bag = sum (Map.elems bag)


x = Map.fromList $ zip "abcde" [1,2,3,5,6]
y = Map.fromList $ zip "deafg" [32,3,2,1,3]
w = Map.fromList $ zip "abcd" [10,20,30,40]
z = Map.fromList $ zip "abd" [1,2,2]
a = Map.fromList $ zip "ka" [3,2]
b = Map.fromList $ zip "ek" [1,1]