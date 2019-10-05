--Escreva a declaracao para o tipo Triple, contendo tres elementos, todos de tipos diferentes.
--Escreva funcoes tripleFst, tripleSnd, tripleThr para extrair respectivamente o primeiro, segundo e terceiro
-- elementos de uma triple.
data Triple a b c = Nada deriving (Eq,Show)

tripleFst = undefined
tripleSnd = undefined
tripleThr = undefined

--Escreva um tipo Quadruple que contem 4 elementos: dois de um mesmo tipo e outros dois de outro tipo
--Escreva as funcoes frstTwo e secondTwo que retornam os dois primeiros e os dois ultimos, respectivamente
data Quadruple a b = Vazio  

firstTwo = undefined
secondTwo = undefined

--Escreva um tipo de dados que pode conter um, dois, tres ou quatro elementos, dependendo do construtor
--Implemente funções tuple1 até tuple4 que que retornam Just <valor> ou Nothing se o valor nao existe
data Tuple a b c d = NVazio

tuple1 = undefined 
tuple2 = undefined 
tuple3 = undefined 
tuple4 = undefined 

data List a = Nil | Cons a (List a) deriving (Eq,Show)

listLength Nil = 0
listLength (Cons x xs) = 1 + listLength xs

listHead Nil = error "Empty list"
listHead (Cons x xs) = x

listTail Nil = error "Empty list"
listTail (Cons x xs) = xs

listFoldr f v Nil = v
listFoldr f v (Cons x xs) = f x (listFoldr f v xs)


listFoldl f v Nil = v
listFoldl f v (Cons x xs) = listFoldl f (f v x) xs 

--Escreva as funcoes sobre a estrutura de dados binary tree
data BinaryTree a = NIL | Node a (BinaryTree a) (BinaryTree a) deriving (Eq,Show)

sizeBST NIL = 0
sizeBST (Node a left right) = 1 + sizeBST left + sizeBST right

--verifica se uma BT é uma BST
isBST bt = isSort (order bt)

--insere uma nova chave na BST retornando a BST modificada
insert = undefined


isNIL NIL = True
isNIL _ = False


left (Node _ l _) = l
left _ = NIL
right (Node _ _ r) = r
right _ = NIL

--retorna o Node da BST contendo o dado procurado ou entao NIL
search (Node v l r) element
    | v == element = (Node v l r)
    | element > v = search r element
    | otherwise = search l element
search _ _ = NIL

--retorna o elmento maximo da BST
mymaximum (Node v _ r) 
    | isNIL r = v
    | otherwise = mymaximum  r

--retorna o elemen  to minimo da BST
myminimum (Node v l _) 
    | isNIL l = v
    | otherwise = myminimum l

--retorna o predecessor de um elemento da BST, caso o elemento esteja na BST

predecessor bst e  = search bst ( lessThen (order bst) e )
lessThen (h:h2:t) e 
    | h2 == e = h
    | otherwise = lessThen (h2:t) e


--retorna o sucessor de um elemento da BST, caso o elemento esteja na BST
successor bst e = search bst ( biggerThen (order bst) e )
biggerThen (h:h2:t) e
    | h == e = h2
    | otherwise = biggerThen (h2:t) e
    
--remove ume lemento da BST
remove = undefined

--retorna uma lista com os dados da BST nos diversos tipos de caminhamento
preOrder NIL = []
preOrder(Node v l r) =  [v] ++ order l ++ order r

order NIL = []
order(Node v l r) = order l ++ [v] ++ order r

postOrder NIL = []
postOrder(Node v l r) =  order l ++ order r ++ [v]

isSort [] = True
isSort (p:[]) = True
isSort (h:p:t) = h < p && isSort (p:t)

bst = 
    (Node 5 
        (Node 3 
            (Node 2 
                (Node 1 NIL NIL) 
            NIL) 
            (Node 4 NIL NIL)
        )
        (Node 7
            (Node 6 NIL NIL)
            (Node 8 NIL
                (Node 11
                    (Node 9 NIL
                        (Node 10 NIL NIL)
                    )
                NIL)
            )
        )
    )