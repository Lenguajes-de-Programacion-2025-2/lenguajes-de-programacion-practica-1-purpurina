module Practica0 where 

{-- Recursion y recursion de Cola --}

--Funcion buscar : Dada una lista de Enteros y elemento , Regresa verdadero en caso de que el elemento se encuentre en la lista
--En otro caso regresa False 

buscar :: [Int] -> Int -> Bool
buscar [] _ = False  
buscar (x:xs) y
  | x == y    = True  
  | otherwise = buscar xs y 


--Funcion sumar_lista : Dada una Lista de Entero , regresa la suma de sus elementos
--Implementala con recursion de Cola
sumar_lista :: [Int] -> Int
sumar_lista p = sumar_lista_aux p 0

-- Función auxiliar con acumulador para recursión de cola
sumar_lista_aux :: [Int] -> Int -> Int
sumar_lista_aux [] acumulador = acumulador
sumar_lista_aux (x:xs) acumulador = sumar_lista_aux xs (acumulador + x)



--Implementa una funcion recursiva de forma "ordinaria" y despues implementala con recursion de cola
--Usa los comandos vistos en clase para comparar tiempo y memoria usados y dado el resultado describe que sucedio
--Y porque crees que haya sido asi
-- :s +t (en el ghci  para ver la estadisticas )

--Vrsión Ordinaria
sumar_ordinario :: [Int] -> Int
sumar_ordinario [] = 0
sumar_ordinario (x:xs) = x + sumar_ordinario xs

-- Versión con Recursión de Cola
sumar_cola :: [Int] -> Int
sumar_cola p = sumar_cola_aux p 0

sumar_cola_aux :: [Int] -> Int -> Int
sumar_cola_aux [] acumulador = acumulador
sumar_cola_aux (x:xs) acumulador = sumar_cola_aux xs (acumulador + x)


--
{--funciones--}

--Funcion filter toma un predicado (funcion que regresa booleano) y filtra los elementos la lista de entrada  dada la condicion
filterB :: (a -> Bool) -> [a] -> [a]
filterB _ [] = []  
filterB p (x:xs)
    | p x       = x : filterB p xs  
    | otherwise = filterB p xs  

--Implementa una funcion llamada mapear que reciba como argumento una funcion y aplique esta funcion a una lista
mapear :: (a -> b) -> [a] -> [b]
mapear _ [] = []  
mapear f (x:xs) = f x : mapear f xs  


--Decima extra : .2
--Forma comprehension
mapear_ :: (a -> b) -> [a] -> [b]
mapear_ f list = [f x | x <- list]




{--Tipos,clases y Estructuras de Datos --}

--Arbol 
data Tree a = Empty 
            | Node a (Tree a) (Tree a)
            deriving (Show, Eq)
 


--Dada la definicion de arbol binario has una funcion que haga un recorrido pre order
preorder:: Tree a -> [a]
preorder Empty =  []
preorder (Node root left right) =  [root] ++  preorder left ++ preorder right

--Hacer una funcion que calcule la altura del arbol ,regresa verdadero en caso de encontrar el eelemento en el arbol
buscar_tree:: Eq a => Tree a -> a -> Bool
buscar_tree Empty _ =  False
buscar_tree (Node t left right) elem
    | t==elem = True
    | otherwise = buscar_tree left elem || buscar_tree right elem


--Punto Extra:  Implementa  una funcion que cuente la cantidad de hojas del arbol 
hojas:: Tree a -> Int
hojas Empty  = 0
hojas (Node _ Empty Empty) = 1
hojas (Node _ left right) = hojas left + hojas right

--Definicion de Grafica 

type Vertex = Int
type Graph = [(Vertex, [Vertex])]

vecinos :: Graph -> Vertex -> [Vertex]
vecinos [] _ = []  
vecinos ((v, ns):xs) x
    | v == x    = ns 
    | otherwise = vecinos xs x

dfs :: Graph -> Vertex -> [Vertex] -> [Vertex]
dfs graph v visited
    | v `elem` visited = visited  
    | otherwise = foldl (\acc n -> dfs graph n acc) (v : visited) (vecinos graph v)

--Dada la siguiente defincion de grafica , crea una funcion que verifique si la grafica es conexa 
--Tip: USA la funcion auxiliar dfs, (si quieres puedes usar otra de tu propio diseño)


isConnected :: Graph -> Bool
isConnected [] = True  -- Una gráfica vacía se considera conexa.
isConnected graph =
    let startVertex = fst (head graph)  -- Tomamos un vértice inicial.
        visited = dfs graph startVertex []  -- Aplicamos DFS desde ese vértice.
        allVertices = map fst graph  -- Extraemos todos los vértices de la gráfica.
    in all (`elem` visited) allVertices  -- Verificamos si todos fueron visitados.


--Ejemplos

connectedGraph :: Graph
connectedGraph = [(1, [2,3]), (2, [4]), (3, [4,5]), (4, [6]), (5, [6]), (6, [])]  --Debe Regresar True

disconnectedGraph :: Graph
disconnectedGraph = [(1, [2]), (2, [1]), (3, [4]), (4, [3])] --Debe regresar False 

graph1 :: Graph
graph1 = [(1, [2]), (2, [1, 3]), (3, [2])]  -- Un árbol con 3 vértices y 2 aristas

tree1 :: Tree Int
tree1 = Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty)

--La siguiente funcion verfiica que la grafica es un arbol 
--Tip : Recuerda que un arbol es una grafica conexa y sin ciclos
isTree :: Graph -> Bool
isTree [] = False  -- Una gráfica vacía no es un árbol
isTree graph =
    let numVertices = length graph  -- Número de vértices
        numEdges = sum (map (length . snd) graph) `div` 2  -- Número de aristas (dividido entre 2 porque es no dirigida)
    in isConnected graph && numEdges == numVertices - 1


--La siguiente funcion regresa a suma de las hojas del arbol
leafSum:: Tree Int -> Int 
leafSum Empty = 0
leafSum (Node n Empty Empty) = n
leafSum (Node _ left right) = leafSum left + leafSum right