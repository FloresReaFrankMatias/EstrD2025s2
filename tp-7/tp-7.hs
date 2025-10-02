data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show


belongBST :: Ord a => a -> Tree a -> Bool
--Prop: Dadao un BST dice si el elemento pertenece o no al arbol
--Costo: O(log n)
belongBST x EmptyT          = False
belongBST x (NodeT y ti td) = if x == y
                              then True 
                              else if x < y
                                   then  belongBST x ti
                                   else  belongBST x td

insertBST :: Ord a => a -> Tree a -> Tree a 
--Prop: Dado un BST inserta un elemento en el arbol
--Costo: O(log n)
insertBST  x EmptyT         = NodeT x EmptyT EmptyT
insertBST x (NodeT y ti td) = if x == y
                              then NodeT x ti td
                              else if x < y 
                                   then NodeT y (insertBST x ti) td
                                   else NodeT y ti (insertBST x td)



deleteBST :: Ord a => a -> Tree a -> Tree a
deleteBST x EmptyT          =
deleteBST x (NodeT y ti td) =
