module Ficha6 where
--Binary tree

data BTree a = Empty
            | Node a (BTree a) (BTree a)
        deriving Show


arv :: BTree Int
arv = Node 7 (Node 4 Empty(Node 10 Empty Empty))
             (Node 9 (Node 8 Empty (Node 5 Empty Empty))(Node 13 Empty Empty))



       --                            7
       --                      /           \
       --                     4             9
       --                    / \           /  \
       --                       10        8    13
       --                                /\    /\
       --                                 5
       --                                 /\                                           
                                   
------1

altura :: BTree a -> Int
altura Empty = 0
altura (Node _ e d) = 1 + max (altura e) (altura d)


contaNodos :: BTree a -> Int 
contaNodos Empty = 0
contaNodos (Node _ e d) = 1 + contaNodos e + contaNodos d


folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node _ Empty Empty) = 1
folhas (Node _ e d) = folhas e + folhas d 


prune :: Int -> BTree a -> BTree a
prune 0 _ = Empty 
prune _ Empty = Empty
prune x (Node i e d) = Node i (prune (x-1) e) (prune (x-1) d) 


path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node i e d) = [i]
path (False:xs) (Node i e d) = i : path xs e 
path (True:xs) (Node i e d) = i : path xs d      


mirror :: BTree a -> BTree a 
mirror Empty = Empty
mirror (Node i e d) = Node i (mirror d) (mirror e) 


zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT _ Empty _ = Empty
zipWithBT _ _ Empty = Empty
zipWithBT f (Node i e d) (Node i2 e2 d2) = Node (f i i2) (zipWithBT f e e2) (zipWithBT f d d2)  


unzipBT :: BTree (a, b, c) -> (BTree a, BTree b, BTree c)
unzipBT Empty = (Empty, Empty, Empty)
unzipBT (Node (x, y, z) e d) =
    (Node x e1 d1, Node y e2 d2, Node z e3 d3)
  where
    (e1, e2, e3) = unzipBT e
    (d1, d2, d3) = unzipBT d


-------2

arvbp :: BTree Int
arvbp = Node 10 (Node 4 Empty(Node 7 Empty Empty))
             (Node 15 (Node 11 Empty (Node 12 Empty Empty))(Node 18 Empty Empty))


minimo :: Ord a => BTree a -> a 
minimo (Node i Empty _) = i
minimo (Node i e _) = minimo e 


semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node i Empty d) = d 
semMinimo (Node i e d) = Node i (semMinimo e) d


--minSmin :: Ord a => BTree a -> (a,BTree a)


remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove v (Node i e d)
    | v < i     = Node i (remove v e) d
    | v > i     = Node i e (remove v d)
    | otherwise = case (e, d) of
                      (Empty, _) -> d
                      (_, Empty) -> e
                      _          -> let m = minimo d
                                        r' = remove m d
                                    in Node m e r'



















