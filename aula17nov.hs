module Ficha7 where 

data ExpInt = Const Int
      | Simetrico ExpInt
      | Mais ExpInt ExpInt
      | Menos ExpInt ExpInt
      | Mult ExpInt ExpInt


--3 + 4 * 5

e :: ExpInt
e = Mais (Const 3) (Mult(Const 4) (Const 5))

e' = (Const 3) `Mais` ((Const 4) `Mult` (Const 5)) 

e'' = ((Const 3) `Mais` (Const 4)) `Mult` (Const 5) 

calcula :: ExpInt -> Int 
calcula (Const x) = x
calcula (Simetrico e) = - (calcula e)
calcula (Mais e1 e2) = calcula e1 + calcula e2
calcula (Menos y1 y2) = calcula y1 - calcula y2
calcula (Mult z1 z2) = calcula z1 * calcula z2


infixa :: ExpInt -> String
infixa (Const x) = show x
infixa (Simetrico e) = "(-" ++ (infixa e) ++ ")"
infixa (Mais e1 e2) = "(" ++ infixa e1 ++ "+" ++ infixa e2 ++ ")"
infixa (Menos y1 y2) = "(" ++ infixa y1 ++ "+" ++ infixa y2 ++ ")"
infixa (Mult z1 z2) = "(" ++ infixa z1 ++ "+" ++ infixa z2 ++ ")"


posfixa :: ExpInt -> String
posfixa (Const x) = show x
posfixa (Simetrico e) = "(" ++ (posfixa e) ++ ")"
posfixa (Mais e1 e2) = posfixa e1 ++ posfixa e2 ++ " + "
posfixa (Menos e1 e2) = posfixa e1 ++ posfixa e2 ++ " - "
posfixa (Mult e1 e2) = posfixa e1 ++ posfixa e2 ++ " * "



data RTree a = R a [RTree a]

rt :: RTree Int  
rt = R 7 [R 1[R 9[], R 8[], R 10[], R 6[]], R 5[], R 4[R 10[], R 12[]]]

soma :: Num a => RTree a -> a
soma (R x l) = x + sum (map soma l)

altura :: RTree a -> Int
altura (R _ []) = 1
altura (R _ l) = 1 + maximum (map altura l)

prune :: Int -> RTree a -> RTree a 
prune 1 (R x _) = R x []
prune n (R x l) = R x (map (prune (n-1)) l) 
infixa (Simetrico e) = "(-" ++ (infixa e) ++ ")"
infixa (Mais e1 e2) = "(" ++ infixa e1 ++ "+" ++ infixa e2 ++ ")"
infixa (Menos y1 y2) = "(" ++ infixa y1 ++ "+" ++ infixa y2 ++ ")"
infixa (Mult z1 z2) = "(" ++ infixa z1 ++ "+" ++ infixa z2 ++ ")"


posfixa :: ExpInt -> String
posfixa (Const x) = show x
posfixa (Simetrico e) = "(" ++ (posfixa e) ++ ")"
posfixa (Mais e1 e2) = posfixa e1 ++ posfixa e2 ++ " + "
posfixa (Menos e1 e2) = posfixa e1 ++ posfixa e2 ++ " - "
posfixa (Mult e1 e2) = posfixa e1 ++ posfixa e2 ++ " * "



data RTree a = R a [RTree a]

rt :: RTree Int  
rt = R 7 [R 1[R 9[], R 8[], R 10[], R 6[]], R 5[], R 4[R 10[], R 12[]]]

soma :: Num a => RTree a -> a
soma (R x l) = x + sum (map soma l)

altura :: RTree a -> Int
altura (R _ []) = 1
altura (R _ l) = 1 + maximum (map altura l)

prune :: Int -> RTree a -> RTree a 
prune 1 (R x _) = R x []
prune n (R x l) = R x (map (prune (n-1)) l) 
