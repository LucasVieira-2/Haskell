--Ficha 1

--1

perimetro :: Float -> Float 
perimetro r = 2*pi*r

dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1,y1) (x2,y2) = sqrt((x2-x1)^2 + (y2-y1)^2)

primUlt :: [Int] -> (Int,Int)
primUlt a = (head a, last a)

multiplo :: Int -> Int -> Bool
multiplo x y = if mod x y == 0 then True else False

truncaImpar ::[Int] -> [Int]
truncaImpar a = if mod (length a) 2 == 0 then a else tail a

max2 :: Int -> Int -> Int 
max2 x y = if x >= y then x else y

max3 :: Int -> Int -> Int -> Int 
max3 x y z = max2 (max2 x y) z  

--2

nRaizes :: Int -> Int -> Int -> Int 
nRaizes x y z | delta1 < 0 = 0 
			  | delta1 == 0 = 1
			  | delta1 > 0 = 2
			  where delta1 = (y^2)-4*x*z

raizes 