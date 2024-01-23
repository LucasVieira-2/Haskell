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

raizes :: Float -> Float -> Float -> [Float]
raizes x y z | delta2 < 0 = [] 
             | delta2 == 0 = [r]
             | delta2 > 0 = [r1, r2]
             where delta2 = (y^2) - 4*x*z
                   r = (-y) / (2*x)
                   r1 = ((-y) + sqrt delta2) / (2*x)
                   r2 = ((-y) - sqrt delta2) / (2*x)


--3
type Hora = (Int, Int)

horaValida :: Hora -> Bool
horaValida (h, m) = if h < 24 && m < 60 then True else False

horaDps :: Hora -> Hora -> Bool
horaDps (h1, m1) (h2, m2) | h1 > h2 = True
			  | h1 < h2 = False
			  | h1 == h2 && m1 > m2 = True
     			  |otherwise = False

converteHoras :: Hora -> Int
converteHoras (h, m) = h*60 + m

converteMinutos :: Int -> Hora
converteMinutos m = (div m 60, mod m 60)

difHoras :: Hora -> Hora -> Int
difHoras h1 h2 = abs(converteHoras(h1) - converteHoras(h2))

adcmin :: Int -> Hora -> Hora 
adcmin x h = converteMinutos(converteHoras(h) + x) 


--5
data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)
--vermelho verde amarelo
next :: Semaforo -> Semaforo
next x | x == Amarelo = Vermelho
next x | x == Verde = Amarelo
next x | x == Vermelho = Verde

stop :: Semaforo -> Bool
stop x = if x == Vermelho then True else False

safe :: Semaforo -> Semaforo -> Bool
safe x y | x == Verde && y == Verde = False
         | x == Verde && y == Amarelo = False
         | x == Verde && y == Vermelho = True
         | x == Amarelo && y == Verde = False
         | x == Amarelo && y == Amarelo = False
         | x == Amarelo && y == Vermelho = True
         | x == Vermelho && y == Verde = True
         | x == Vermelho && y == Amarelo = True
         | x == Vermelho && y == Vermelho = True

--6
data Ponto = Cartesiano Double Double | Polar Double Double
            deriving (Show,Eq)

posx :: Ponto -> Double
posx (Cartesiano x y) = x 
posx (Polar d a) = d*(cos a)

posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar d a) = d*(sin a)

raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt (x^2 + y^2)
raio (Polar d a) = d 

angulo :: Ponto -> Double
angulo (Cartesiano 0 0) = 0
angulo (Cartesiano 0 y1) | y1 > 0 = pi/2
                         | y1 < 0 = -pi/2
angulo (Cartesiano x1 y1) | x1 > 0 = atan (y1/x1)
                          | x1 < 0 = atan (y1/x1) + pi
angulo (Polar d a) = a

--7
