module F1 where
import Data.List
import System.IO
import Data.Char

--1

perimetro :: Float -> Float
perimetro x = (2*pi)*x

distCart :: (Num a, Eq a, Floating a) => (a,a) -> (a,a) -> a
distCart (x,y) (x1,y1) = sqrt (((x1-x)^2)+((y1-y)^2))

primUlt :: Num a => [a]-> (a,a)
primUlt l = (head l, last l)

multiplo :: Int -> Int -> Bool
multiplo x y = if mod x y == 0
			   then True
			   else False
			   
			   
truncaImpar :: [a] -> [a]
truncaImpar l = if mod (length l) 2 == 0
				then l
				else tail l
				
max2 :: Int -> Int -> Int 
max2 x y = if x>y then x else y

max3 :: Int -> Int -> Int -> Int
max3 x y z = if x>y && x>z then x else max2 y z

--2

nRaizes a b c 
    | delta > 0 = 2 
    | delta == 0 = 1
    | delta < 0 = 0 
    where delta = b^2 - 4*a*c

raizes a b c 
    | n == 2 = [x1, x2] 
    | n == 1 = [x1] 
    | n == 0 = [] 
    where n = nRaizes a b c
          delta = b^2 - 4*a*c
          (x1,x2) = (((-b) + sqrt delta)/ (2*a), ((-b) - sqrt delta)/ (2*a))
		  
--3

type Hora = (Int,Int)

testHora :: Hora -> Bool
testHora (x,y) = if (x >= 0 && x <=23) && (y>=0 && y<=59) 
                 then True 
				 else False

compHora :: Hora -> Hora -> Bool
compHora (x1,y1) (x2,y2) = if x1>x2 
						   then True
						   else if (x1 == x2 && y1>y2)
						   then True
						   else False

convminHora :: Hora ->  Int
convminHora (x,y) = ((60*x)+y)

convhoraHora :: Int -> Hora
convhoraHora x = ((div x 60),(mod x 60))

diffHora :: Hora -> Hora -> Int
diffHora (x1,y1) (x2,y2) = (convminHora (x1,y1)-convminHora(x2,y2))

addminHora :: Hora -> Int -> Hora
addminHora (x1,y1) m = (x1+a,y1+b)
					  where 
					  a = div m 60
					  b = mod m 60
					  
--4		

data Hora2 = H Int Int deriving (Show,Eq)
                            
testHora2 :: Hora2 -> Bool
testHora2 (H x y) = if (x >= 0 && x <=23) && (y>=0 && y<=59) 
                 then True 
				 else False

compHora2 :: Hora2 -> Hora2 -> Bool
compHora2 (H x1 y1) (H x2 y2) = if x1>x2 
						   then True
						   else if (x1 == x2 && y1>y2)
						   then True
						   else False

convminHora2 :: Hora2 ->  Int
convminHora2 (H x y) = ((60*x)+y)

convhoraHora2 :: Int -> Hora2
convhoraHora2 x = H (div x 60) (mod x 60)

diffHora2 :: Hora2 -> Hora2 -> Int
diffHora2 (H x1 y1) (H x2 y2) = (convminHora (x1,y1)-convminHora(x2,y2))

addminHora2 :: Hora2 -> Int -> Hora2
addminHora2 (H x1 y1) m = H (x1+a) (y1+b)
					  where 
					  a = div m 60
					  b = mod m 60
					  
--5

data Semaforo = Verde | Amarelo | Vermelho deriving (Show, Eq)

nextSemaforo :: Semaforo -> Semaforo
nextSemaforo x = case x of Verde -> Amarelo
                           Amarelo -> Vermelho
                           Vermelho -> Verde
						   
stopSemaforo :: Semaforo -> Bool
stopSemaforo x = if x == Vermelho 
			     then True
			     else False

safeSemaforo :: Semaforo -> Semaforo -> Bool
safeSemaforo x y = if x == Vermelho || y == Vermelho
				   then True
				   else False
				   
--6

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

posx :: Ponto -> Double
posx ponto = case ponto of Cartesiano x _ -> x
                           Polar d a -> d * cos a
					
posy :: Ponto -> Double					
posy ponto = case ponto of Cartesiano _ y -> y
                           Polar d a -> d * sin a
						   
raio :: Ponto -> Double
raio ponto = case ponto of Cartesiano x y -> sqrt ((x^2)+(y^2))
                           Polar d _ -> d
						   
angulo :: Ponto -> Double	
angulo ponto = case ponto of Cartesiano x y -> atan (y/x)
                             Polar _ a -> a

dist :: Ponto -> Ponto -> Double
dist ponto1 ponto2 = sqrt ((((posx ponto1)-(posx ponto2))^2) +(((posy ponto1)-(posy ponto2))^2))
						   

--7

data Figura = Circulo Ponto Double | Rectangulo Ponto Ponto | Triangulo Ponto Ponto Ponto deriving (Show,Eq)

poligono :: Figura -> Bool 
poligono (Circulo x y) = False
poligono (Rectangulo ponto1 ponto2) = if ((posx ponto1 /= posx ponto2) && (posy ponto1 /= posy ponto2))
                                      then True
									  else False
									  
poligono (Triangulo ponto1 ponto2 ponto3) = (posy ponto2 - posy ponto1) / (posx ponto2 - posx ponto1) /= (posy ponto3 - posy ponto2) / (posx ponto3 - posx ponto2)		


--vertices :: Figura -> [Ponto]
--vertices :: Figura -> [Ponto]


--Nao acabei, nao tenho paciencia pa esta merda

--8

isLower2 :: Char -> Bool
isLower2 x = if x `elem` ['a'..'z'] 
			 then True 
			 else False 

isUpper2 :: Char -> Bool
isUpper2 x = if x `elem` ['A'..'Z'] 
			 then True 
			 else False 
			 
isAlpha2 :: Char -> Bool
isAlpha2 x = if x `elem` ['a'..'z'] && x `elem` ['A'..'Z']
			 then True
			 else False

isDigit2 :: Char -> Bool
isDigit2 x = if x `elem` ['0'..'9'] 
			 then True
			 else False 			 
			 
intToDigit2 :: Int -> Char
intToDigit2 x = chr (x + 48)

digitToInt2 :: Char -> Int 
digitToInt2 x = ord x - 48






			  


