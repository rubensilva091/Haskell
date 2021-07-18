module F2 where
import Data.Char (ord)
import Data.List (partition)

--1

--BULLSHIT

--2

dobros2 :: [Float] -> [Float]
dobros2 [] = []
dobros2 (h:t) = 2*h:dobros2 t


num0corre2 :: Char -> String -> Int
num0corre2 ch (h:t) = num0corre2 ch t 0
					  where
					  num0corre2 ch [] i = i
					  num0corre2 ch (h:t) i = if ch == h 
										  then num0corre2 ch t i+1
										  else num0corre2 ch t i
										  	  

positivos2 :: [Int] -> Bool
positivos2 [] = True
positivos2	(h:t) = if h>=0 
					then positivos2 t 
					else False


soPos2 :: [Int] -> [Int]		
soPos2 [] = []
soPos2 (h:t) = if h >=0  
			   then h:soPos2 t 
			   else soPos2 t
			   

somaNeg2 :: [Int] -> Int
somaNeg2 l1 = somaNeg2 l1 0
			where
			somaNeg2 [] n = n
			somaNeg2 (h:t) n = if h<0 	
							   then somaNeg2 t (n+h)  
							   else somaNeg2 t n
							   
							   
tresUlt2 :: [a] -> [a]
tresUlt2 [] = []	
tresUlt2 l1 = if length l1 <= 3 
			  then (head l1):tresUlt2 (tail l1) 
			  else tresUlt2 (tail l1)


segundos2 :: [(a,b)] -> [b]
segundos2 [] =[]
segundos2 ((a,b):t) = b:segundos2 t


nosPrimeiros2 :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros2 _ [] = False
nosPrimeiros2 n ((a,b):t) = if n == a 
							then True 
							else nosPrimeiros2 n t
							
sumTriplos2 :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos2 l1 = aux l1 (0,0,0)
			   where 
			   aux [] n = n
			   aux ((a,b,c):t) (a1,a2,a3) = aux t (a1+a,a2+b,a3+c)


--3

soDigitos2 :: [Char] -> [Char]
soDigitos2 [] =[]
soDigitos2 (h:t) = if h `elem` ['0'..'9'] 
				   then h:soDigitos2 t 
				   else soDigitos2 t

minusculas2 :: [Char] -> Int
minusculas2 (h:t) = aux (h:t) 0
					where
					aux [] i = i
					aux (h:t) i = if h `elem` ['a'..'z'] 
								  then aux t (i+1)
								  else aux t i


nums2 :: String -> [Int]
nums2 [] =[]
nums2 (h:t) = if h `elem` ['0'..'9'] 
			  then (ord h - ord '0'):nums2 t   --Nao percebi o pq do ord
			  else nums2 t


--4

type Polinomio = [Monomio]
type Monomio = (Float,Int)

conta :: Int -> Polinomio -> Int
conta n l1 = aux n l1 0
			 where
			 aux _ [] i = i
			 aux n ((a,b):t) i = if n==b 
								 then aux n t (i+1)
								 else aux n t i

grau :: Polinomio -> Int
grau l1 = aux l1 0
		  where
		  aux [] i = i
		  aux ((a,b):t) i = if i<b 
						    then aux t b 
						    else aux t i
							

selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau n ((a,b):t) = if n == b 
					  then (a,b):selgrau n t	
				      else selgrau n t


deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((a,b):t) = if b == 0 then deriv t else (a*fromIntegral b, b-1):deriv t


calcula :: Float -> Polinomio -> Float
calcula x [] = 0
calcula x ((a,b):t) = a*(x^b) + calcula x t


simp :: Polinomio -> Polinomio
simp [] = []
simp ((a,b):t) = if b /= 0
				 then (a,b):simp t	
				 else simp t


--normaliza :: Polinomio -> Polinomio

--todos asseguir precisam do normaliza, nao ha paciencia para tal fds

soma :: Polinomio -> Polinomio -> Polinomio
soma ((a,b):t1) ((x,y):t2) = if 
				 
		  
		  
								 
			  
				