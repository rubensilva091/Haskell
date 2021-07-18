module F4 where
import Data.Char (isDigit, isAlpha)

--1

--a) x = [6,12,18]

--b) y = [2,4,6,8,10,12,14,16,18,20] -> x = [6,12,16]

--c) (x,y) = [(10,20),(11,19),(12,18),(13,17),(14,16),(15,15),(16,14),(17,13),(18,12),(19,11),(20,10)]

--d) y = [1,3,5,7,9] mas y<=x, x = [1,2,3,4,5,6,7,8,9], sum = [1,1,4,4,9,9,16,16,25,25]

--2

exA = [2^x | x <- [0..10]]

exB = [(x,y) | x <- [1..5], y <- [1..5], x+y == 6]

exC = [[1..x] | x <- [1..5]]

exD = [ replicate x 1 | x <- [1..5]]

exE = [ factorial x | x <- [1..6]]
    where factorial 0 = 1
          factorial x = x * factorial (x - 1)
		  
--3

digitAlpha2 :: String -> (String,String)
digitAlpha2 l = aux l [] []
			   where 
			   aux [] digit alpha = (digit,alpha)
			   aux (h:t) digit alpha = if isDigit h 
                                       then aux t (digit++[h]) alpha
				                       else if isAlpha h
				                            then aux t digit (alpha++[h])
				                            else aux t digit alpha
				

--4

nzp2 :: [Int] -> (Int,Int,Int)
nzp2 (h:t) = aux (h:t) 0 0 0
		where
		aux [] negativos zeros positivos = (negativos,zeros,positivos)
		aux (h:t) negativos zeros positivos = if h < 0 
		                                      then aux t (negativos+1) zeros positivos
											  else if h > 0 
											       then aux t negativos zeros (positivos+1)
												   else aux t negativos (zeros+1) positivos
												   
--5

divMod2 :: Integral a => a -> a -> (a,a)
divMod2 x y = aux x y 0
             where
             aux x y z = if x<y 
			             then (z,x)
						 else aux (x-y) y (z+1)

--6
--nao percebi nem faz sentido

fromDigits2 :: [Int] -> Int
fromDigits2 [] = 0
fromDigits2 (h:t) = h*10^(length t) + fromDigits2 t	

--7
--outra vez a mesma merda de exercicio
maxSumInit2 :: (Num a, Ord a) => [a] -> a
maxSumInit2 l = maximum [sum m | m <- inits l]

--8

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)



--estes ultimos 3 ex sao tao estupidos que nem a engenheira percebeu direito, fds ...
				 
						 
						 
		
												
				               
				   
				
	