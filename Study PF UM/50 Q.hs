import Data.List
import System.IO

--1 

enumFromTo2 :: Int -> Int -> [Int]
enumFromTo2 x y = if x<y then x:enumFromTo2 (x+1) y else [y]

--2
enumFromThenTo2 :: Int -> Int-> Int -> [Int]
enumFromThenTo2 x y z = if x>z then []
						else x:enumFromThenTo2 x (y-z) z

--3

concat3 :: [a] -> [a] -> [a]
concat3 _ [] = []
concat3 [] l = l
concat3 (h:t) l = h:concat3 (t) l 

--4

finder2 :: [a] -> Int -> a
finder2 (h:t) x = if x==0 then h else finder2 t (x-1)

--5

reverse2 :: [a] -> [a]
reverse2 []=[]
reverse2 l = last (l):reverse2(init l)

--6

take2 :: Int -> [a] -> [a]
take2 0 _ = []
take2 _ []= []
take2 x (h:t) = h:take2 (x-1) t

--7

drop2 :: Int -> [a] -> [a]
drop2 _ []= []
drop2 x l = if x>0 then drop2 (x-1) (tail l) else head l:drop2 x(tail l)

--8

zip2 :: [a] -> [b] -> [(a,b)]
zip2 [] l2 = []
zip2 l1 [] = []
zip2 l1 l2 = (head l1, head l2) : zip2 (tail l1) (tail l2)

--9

elem2 :: Eq a => a -> [a] ->Bool
elem2 _ [] = False
elem2 x l = if x == head (l) then True else elem2 x(tail l)

--10

replicate2 :: Int -> a -> [a] 
replicate2 0 _ = []
replicate2 x y = y:replicate2(x-1) y

--11

intersperse2 :: a -> [a] ->[a]
intersperse2 _ [] = []
intersperse2 _ [h] = [h]
intersperse2 x l = (head l):x: intersperse2 x (tail l)

--12

group2 :: Eq a => [a] -> [[a]] 
group2 [] = [[]]
group2 (h:t) = aux [h] t
              where
                aux a [] = [a]
                aux a (h:t) | elem h a = aux (h:a) t
                            | otherwise = a :aux [h] t
--13

concat7 :: [[a]] -> [a]
concat7 [] = []
concat7 l1 = (head l1)++ concat7 (tail l1)

--14

inits2 :: [a] -> [[a]]
inits2 [] = [[]]
inits2 l1 = inits2(init l1) ++ [l1]

--15
 
tails2 :: [a] -> [[a]]
tails2 [] = [[]]
tails2 l1 = [l1] ++ tails2(tail l1) 

--16

isPrefixOf2 :: Eq a => [a]-> [a] -> Bool
isPrefixOf2 [] _ = True
isPrefixOf2 _ [] = False
isPrefixOf2 l1 l2 = if (head l1) == (head l2) 
                    then isPrefixOf2(tail l1) (tail l2)
					else False

--17

isSuffixOf2 :: Eq a => [a]-> [a] -> Bool
isSuffixOf2 [] _ =True
isSuffixOf2 _ []= False
isSuffixOf2 l1 l2 = if (last l1) == (last l2)
					then isSuffixOf2(init l1) (init l2)
					else False 

--18

isSubsequenceOf2 :: Eq a =>[a] -> [a] -> Bool
isSubsequenceOf2 [] _ = True
isSubsequenceOf2 _ [] = False
isSubsequenceOf2 l1 l2 = if (head l1) /= (head l2) 
                         then isSubsequenceOf2 l1 (tail aux) 
						 else isSubsequenceOf2 (tail l1) aux
						 where aux = l2
						 
						 
--19						 
						 
elemIndices2 :: Eq a => a ->[a] -> [Int]
elemIndices2 _ [] = []
elemIndices2 n l1 = aux 0 n l1
					where
					aux _ _ [] =[]
					aux i n l1 = if n == head l1 
										then i:aux (i+1) n (tail l1)
										else aux i n (tail l1)
										
--20

nub2 :: Eq a => [a] -> [a]
nub2 [] = []
nub2 (h:t) = if h `elem` t then nub2 t else h:nub2 t

--21

delete2 :: Eq a => a -> [a] -> [a]
delete2 _ [] =[]
delete2 n l1 = aux 0 n l1
			where
			aux _ _ [] = []
			aux i n l1 = if n == (head l1) && i==0 
						 then  aux (i+1) n (tail l1)
						 else (head l1): aux i n (tail l1)


--22

separe2 :: Eq a => [a] -> [a]-> [a]
separe2 _ []= []
separe2 [] _ = []
separe2 l1 l2= if (head l1) `elem` (l2)
			   then (head l1): separe2 (tail l1) l2
			   else separe2 (tail l1) l2 

--23

union' :: Eq a => [a] -> [a] -> [a]
union' l [] = l
union' l (h:t)
    | h `elem` l = union' l t
    | otherwise = union' (l ++ [h]) t
	
	
--24
	
intersect2 :: Eq a => [a] ->[a] -> [a]
intersect2 l1 [] = []
intersect2 [] l2 = []
intersect2 l1 l2 = if (head l1) `elem` l2 
				   then (head l1):intersect2 (tail l1) l2
				   else intersect2 (tail l1) l2
				
				
	
--25

insert2 :: Ord a => a -> [a] -> [a]
insert2 n [] = [n]
insert2 n l1 = aux 0 n l1
				where
				aux _ _ [] =[]
				aux i n l1= if n < (head l1) && i == 0
							then n:(head l1):aux (i+1) n (tail l1)
							else (head l1):aux i n (tail l1)


--25.1

insert4 :: Ord a => a -> [a] -> [a]
insert4 x [] = [x]
insert4 x (h:t) = if x > h then h: insert4 x t else x:h:t


--26
		
unwords3 :: [String] -> String
unwords3 [] = ""
unwords3 (h:t) = h ++ (if null t then "" else " ") ++ unwords3 t

--27

unlines2 :: [String] -> String
unlines2 [] = ""
unlines2 (h:t) = h ++ (if null t then "" else "\n") ++ unlines2 t

--28

--pMaior2 :: Ord a => [a] -> Int
--pMaior2 [_] = []
--pMaior2 l1 = aux 0 0 0 l1
--		where
--		aux _ _ [] = []
--		aux i x y l1 = if x < (head l1) 
--					 then aux (i+1)
	


--29

temRepetidos2 :: Eq a => [a] -> Bool
temRepetidos2 [] = False
temRepetidos2 l1 = if (head l1) `elem` (tail l1) 
				   then True
				   else temRepetidos2 (tail l1)


--30
	
algarismos2 :: [Char] -> [Char]
algarismos2 [] = []
algarismos2 l1 = aux ['0'..'9'] l1
                 where
				 aux _ [] = []
				 aux listAux l1 = if (head l1) `elem` listAux 
								  then (head l1): aux (tail l1) listAux
								  else aux (tail l1) listAux
								  

--31


posImpares2 :: [a] -> [a]
posImpares2 [] = []
posImpares2 l1 = aux 0 l1
              where
			  aux _[] = []
			  aux i l1 = if i==1 
						 then (head l1):aux (i-1) (tail l1)
						 else aux (i+1) (tail l1)
						 
--31.1

posImpares3 ::  [a] -> [a]
posImpares3 [] = []
posImpares3 [_] = []
posImpares3 (h:s:t) = s:posImpares3 t						 
		

		
--32

posPares2 :: [a] -> [a]
posPares2 [] = []
posPares2 l1 = aux 0 l1
              where
			  aux _[] = []
			  aux i l1 = if i==0 
						 then (head l1):aux (i+1) (tail l1)
						 else aux (i-1) (tail l1)

--33						 

isSorted2 :: Ord a => [a] -> Bool
isSorted2 [] = True
isSorted2 (h:s:t) = if s>=h 
					then if null t 
						then True 
						else isSorted2 (s:t) 	
					else False
					
--34

iSort2 :: Ord a => [a] -> [a]
iSort2 [] = []
iSort2 (h:t) = insert2 h (iSort2 t)

--35

menor2 :: String -> String -> Bool
menor2 l1 "" = False
menor2 "" l2 = True
menor2 l1 l2 = if (head l1) < (head l2) 
			   then True
			   else menor2 (tail l1) (tail l2)
			   
--36

elemMSet2 :: Eq a => a -> [(a,Int)] -> Bool
elemMSet2 _ [] = False
elemMSet2 n ((x,y):t) = if n == x 
						then True
						else elemMSet2 n t
						
--37

lengthMSet2 :: [(a,Int)] -> Int	
lengthMSet2 [] = 0
lengthMSet2 ((x,y):t) = aux 0 ((x,y):t)
						where
						aux  _ [] = 0
						aux n1 ((x,y):t) =if null t  
										  then n1 
										  else aux (n1 + y) t
										  
--38

converteMSet2 :: [(a,Int)] -> [a]
converteMSet2 [] = []
converteMSet2 ((x,y):t) = if y /= 0 
						  then x:converteMSet2 ((x,y-1):t)
						  else converteMSet2 t
						  
--39

insereMSet2 :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet2 _ []=[]
insereMSet2 c ((x,y):t) = if c == x 
						then (x,y+1):insereMSet2 c t
						else (x,y):insereMSet2 c t
						

--40

removeMSet2 :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet2 _ []=[]
removeMSet2 c ((x,y):t) =if c == x 
						then removeMSet2 c t
						else (x,y):removeMSet2 c t
						
--41

constroiMSet2 :: Ord a => [a] -> [(a,Int)]
constroiMSet2 [] = []
constroiMSet2 l1 = aux 1 l1
				   where
				   aux i [x] = [(x,i)]
				   aux i (h:s:t)= if h == s 
								  then aux (i+1) (s:t)
								  else (h,i):aux 1 (s:t)
						
--42

mypartitionEithers :: [Either a b] -> ([a],[b])
mypartitionEithers l = (left l, right l)
                    where
                      left  (Left a :t)   = a : left t
                      left  (Right b :t)  = left t
                      left _  = []
                      right (Left a :t)   = right t
                      right (Right a :t)  = a : right t
                      right _ = []

--43

catMaybes :: [Maybe a] -> [a]		
catMaybes [] = []
catMaybes (m:ms) = case m of Nothing -> catMaybes ms
                             Just x -> x:catMaybes ms
							 
--44

data Movimento = Norte | Sul | Este | Oeste
				deriving Show
						
posicao2 :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao2 (x,y) [] = (x,y)
posicao2 (x,y) (h:t) = posicao2 (case h of Norte -> (x, y + 1)
                                           Sul -> (x, y - 1)
                                           Este -> (x + 1, y)
                                           Oeste -> (x - 1, y)) t


--45


caminho2 :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho2 (xi, yi) (xf, yf) | xi < xf   = Este : caminho2 (xi + 1, yi) (xf, yf)
                          | xi > xf   = Oeste : caminho2 (xi - 1, yi) (xf, yf)
                          | yi < yf   = Norte : caminho2 (xi, yi + 1) (xf, yf)
                          | yi > yf   = Sul : caminho2 (xi, yi - 1) (xf, yf)
                          | otherwise = []


--46										   

vertical2 :: [Movimento] -> Bool
vertical2 [] = True
vertical2 (h:t) = case h of Este -> False
                            Oeste -> False
                            _ -> vertical2 t


-- 47
							
data Posicao = Pos Int Int
               deriving Show

maisCentral2 :: [Posicao] -> Posicao
maisCentral2 [x] = x
maisCentral2 (x:y:t) | aux x <= aux y = maisCentral2 (x:t)
                    | aux x >  aux y = maisCentral2 (y:t)
                  where
                    aux (Pos k m) = sqrt(fromIntegral(k^2+m^2))



--48 


vizinhos2 :: Posicao -> [Posicao] -> [Posicao]
vizinhos2 _ [] = []
vizinhos2 (Pos x y) ((Pos z w):t) = if (y == w && x == (z+1)) || (y == w && x == (z-1)) || (x == z && y == (w+1)) || (x == z && y == (w-1))
                                   then (Pos z w):vizinhos2 (Pos x y) t
                                   else vizinhos2 (Pos x y) t


--49 


mesmaOrdenada2 :: [Posicao] -> Bool
mesmaOrdenada2 [x] = True
mesmaOrdenada2 ((Pos x y):(Pos z w):t) = if y==w
                                        then mesmaOrdenada2 ((Pos z w):t)
                                        else False


--50 


data Semaforo = Verde | Amarelo | Vermelho
                deriving Show

interseccaoOK :: [Semaforo] -> Bool
interseccaoOK l = (aux l)<=1
              where
                aux [Vermelho] = 0
                aux [Verde] = 1
                aux [Amarelo] = 1
                aux (Vermelho:resto) = aux resto
                aux (Verde:resto) = 1+aux resto
                aux (Amarelo:resto) = 1+aux resto
