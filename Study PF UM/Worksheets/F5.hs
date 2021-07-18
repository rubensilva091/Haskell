import Data.List
import System.IO
import Data.Char

--1

any2 :: (a -> Bool) -> [a] -> Bool
any2 f [] = False
any2 f (h:t) = if f h == True 
			   then True
			   else any2 f t
			   
			   
zipWith2 :: (a->b->c) -> [a] -> [b] -> [c]
zipWith2 f [] _ = []
zipWith2 f _ [] = []
zipWith2 f (h1:t1) (h2:t2) = (f h1 h2):zipWith2 f t1 t2


takeWhile2 :: (a->Bool) -> [a] -> [a]
takeWhile2 f [] = []
takeWhile2 f (h:t) = if f h == True
					 then h:takeWhile2 f t
					 else []
					 

dropWhile2 :: (a->Bool) -> [a] -> [a]
dropWhile2 f [] = []
dropWhile2 f l1 = if f (head l1) == True
				  then dropWhile2 f (tail l1)
				  else l1
				  

--Um bocado javardo mas funfa
span2 :: (a-> Bool) -> [a] -> ([a],[a])
span2 f l1 = aux f l1 [] [] 0
			 where 
			 aux f [] _ _ _ = ([],[])
			 aux f _ take drop 1 = (take,drop)
			 aux f (h:t) take drop n = if f h == True && n == 0
								       then aux f t (take ++ [h]) drop n
							           else aux f t take (drop ++ t) 1
									   
									   
deleteBy2 :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy2 f n l1 = aux f n l1 0 []
                   where
				   aux f _ [] _ l2 = l2			   
				   aux f n (h:t) x l2 = if f n h && x == 0
								        then aux f n t 1 l2
									    else aux f n t x (l2++[h])			

--nao percebi e copiei da Eng

sortOn2 :: (Ord b) => (a -> b) -> [a] -> [a]
sortOn2 f [] = []
sortOn2 f (h:t) = insere (h) (sortOn2 f t)
    where insere x [] = [x]
          insere x (a:b) = if f x > f a then a:insere x b else x:a:b

--2
--Por exemplo, [(2,3), (3,4), (5,3), (4,5)] representa o polinomio: 
--2x^3 + 3x^4 + 5x^3 + 4x^5
type Polinomio = [Monomio]
type Monomio = (Float,Int)

selgrau :: Int -> Polinomio -> Polinomio
selgrau n l1 = filter (\x-> snd x==n) l1

conta :: Int -> Polinomio -> Int
conta n l1 = length $ filter (\x-> snd x==n) l1

--acc quer dizer que é o counter      e o '0' e 'l' correspondem respetivamente a acc e snd x
grau :: Polinomio -> Int
grau l = foldl (\acc x -> if acc > snd x then acc else snd x) 0 l

--nao sei derivar na programacao, fck it
--deriv :: Polinomio -> Polinomio
--deriv l

calcula :: Float -> Polinomio -> Float
calcula n lista = sum [ a*(n^b) | (a, b) <- lista]

simp :: Polinomio -> Polinomio
simp lista = [ (a, b) | (a, b) <- lista, a /= 0]

mult :: Monomio -> Polinomio -> Polinomio
mult (x,y) lista = [ (x*a, y*b) | (a, b) <- lista]

ordena :: Polinomio -> Polinomio
ordena lista = sortOn2 snd lista

normaliza :: Polinomio -> Polinomio
normaliza lista = let x = fromIntegral $ grau lista in [ ((a/x),b) | (a,b) <- lista]

soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza (p1 ++ p2)

produto :: Polinomio -> Polinomio -> Polinomio
produto p1 p2 = normaliza $ concat [ mult (a, b) p2 | (a, b) <- p1]

--desisto desta merda, funcoes de ordem superior sucks, sry, tru fact


--3
--tao mais facil fds

type Mat a = [[a]]

dimOK :: Mat a -> Bool
dimOK [h] = True
dimOK (h:s:t) = if length h == length s 
				then dimOK (s:t) 
				else False
				
dimMat :: Mat a -> (Int,Int)
dimMat l = aux l 0 0 
		   where 
		   aux [] x y = (x,y)
		   aux l x y = aux (tail l) (length (head l)) (y+1)
		   
		   
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat [] _ = []
addMat _ [] = []
addMat l1 l2 = (zipWith (+)(head l1) (head l2)): addMat (tail l1) (tail l2)

--nao sei mesmo como fazer, que moca, desisti
--transpose :: Mat a -> a
--transpose (h:t) = aux (h:t) (h:t) (length h) (length h) (length h) 0
--			      where
--			      aux ((x:xs):t) l2 save c j z = if z==1 
--									             then aux l2 l2 j c j 0
--									             else if save /= c
--												      then aux ((xs):t) l2 (save-1) c j 0
--												      else x:aux t l2 save (c-1) j 1 
									    

 
		   
--multMat :: Num a => Mat a -> Mat a -> Mat a

		        
  

