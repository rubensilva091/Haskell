import Data.List (nub)

--1

intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect (h:t) l1 = if h `elem` l1 then h:intersect t l1 else intersect t l1

tails :: [a] -> [[a]]
tails []= [[]]
tails l1 = l1:tails (tail l1)

--2

type ConjInt = [Intervalo]
type Intervalo = (Int,Int)

elems :: ConjInt -> [Int]
elems [] = []
elems ((a,b):t) = if a<b 
			      then a:elems (((a+1),b):t)
				  else a:elems t


geraconj :: [Int] -> ConjInt
geraconj l = aux l (head l)
			 where
			 aux [] _ = []
			 aux [h] x = if (x-h)== 0 then [] else [(x,h)]
			 aux (h:s:t) x = if (s-h) == 1 
							 then aux(s:t) x 
							 else (x,h):aux (s:t) s

--3

data Contacto = Casa Integer
 | Trab Integer
 | Tlm Integer
 | Email String
 deriving (Show)
type Nome = String
type Agenda = [(Nome, [Contacto])]


acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail _ _ [] = []
acrescEmail nome1 email ((nome2,contacto):t) =if nome1 == nome2 
											  then [(nome2, contacto++[Email email])]
											  else acrescEmail nome1 email t
											  
--verEmails :: Nome -> Agenda -> Maybe [String]
--verEmails _ [] = Nothing
--verEmails nome ((nome2,contacto):t) = if nome == nome2 
								      --then Just Email [email]
									  --else verEmails nome t


--consulta :: [Contacto] -> ([Integer],[String])
--consulta (h:t) = (case h of Casa x -> x;
						  --  Trab x -> x;
						    --Tlm x -> x;
						   -- otherwise -> consulta t
						   
						   
--4

data RTree a = R a [RTree a] deriving (Show, Eq)

paths :: RTree a -> [[a]]
paths (R e []) = []
paths (R e es) = aux (R e es) e
				 where
				 aux (R e es) z


