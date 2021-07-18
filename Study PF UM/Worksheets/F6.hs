import Data.List
import System.IO
import Data.Char

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show
 
tree= Node 20 (Node 15 (Node 10 Empty Empty) (Node 17 Empty Empty)) 
                  (Node 25 Empty Empty)
				  
				  
				  				  
--1
				  
altura :: BTree a -> Int
altura (Empty) = 0
altura (Node a l r) = 1+ max (altura l) (altura r)


contaNodos :: BTree a -> Int
contaNodos (Empty) = 0
contaNodos (Node a l r) = 1 + (contaNodos l) + (contaNodos r) 

folhas :: BTree a  -> Int
folhas (Node _ (Empty) (Empty)) = 1
folhas (Node a l r) = (folhas l) + (folhas r)

prune :: Int -> BTree a -> BTree a
prune _ (Empty) = (Empty)
prune n (Node a l r) = if n==0 
					   then (Empty)
					   else Node a (prune (n-1) l) (prune (n-1) r)
					   
path :: [Bool] -> BTree a -> [a]
path _ (Empty) = []
path (h:t) (Node a l r) = if h == True
						  then a: path t r
						  else a: path t l
						  
mirror :: BTree a -> BTree a 
mirror (Empty)  = (Empty)
mirror (Node a l r) = Node a (mirror r) (mirror l)


zipWithBT :: (a->b->c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node a1 l1 r1) (Node a2 l2 r2) = Node (f a1 a2) (zipWithBT f l1 l2) (zipWithBT f r1 r2)
zipWithBT _ _ _ = Empty

--unzipBT :: Btree (a->b->c) -> (BTree a, BTree b, BTree c)


--2

minimo :: Ord a => BTree a -> a
minimo (Node e Empty _) = e
minimo (Node e l r) = minimo l


semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node a Empty _) = Empty
semMinimo (Node a l r) = Node a (semMinimo l) (semMinimo r)


minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node e Empty _) = (e,Empty)
minSmin (Node e l r) = (a,Node e b r)
    where (a,b) = minSmin l


--Nao percebi este, portanto vai asism, foda-se
remove :: Ord a => a -> BTree a -> BTree a
remove n (Empty) = (Empty)
remove n (Node a l r) = if n == a then Empty else Node a (remove n l) (remove n r) 



--3

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
					| Rep
					| Faltou
					deriving Show
type Turma = BTree Aluno -- arvore binaria de procura (ordenada por numero)


--Arvore de uma Turma
----------------------------------------------------------------------------------------------------------------------------------------------- |
turma1 :: Turma                                                                                                                              -- |
                                                                                                                                             -- |
turma1 = (Node (15,"Luís",ORD,Aprov 14) (Node (12,"Joana",MEL,Faltou) (Node (7,"Diogo",TE,Rep) Empty                                         -- |
                                                                                               Empty)                                        -- |
                                                                      (Node (14,"Lara",ORD,Aprov 19) Empty                                   -- |
                                                                                                     Empty))                                 -- |
                                        (Node (20,"Pedro",TE,Aprov 10) Empty                                                                 -- |
                                                                       (Node (25,"Sofia",ORD,Aprov 20) (Node (23,"Rita",ORD,Aprov 17) Empty  -- |
                                                                                                                                      Empty) -- |
                                                                                                       (Node (28,"Vasco",MEL,Rep) Empty      -- |
                                                                                                                                  Empty))))  -- |
----------------------------------------------------------------------------------------------------------------------------------------------- |

inscNum :: Numero -> Turma -> Bool
inscNum x (Empty) = False
inscNum x (Node (n,_,_,_) l r) = if n == x 
								 then True
								 else if n>x
									  then (inscNum x r)
									  else (inscNum x l) 
									  
									  
inscNome :: Nome -> Turma -> Bool
inscNome name (Empty) = False
inscNome name (Node (_,n,_,_) l r) = if name == n || inscNome name l || inscNome name r
									 then True
									 else False

trabEst :: Turma -> [(Numero,Nome)]
trabEst (Empty) = []
trabEst (Node (num,name,reg,_) l r) = (case reg of TE -> [(num,name)];
												   otherwise->[]) ++ trabEst l ++ trabEst r

nota :: Numero -> Turma -> Maybe Classificacao
nota n (Node (num,_,_,clas) l r) | n == num = Just clas
                                       | n < num = nota n l
                                       | otherwise = nota n r
									   
--percFaltas :: Turma -> Float
--percFaltas x = aux x 0 0
--			   where
--			   aux (Empty) fav total = fav `div` total
--			   aux (Node (_,_,_,clas) l r) fav total = case clas of Faltou -> if l == (Empty) then (aux r (fav+1) (total+1)) else (aux l (fav+1) (total+1));
--																    otherwise -> if l == (Empty) then (aux r (fav) (total+1)) else (aux l (fav) (total+1)) (aux l fav total)


							
										












