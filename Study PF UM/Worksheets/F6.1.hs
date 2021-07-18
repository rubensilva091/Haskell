import Data.List
import System.IO
import Data.Char

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show
 
tree= Node 20 (Node 15 (Node 10 Empty Empty) (Node 17 Empty Empty)) 
                  (Node 25 Empty Empty)
		

altura :: BTree a -> Int
altura Empty = 0
altura (Node a l r) = 1 + max (altura l) (altura r)

contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node a l r) = 1 + contaNodos l + contaNodos r

folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node _ Empty Empty) = 1
folhas (Node _ a b) = folhas a + folhas b


folhas2 :: BTree a -> Int
folhas2 Empty = 0
folhas2 (Node a Empty Empty) = 1
folhas2 (Node a l r) = folhas2 l + folhas2 r


prune :: Int -> BTree a -> BTree a
prune 0 (Node a l r) = Empty
prune _ (Empty) = Empty
prune n (Node a l r) = Node a (prune (n-1) l) (prune (n-1) r)

					
path :: [Bool] -> BTree a -> [a]
path _ (Empty) = []
path (h:t) (Node a l r) = if h == True then a:path t r else a:path t l

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node a l r) = Node a (mirror r) (mirror l)

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Empty) _ = Empty
zipWithBT f _ (Empty) = Empty
zipWithBT f (Node a1 l1 r1) (Node a2 l2 r2) = Node (f a1 a2) (zipWithBT f l1 l2) (zipWithBT f r1 r2)

unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty, Empty, Empty)
unzipBT (Node (a,b,c) l r) = (Node a unzipL1 unzipR1,Node b unzipL2 unzipR2,Node c unzipL3 unzipR3)
    where (unzipL1,unzipL2,unzipL3) = unzipBT l
          (unzipR1,unzipR2,unzipR3) = unzipBT r

--2

minimo :: Ord a => BTree a -> a
minimo (Node a Empty _) = a
minimo (Node a l r) = minimo l

semMinimo :: Ord a => BTree a -> BTree a 
semMinimo (Node a Empty _) = Empty
semMinimo (Node a l r) = Node a (semMinimo l) (semMinimo r)

remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty

			  
				  
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
inscNum n (Empty) = False
inscNum n (Node (num,name,reg,clas) l r) = if n==num || inscNum n l || inscNum n r then True else False
										   

inscNome :: Nome -> Turma -> Bool
inscNome name (Empty) = False
inscNome name (Node (_,n,_,_) l r) = if name==n || inscNome name l || inscNome name r then True else False


trabEst :: Turma -> [(Numero,Nome)]
trabEst (Node (num,name,reg,_) l r) = case reg of TE -> [(num,name)];
											      otherwise -> []++(trabEst l)++(trabEst r)
												  

nota :: Numero -> Turma -> Maybe Classificacao
nota _ Empty = Nothing
nota n (Node (num,_,_,clas) l r) = if n == num
								   then Just clas
								   else if n>num
										then nota n l
										else nota n r
										



