
--1

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices n l = aux n l 1
				  where
				  aux _ [] _ = []
				  aux n (h:t) i = if n == h
								  then i:aux n t (i+1)
								  else aux n t (i+1)
							
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (h1:t1) (h2:t2) = if h1==h2
								  then isSubsequenceOf t1 t2
								  else isSubsequenceOf (h1:t1) t2
								  
--2 

data BTree a = Empty | Node a (BTree a) (BTree a)

--lookupAP :: Ord a => a -> BTree (a,b) -> Maybe b



zipWithBt :: (a-> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBt f (Node a l1 r1) (Node b l2 r2) = Node (f a b) (zipWithBt f l1 l2) (zipWithBt f r1 r2)
zipWithBt _ _ _ = Empty

--3

digitAlpha :: String -> (String,String)
digitAlpha (h:t) = aux (h:t) (" ") (" ")
				   where
				   aux [] digit alpha = (digit,alpha)
				   aux (h:t) digit alpha = if h `elem` ['0'..'9']
						                   then aux t (digit ++ [h]) alpha
				                           else if h `elem` ['a'..'z'] || h `elem` ['A'..'Z']
						                        then aux t digit (alpha ++ [h])
						                        else aux t digit alpha
												
--4

data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq b)

firstSeq :: Seq a -> a
firstSeq (Cons a s) -> a
firstSeq (App Nil s) = firstSeq s
firstSeq (App s _) = firstSeq s

dropSeq :: Int -> Seq a -> Seq a
dropSeq

