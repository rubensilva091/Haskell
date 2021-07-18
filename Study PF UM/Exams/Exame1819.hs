
--1

isSorted :: (Ord a) => [a] -> Bool
isSorted [h] = True
isSorted (h:s:t) = if h<s then isSorted (s:t) else False

inits2 :: [a] -> [[a]]
inits2 [] = [[]]
inits2 l = inits2 (init l) ++ [l]

--2 
 
 
--3

data LTree a = Tip a | Fork (LTree a) (LTree a)

--sabia fazer


--4 

maxSumInit 