
-- 1

data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt

calcula :: ExpInt -> Int
calcula (Const num) = num
calcula (Simetrico n) = 0-(calcula n)
calcula (Mais a b) = (calcula a) + (calcula b)
calcula (Menos a b) = (calcula a) - (calcula b)
calcula (Mult a b) = (calcula a) * (calcula b)


infixa :: ExpInt -> String
infixa (Const num) = show num
infixa (Simetrico num) = "(-" ++ infixa num ++ ")"
infixa (Mais a b) = "(" ++ infixa a ++ "+" ++ infixa b ++ ")"
infixa (Menos a b) = "(" ++ infixa a ++ "-" ++ infixa b ++ ")"
infixa (Mult a b) = "(" ++ infixa a ++ "*" ++ infixa b ++ ")"


posfixa :: ExpInt -> String
posfixa (Const num) = show num
posfixa (Simetrico num) = "-"++posfixa num
posfixa (Mais a b) = posfixa a ++ posfixa b ++ "+"
posfixa (Menos a b) = posfixa a ++ posfixa b ++ "-"
posfixa (Mult a b) = posfixa a ++ posfixa b ++ "*"


--2

data RTree a = R a [RTree a]

rtree1 = R 6 [R 4 [R 7 [R 1 [],
                        R 3 []],
                   R 9 []],
              R 3 [R 12 []],
              R 6 [],
              R 11 []]
			  
			  
			  
soma :: Num a => RTree a -> a
soma (R e []) = e
soma (R e es ) = e + sum (map soma es)

altura :: RTree a -> Int
altura (R e []) = 1
altura (R e es) = 1 + maximum (map altura es)

prune :: Int -> RTree a -> RTree a
prune 0 (R e es) = R e []
prune n (R e es) = R e (map (prune (n-1)) es)

mirror :: RTree a -> RTree a
mirror (R e es) = R e (map mirror (reverse es))

postorder :: RTree a -> [a]
postorder (R e []) = [e]
postorder (R e es) = concatMap postorder  es ++ [e]

--3 

data LTree a = Tip a | Fork (LTree a) (LTree a)

ltree1 = Fork (Fork (Tip 5)
                    (Fork (Tip 6)
                          (Tip 4)))
              (Fork (Fork (Tip 3)
                          (Tip 7))
                    (Tip 5))


ltSum :: Num a => LTree a -> a
ltSum (Tip n) = n
ltSum (Fork l r) = 0 + (ltSum l) + (ltSum r)

listaLT :: LTree a -> [a]
listaLT (Tip n) = [n]
listaLT (Fork l r) = (listaLT l) ++ (listaLT r)

ltHeight :: LTree a -> Int
ltHeight (Tip n) = 0
ltHeight (Fork l r) = 1 + (max (ltHeight l) (ltHeight r))


--2.1

