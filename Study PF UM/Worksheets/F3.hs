module Ficha03 where

import F1 as F1

--1

-- data Hora = H Int Int deriving Show           Estou a usar a Ficha 1 como supp

type Etapa = (F1.Hora2,F1.Hora2)
type Viagem = [Etapa]


etapaValida :: Etapa -> Bool
etapaValida (x,y) = if F1.testHora2 x && F1.testHora2 y && y `F1.compHora2` x 
					then True 
					else False


viagemValida :: Viagem -> Bool
viagemValida (x:[]) = etapaValida x
viagemValida ((x1,x2):(s1,s2):t)= if etapaValida (x1,x2) && etapaValida (s1,s2) && s1 `compHora2` x2
								  then viagemValida ((s1,s2):t)
					              else False
								  
								  
partidaChegada :: Viagem -> [F1.Hora2]   --SOU UM GENIO FDSSS
partidaChegada ((x1,x2):t) = aux ((x1,x2):t) 0
							where
							aux [] _ = []							
							aux  ((x1,x2):t) n = if n==0 || null t
												 then if null t 
													  then x2:aux t (n+1)
													  else x1:aux t (n+1)
												 else aux t (n+1)
												 

tempoEspera :: Viagem -> F1.Hora2
tempoEspera x = aux x 0
				where 
				aux (x:[]) tempo = convhoraHora2 tempo
				aux ((x1,x2):(y1,y2):t) tempo = aux ((y1,y2):t) (tempo+((convminHora2 y1)-(convminHora2 x2)))
				
				

tempoTotalViagem :: Viagem -> F1.Hora2   
tempoTotalViagem ((x1,x2):t) = aux ((x1,x2):t) 0 0 0
							   where
							   aux [] _ tempo1 tempo2 = convhoraHora2 (tempo2 - tempo1)						
							   aux  ((x1,x2):t) n tempo1 tempo2 = if n==0 || null t
														  then if null t 
													           then aux t (n+1) tempo1 (convminHora2 x2)
													           else aux t (n+1) (convminHora2 x1) tempo2
												          else aux t (n+1) tempo1 tempo2
														  
														  
														  
														  
--2

type Poligonal = [F1.Ponto]

												 
distanciaPoligonal :: Poligonal -> Double
distanciaPoligonal l = aux l 0
					where 
					aux [h] n = n
					aux (h:s:t) n = aux (s:t) (n + (dist h s))
									
--Nao entendi um caralho daqui pra frente sinceramente
									
--fechadoPoligonal :: Poligonal -> Bool
--fechadoPoligonal l1 = 


--3

data Contacto = Casa Integer
				| Trab Integer
				| Tlm Integer
				| Email String deriving Show
				
type Nome = String

type Agenda = [(Nome, [Contacto])]


acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome mail agenda = [(nome, [Email mail])] ++ agenda

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails nome [] = Nothing
verEmails nome ((name,[Email x]):t) = if nome == name 
								then Just [x] 
								else verEmails nome t
								
consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs (h:t) = case h of  Casa x -> x:consTelefs t
                              Trab x -> x:consTelefs t
                              Tlm x -> x:consTelefs t 
                              otherwise -> consTelefs t
							  
							  
casa :: Nome -> Agenda -> Maybe Integer
casa nome [(n,(c:cs))] = if nome == n then case c of Casa x -> Just x
                                                     otherwise -> casa nome [(n,cs)] 
                                      else Nothing
casa nome ((n,c):agenda) = if nome == n then casa nome [(n,c)] else casa nome agenda

--4 


type Dia = Int
type Mes = Int
type Ano = Int

data Data = D Dia Mes Ano deriving Show
type TabDN = [(Nome,Data)]

procura :: Nome -> TabDN -> Maybe Data
procura _ [] = Nothing
procura nome ((x,y):t) = if nome == x
						 then Just y
						 else procura nome t
						 
						 
idade :: Data -> Nome -> TabDN -> Maybe Int
idade d nome tab = if (procura nome tab) == Nothing 
				   then Nothing
				   else 			 
							  
								
				   