-- Pedro Henrique Alves Barbosa - 2022.1.08.043


import Data.Char


anoNormal :: Int -> Int
anoNormal x
	|x == 1 = 31
	|x == 2 = 28
	|x == 3 = 31
	|x == 4 = 30
	|x == 5 = 31
	|x == 6 = 30
	|x == 7 = 31
	|x == 8 = 31
	|x == 9 = 30
	|x == 10 = 31
	|x == 11 = 30
	|x == 12 = 31
		
anoBissexto :: Int -> Int
anoBissexto x
	|x == 1 = 31
	|x == 2 = 29
	|x == 3 = 31
	|x == 4 = 30
	|x == 5 = 31
	|x == 6 = 30
	|x == 7 = 31
	|x == 8 = 31
	|x == 9 = 30
	|x == 10 = 31
	|x == 11 = 30
	|x == 12 = 31


-- Questão 1
aux :: Int -> Int -> [Int] -> Bool
aux 2 x [] = True
aux y x [] = False 
aux y x (a:b) 
         | x `mod` a /= 0 = aux y x b  
         | otherwise = aux (y+1) x b
         
ehprimo :: Int -> Bool
ehprimo x = aux 0 x [1,2 .. x] 


-- Questão 2
ordenaEmTupla :: Int -> Int -> Int -> Int -> (Int, Int, Int, Int)
ordenaEmTupla a b c d 
  |a > b = ordenaEmTupla b a c d
  |b > c = ordenaEmTupla a c b d
  |c > d = ordenaEmTupla a b d c
  |otherwise = (a,b,c,d)


-- Questão 3
quantosDias :: Int -> Int
quantosDias a
  |(a `mod` 4) == 0 && (a `mod` 100) /= 0 || (a `mod` 400) == 0 = 366
  |otherwise = 365
  
  
-- Questão 4  
diasMes :: Int -> Int -> Int
diasMes x y 
  |quantosDias x == 365 = anoNormal y
  |otherwise = anoBissexto y 
  
  
-- Questão 5
dia :: Int -> Int -> Int -> Int
dia _ 1 z = z
dia x y z 
  |quantosDias x == 365 = z + dia x (y-1) (anoNormal (y-1) )
  |quantosDias x == 366 = z + dia x (y-1) (anoBissexto (y-1) )
  |otherwise = -1


-- Questão 6
aux1 :: [Int]->Int->Int
aux1 [] b = b 
aux1 (a:x) b
  |a>b = aux1 x a
  |otherwise = aux1 x b
  
aux2 :: [Int]->Int->Int
aux2 [] b = b 
aux2 (a:x) b
  |a<b = aux2 x a
  |otherwise = aux2 x b
  
maiormenor :: [Int] -> (Int,Int)
maiormenor [] = (0,0)
maiormenor (a:x) = (aux2 x a, aux1 x a)


-- Questão 7
insereOrdenado :: Int -> [Int] -> [Int]
insereOrdenado b [] = [b]
insereOrdenado b (a:x)
  |b <= a = b:a:x
  |otherwise = a:insereOrdenado b x
  
ordena::[Int]->[Int]
ordena [] = []
ordena (a:x) = insereOrdenado a (ordena x)


-- Questão 8
repete :: Int -> Int -> [Int]
repete 0 _ = []
repete x y = y : repete (x-1) y

repeteElemento :: [Int] -> [Int]
repeteElemento [] = []
repeteElemento (x:xs) = (repete x x) ++ repeteElemento xs


-- Questão 9
serie:: Int -> Int -> Int
serie x 1 = 1 `div` x
serie x y
  |(y `mod` 2) == 0 = (x `div` y) + serie x (y-1)
  |otherwise = y`div`x + serie x (y-1)
