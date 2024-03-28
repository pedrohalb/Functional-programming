isEven :: Int -> Bool
isEven x = x `mod` 2 == 0


maxi2 :: Int -> Int -> Int
maxi2 x y
  |x>y =  x
  |otherwise =  y
  
nonEmpty :: String -> Bool
nonEmpty x = x /= "" 

------------------------------------------------------------------------------------------------------------------------------------

foldrr1 :: (a -> a -> a) -> [a] -> a
foldrr1 f [a] = a
foldrr1 f (a:b:x) = f a (foldrr1 f (b:x)) 

mapp :: (u -> t) -> [u] -> [t]
mapp f [] = []
mapp f (a:x) = f a : mapp f x

filterr :: (u->Bool) -> [u] -> [u]
filterr f [] = []
filterr f (a:x)
  |f a = a:filterr f x
  |otherwise = filterr f x
  
------------------------------------------------------------------------------------------------------------------------------------

insereOrdenado :: Int -> [Int] -> [Int]
insereOrdenado x [] = [x]
insereOrdenado x (a:xs)
  |x>=a=a:insereOrdenado x xs
  |otherwise = x:insereOrdenado a xs
  
ordena :: [Int] -> [Int]
ordena [] =[]
ordena (a:x) = insereOrdenado a (ordena x)

------------------------------------------------------------------------------------------------------------------------------------

repete::Int->Int->[Int]
repete 0 _ = []
repete x y = y : repete (x-1) y

repeteElemento :: [Int] -> [Int]
repeteElemento [] = []
repeteElemento (a:x) = (repete a a) ++ repeteElemento x

------------------------------------------------------------------------------------------------------------------------------------

contarLetra :: Char -> String -> Int
contarLetra x [] = 1
contarLetra x (a:xs)
  |x == a = 1 + contarLetra x xs
  |otherwise = contarLetra x xs

contaString :: String -> [(Char,Int)]
contaString [] = []
contaString (a:x) = (a, contarLetra a x) : contaString x

------------------------------------------------------------------------------------------------------------------------------------
pos::[Int]->Int->Int
pos [] 0 = 0
pos (a:x) y
  |a==y = 0
  |otherwise = 1+ pos x y

maior::[Int]->Int
maior [] = 0
maior (a:x) 
  |a > maior x = a 
  |otherwise = maior x

tupla2 :: [Int] -> [(Int,Int)]
tupla2 x = [(maior x, pos x (maior x))]		


remove::[Int]->Int->[Int]
remove [] y = []
remove (a:x) y
  |pos (a:x) a == y = x
  |otherwise = a: remove x (y-1)
  
  
filterrr :: (u -> Bool) ->[u]-> [u]
filterrr f [] = [] 
filterrr f (a:x) 
  |f a = a : filterrr f x
  |otherwise = filterrr f x
  
mappp :: (u -> t) -> [u] -> [t]
mappp f [] = []
mappp f (a:x) = f a : mappp f x

foldr3 :: (a->a->a) -> [a] -> a
foldr3 f [a] = a
foldr3 f (a:b:x) = f a (foldr3 f (b:x))

sumDouble :: Int -> Int
sumDouble x = x * 2

--aux2 :: [Int] -> [Int] 
--aux2 (a:x) = map(sumDouble a): aux2 x


contarLetras :: Char -> String -> Int 
contarLetras x [] = 1
contarLetras x (a:xs) 
  |x==a = 1+ contarLetras x xs
  |otherwise = contarLetras x xs
  
contarString :: String -> [(Char,Int)]
contarString "" = []
contarString (a:x) = (a, contarLetras a x) : contarString x
