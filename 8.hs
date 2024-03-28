-- Questão 1
divisores :: Int -> [Int]
divisores x = [a|a<-[1..(x-1)], x`mod`a == 0]

soma :: [Int] -> Int
soma [] = 0
soma (a:x) = a + soma x

ehPerfeito :: Int -> Bool
ehPerfeito x = x == (soma (divisores x))
  

-- Questão 2 
contarLetra :: Char -> String -> Int
contarLetra a [] = 1
contarLetra a (b:x)
  | a == b = contarLetra a x + 1
  | otherwise  = contarLetra a x

contaString :: String -> [(Char, Int)]
contaString "" = []
contaString (a:b) = (a, contarLetra a b) : contaString b


-- Questão 3
inverte :: String -> String
inverte [] = []
inverte (a:x) = inverte x ++ [a]


-- Questão 4
-- FUNÇÃO_DE_GENERALIZAÇÃO : map
squares :: Int -> Int
squares x = x*x


-- Questão 5
cartesiano :: [Int] -> [Int] -> [(Int, Int)]
cartesiano x y = [(a, b) | a <- x, b <- y] ++ [(a, b) | a <- y, b <- x]


-- Questão 6
-- FUNÇÃO_DE_GENERALIZAÇÃO : filter
positives :: Int -> Bool
positives a = a > 0


-- Questão 7 
-- foldr1(+) (map(*2) [1,-2,3])

sumDoubleExtra :: [Int] -> Int
sumDoubleExtra [] = 0
sumDoubleExtra (a:x) = (a*2) + sumDoubleExtra x 


-- Questão 8
-- FUNÇÃO_DE_GENERALIZAÇÃO : foldr1
concatena :: String -> String -> String
concatena [] [] = []
concatena x y = x++y
