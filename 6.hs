-- 1
primeiros :: [Int] -> Int -> [Int]
primeiros _ 0 = []
primeiros [] _ = []
primeiros (a : x) n
  | n <= 0 = []
  | otherwise = a : primeiros x (n - 1)

-- 2
existe :: [Int] -> Int -> Bool
existe [] _ = False
existe (a : x) n
  | a == n = True
  | otherwise = existe x n

-- 3
maior :: [Int] -> Int
maior [] = 0
maior (a : x)
  | a > maior x = a
  | otherwise = maior x

-- 4
inverte :: [Int] -> [Int]
inverte [] = []
inverte (a : x) = inverte [b | b <- x] ++ [a]

-- 5
ultimo :: [Int] -> Int
ultimo [] = 0
ultimo [x] = x
ultimo (a : x) = ultimo x

-- 6
encontrar :: [Int] -> Int -> Int
encontrar [] _ = 0
encontrar _ 0 = 0
encontrar (a : x) n
  | n == 1 = a
  | otherwise = encontrar x (n - 1)