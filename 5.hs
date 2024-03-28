import Data.Char

inverteLista :: [x] -> [x]
inverteLista [] = []
inverteLista (a:x) = inverteLista x ++ [a]

duplasTuplas :: [a] -> [b] -> [(a, b)]
duplasTuplas [] [] = []               
duplasTuplas (x:xs) (y:ys) = (x, y) : duplasTuplas xs ys

digits :: String -> String
digits [] = []
digits (x:xs)
    | isDigit x = x : digits xs
    | otherwise = digits xs

letters :: String -> String
letters [] = []
letters (x:xs)
    | isLetter x = x : letters xs
    | otherwise = letters xs
