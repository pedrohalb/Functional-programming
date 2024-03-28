-- 1
isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

-- 2
maxi2 :: Int -> Int -> Int
maxi2 x y
  | x > y = x
  | otherwise = y

-- 3
nonEmpty :: String -> Bool
nonEmpty x = length x /= 0