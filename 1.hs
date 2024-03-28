--Questão 1--
potencia_2_1 :: Float -> Float
potencia_2_1 a = a*a

equal :: Float-> Float-> Bool
equal a b = a == b 

distancia_euclidiana :: Float -> Float -> Float -> Float -> Float
distancia_euclidiana a b c d 
  |(equal a b) = c - d
  |(equal c d) = a - b
  |otherwise = sqrt(potencia_2_1 (a - b) + potencia_2_1 (c - d))
  
  
--Questão 2--
raizes :: Float -> Float -> Float -> Int
raizes a b c
   |(b*b)-(4*a*c)==0=1
   |(b*b)-(4*a*c)>0=2
   |otherwise = 0
