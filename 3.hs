--Ex. 1
retornaNElementos :: [a] -> Int -> [a]
retornaNElementos _ 0 = []
retornaNElementos [] _ = []
retornaNElementos (a:b) n = a : retornaNElementos b (n - 1)

--Ex. 2
inverteLista :: [Int] -> [Int]
inverteLista [] = []
inverteLista (a:x) = inverteLista x ++ [a]

--Ex. 3
agrupaListas :: [a] -> [b] -> [(a, b)]
agrupaListas [] _ = []  
agrupaListas _ [] = []  
agrupaListas (a:b) (c:d) = (a, c) : agrupaListas b d

--Ex. 4
maiorValor :: (Ord a) => [a] -> a
maiorValor [a] = a  
maiorValor (a:b) = max a (maiorValor b)
 
--Ex. 5
adicionaSemRepeticao:: (Ord a) => [a] -> a -> [a]
adicionaSemRepeticao lista elemento
    | elemento `elem` lista = lista
    | otherwise = adicionarOrdenado lista elemento
  where
    adicionarOrdenado :: (Ord a) => [a] -> a -> [a]
    adicionarOrdenado listaOrd elementoOrd = inserirOrd elementoOrd listaOrd
      where
        inserirOrd :: (Ord a) => a -> [a] -> [a]
        inserirOrd novo [] = [novo]
        inserirOrd novo (cabecaCauda:resto)
          | novo <= cabecaCauda    = novo : cabecaCauda : resto
          | otherwise = cabecaCauda : inserirOrd novo resto