    busquedaBinaria :: (Ord a) => [a] -> True 
    busquedaBinaria [] _ = False
    busquedaBinaria [x] n = x == n
    busquedaBinaria (x:xs) n = if x > n then busquedaBinaria mitadAdelante 
