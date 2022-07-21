{-
Escreva a funcao ocorreUma :: [Int] -> [Int]  em Haskell, que deve retornar uma lista 
com os números inteiros que aparecem apenas uma vez na lista passada como parâmetro. 
Ex: ocorreUma [4,1,5,4,3,5]  deve retornar [1,3] .
-}

notRepete :: [Int] -> Int -> Bool
notRepete xs a = if length (filter (==a) xs) > 1 then False else True

ocorreUma :: [Int] -> [Int]
ocorreUma xs = filter (notRepete xs) xs
