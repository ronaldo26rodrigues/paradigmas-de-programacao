{-
Assumindo que os dados de vacina para o COVID-19 estejam em um arquivo "vacina.txt" e que 
as linhas representam dados das pessoas que participam do estudo e estão estruturadas da seguinte forma:

[id];[idade];[placebo];[reação]

onde [id] representa um inteiro identificando uma pessoa, [idade] informa a idade da pessoa, 
[placebo] caso seja "true" indica que a vacina tomada é um placebo, caso seja "false" indica que 
realmente foi a vacina sendo testada, e [reação] pode ser "nenhuma" caso não tenha tido reação, 
"leve" caso tenha tido algum sintoma leve, "forte" caso tenha sentido algum sintoma forte.

Crie um programa em Haskell deve ler este arquivo e imprimir na saída padrão as seguintes informações:

- A razão de pessoas que tomaram placebo;

- A razão de pessoas que não tomaram placebo;

- A razão de pessoas que tomaram a vacina (não foi placebo) não tiveram reação;

- Quantidade de pessoas acima de 50 anos que tomaram a vacina (não foi placebo) e tiveram algum tipo de reação;
-}

splitPontoVirgula :: String -> [String]
splitPontoVirgula a = words [if c == ';' then ' ' else c|c <- a]

nPLacebo :: Int -> [[String]] -> Int
nPLacebo n [] = n
nPLacebo n (a:as) = if a !! 2 == "true" then nPLacebo (n+1) as else nPLacebo n as

nReacao :: Int -> [[String]] -> Int
nReacao n [] = n
nReacao n (a:as) = if a !! 2 == "false" && a !! 3 == "false" then nReacao (n+1) as else nReacao n as

reacao50 :: Int -> [[String]] -> Int
reacao50 n [] = n
reacao50 n (a:as) = if a !! 2 == "false" && a !! 3 == "true" && read (a !! 1) >= 50  then reacao50 (n+1) as else reacao50 n as

analise :: [String] -> IO ()
analise a = do putStr "Razão de pessoas que tomaram placebo: "
               print ( fromIntegral(nPLacebo 0 (map splitPontoVirgula a)) / fromIntegral (length a))
               putStr "Razão de pessoas que não tomaram placebo: "
               print (1-( fromIntegral(nPLacebo 0 (map splitPontoVirgula a)) / fromIntegral (length a)))
               putStr "Razão de pessoas que tomaram a vacina (não foi placebo) não tiveram reação: "
               print (fromIntegral(nReacao 0 (map splitPontoVirgula a)) / fromIntegral (length a))
               putStr "Quantidade de pessoas acima de 50 anos que tomaram a vacina (não foi placebo) e tiveram algum tipo de reação: "
               print (reacao50 0 (map splitPontoVirgula a))

vacina :: IO ()
vacina = do a <- readFile "Semana6/vacina.txt"
            analise (lines a)
