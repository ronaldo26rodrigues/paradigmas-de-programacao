{-
Dada as funções "vendas" e "totalVendas" que utilizamos nas aulas desta semana (videoaula e slides),
 crie uma função "relatorio::Int -> (Int, Int, Int, Float)" 
 que retorna um relatório dos dados até uma determinada semana passada como parâmetro.
 O relatório é uma tupla onde 
 o primeiro elemento é o total de vendas até aquela semana,
 o segundo é o número da semana com mais vendas até aquela semana,
 o terceiro é a maior quantidade de vendas até aquela semana,
 e o quarto é a média de vendas até aquela semana. 
Dica: use funções intermediárias para calculcar o que se deseja. 
-}

vendas :: Int -> Int
vendas 0 = 15
vendas 1 = 20
vendas 2 = 30
vendas 3 = 42
vendas 4 = 8
vendas 5 = 18
vendas 6 = 24
vendas 7 = 21
vendas 8 = 55
vendas 9 = 32

maxi :: Int -> Int -> Int
maxi a b | a > b = a
		 | otherwise = b

maxVendas :: Int -> Int
maxVendas 0 = vendas 0
maxVendas n = maxi (maxVendas (n-1)) (vendas n)

semanaMaxVendas :: Int -> Int
semanaMaxVendas n | vendas n == maxVendas n = n
				  | otherwise = semanaMaxVendas (n-1)

totalVendas :: Int -> Int
totalVendas n | n == 0 = vendas 0
			  | otherwise = totalVendas (n-1) + vendas n

media :: Int -> Float
media n = (int(totalVendas n) / int(n))
 where int = fromIntegral

relatorio :: Int -> (Int, Int, Int, Float)
relatorio n = (totalVendas n, semanaMaxVendas n, maxVendas n, media n) 