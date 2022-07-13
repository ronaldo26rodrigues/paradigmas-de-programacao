{-No sistema eleitoral americano quando um candidato a presidência
ganha a votação por maioria de um determinado estado, os votos dos delegados
daquele estado são contabilizados para aquele candidato.
Ganha a eleição quem possuir mais votos de delegados, 
o que não necessariamente representa a maioria dos votos dos eleitores no país. 
Crie uma função em Haskell para indicar o vencedor de uma eleição americana. 
Vamos simplificar utilizando apenas dez estados, os quais são listados abaixo 
de acordo com seus números de delegados:

1 - California = 55

2 - Texas = 38

3 - Florida = 29

4 - Nova York = 29

5 - Illinois = 20

6 - Pensilvânia = 20

7 - Ohio = 18

8 - Georgia = 16

9 - Michigan = 16

10 - Carolina do Norte = 15

Assumindo uma eleição com dois candidatos, A e B, 
crie uma função em Haskell que recebe uma tupla de 10 elementos
indicando em cada posição o candidato que venceu em cada 
estado na ordem listada acima. Por exemplo, se o candidato A aparece na posição 1
então ele venceu no estado da Califórnia, já se ele aparece na posição 8, 
ele ganhou o estado da Georgia. Desta forma, calcule a quantidade de votos
de delegados que cada candidato terá no colégio eleitoral e retorne o vencedor da eleição, ou seja,
aquele que conseguiu o maior número de votos de delegados.
-}

nomeEstado :: Int -> String
nomeEstado  1 = "California"
nomeEstado  2 = "Texas"
nomeEstado  3 = "Florida"
nomeEstado  4 = "Nova York"
nomeEstado  5 = "Illinois"
nomeEstado  6 = "Pensilvania"
nomeEstado  7 = "Ohio"
nomeEstado  8 = "Georgia"
nomeEstado  9 = "Michigan"
nomeEstado 10 = "Carolina do Norte"

estados :: Int -> Int
estados  1 = 55
estados  2 = 38
estados  3 = 29
estados  4 = 29
estados  5 = 20
estados  6 = 20
estados  7 = 18
estados  8 = 16
estados  9 = 16
estados 10 = 15

contaVotos :: Int -> Char -> [Char] -> Int
contaVotos n c lista | n == 0 && indiceLista == c = estados 1
                     | n == 0 && indiceLista /= c = 0
                     | indiceLista == c = (contaVotos (n-1) c lista) + estados (n+1)
                     | otherwise = (contaVotos (n-1) c lista)
                     where indiceLista = (lista !! n)

votos :: (Char, Char, Char, Char, Char, Char, Char, Char, Char, Char) -> Char
votos (a, b, c, d, e, f, g, h, i, j) | contaVotos 9 'A' mapa > contaVotos 9 'B' mapa = 'A'
                                     | otherwise = 'B'
                                     where contar c = contaVotos 9 c mapa
                                           mapa = [a, b, c, d, e, f, g, h, i, j]

