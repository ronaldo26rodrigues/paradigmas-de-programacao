{-
Usando a linguagem funcional Haskell, defina uma função que recebe uma lista de Strings e retorna 
a soma dos valores correspondentes somente aos caracteres alfabéticos. 
Por exemplo: somaCarac ["expr 2222", "casa 23"] retorna e(101) + x(120) + p(112) + r(114) + c(99) + a(97) + s(115) + a(97) = 855. 
OBS: Lembre-se que as funções isAlpha e ord já estão disponíveis na linguagem,
onde a primeira retorna um booleano True caso o caractere passado como parâmetro seja alfabético e False caso contrario,
enquanto que a segunda retorna o valor ordinal de um caractere de acordo com a tabela ASCII.
-}

import Data.Char

somaString :: [Char] -> Int
somaString [] = 0
somaString (x:xs) | isAlpha x = ord x + somaString xs
                  | otherwise = somaString xs

somaCarac :: [[Char]] -> Int
somaCarac [] = 0
somaCarac (x:xs) = somaString x + somaCarac xs
