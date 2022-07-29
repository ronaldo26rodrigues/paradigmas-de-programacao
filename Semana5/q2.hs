import Data.Type.Bool (Not)
{-
Blockchain é basicamente um banco de dados distribuído onde os dados são armazenados em blocos 
onde cada bloco possui transações financeiras e uma referência para o próximo bloco formando uma corrente. 
Assumindo os tipos de dados abaixo, crie uma função em Haskell que dado um Bloco e uma String de uma conta 
retornam uma tupla-2, no qual o primeiro elemento corresponde a média dos valores das transações em toda a 
corrente nas quais a conta passada como parâmetro era a origem (de), e o segundo elemento contém também a 
média dos valores mas quando a conta era destino (para).

data Transacao = Transacao { de :: String -- Conta que paga

                                                , para :: String -- Conta que recebe

                                                 , valor :: Float -- Quanto está pagando

                                                } deriving (Show)

data Bloco = Bloco { indice :: Int -- Indice do bloco

                                 , trs :: [Transacao] -- Lista de transações de um bloco

                                 , proximo :: Maybe Bloco -- Proximo bloco

                                 } deriving (Show)

OBS: para definir um valor do tipo bloco é só especificar (Bloco ind trs prox), onde ind é um inteiro, 
trs é uma lista de transações e prox é um Maybe Bloco. O mesmo se aplica ao tipo Transacao.
-}


data Transacao = Transacao { de :: String -- Conta que paga
                             , para :: String -- Conta que recebe
                             , valor :: Float -- Quanto está pagando
                           } deriving (Show)

data Bloco = Bloco { indice :: Int -- Indice do bloco
                     , trs  :: [Transacao] -- Lista de transações de um bloco
                     , proximo :: Maybe Bloco -- Proximo bloco
                   } deriving (Show)


-- para testar: medias (Just (Bloco 0 [(Transacao "001" "002" 150), (Transacao "001" "003" 100), (Transacao "002" "001" 180), (Transacao "003" "004" 120)] (Just (Bloco 1 [(Transacao "004" "002" 150), (Transacao "001" "003" 100), (Transacao "002" "001" 180), (Transacao "004" "001" 120)] Nothing)))) "001"

juntarListas :: Maybe Bloco -> [Transacao]
juntarListas Nothing = []
juntarListas (Just (Bloco _ ts b)) = ts ++ juntarListas b

getValorTransacao :: Transacao -> Float
getValorTransacao (Transacao _ _ v) = v

somarLista :: [Transacao] -> Float
somarLista = foldr ((+) . getValorTransacao) 0

contaDeIgual :: String -> Transacao -> Bool
contaDeIgual c (Transacao d _ _) = c == d

contaFromIgual :: String -> Transacao -> Bool
contaFromIgual c (Transacao _ f _) = c == f

medias :: Maybe Bloco -> String -> (Float, Float)
medias b c = (media (contaDeIgual c), media (contaFromIgual c))
            where media x = somarLista (filter x (juntarListas b)) / fromIntegral (length (filter x (juntarListas b)))