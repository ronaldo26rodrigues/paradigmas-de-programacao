{-
Em um sistema de RH uma pessoa é representada por nome, cpf e idade. 
Além disto, ela pode estar na categoria adulta (>=18 anos) ou criança (<18 anos). 
Por último, um adulto pode estar associado a uma lista contendo seus filhos (os quais podem ser crianças ou adultos). 
Crie tipos algébricos para representar estas estruturas. Em seguida defina duas funções: 

a) crie uma função "ehFilho::Pessoa -> Pessoa -> Bool" que recebe duas pessoas e retorna 
True caso a segunda seja filho(a) da primeira e False caso contrário.

b) crie uma função "mostraPessoa::Pessoa -> String" que recebe uma pessoa e retorna uma 
String contendo todas as suas informações (nome, cpf, idade, informações dos filhos)
-}


type Nome = String
type CPF = String
type Idade = Int

data Pessoa = Adulto Nome CPF Idade [Pessoa] | Crianca Nome CPF Idade deriving (Show)




ehFilho :: Pessoa -> Pessoa -> Bool
ehFilho (Adulto n c i as) p2 = if length (filter (==p2) as) > 0 then True else False



mostraPessoa :: Pessoa -> String
mostraPessoa a = "a"






