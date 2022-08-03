{-
Crie um programa em Haskell que pergunta ao usuário se ele deseja saber o 
total de palavras ou de caracteres de um arquivo de texto. Em seguida ele 
deve solicitar que o usuário forneça como entrada o caminho para um arquivo de texto. 
Por último, de acordo com a primeira escolha do usuário, ele deve contar palavras ou 
caracteres e informar na saída padrão o total de elementos de acordo com a escolha. 
Caso a escolha seja caracter, espaços em branco, caracteres de tabulação e nova linha 
não devem ser considerados na contabilização. Abaixo segue um exemplo de como o prompt 
do seu programa deve funcionar:

"Digite 1 para contar palavras ou 2 para contar caracteres:"

-> 2

"Digite o caminho do arquivo de texto:"

-> exemplo.txt

"O total de caracteres no arquivo exemplo.txt é 156."   


-}

contaPalavras :: String -> Int
contaPalavras a = length (words a)

contaCarac :: String -> Int
contaCarac a = length (filter (\x -> x /=' ' && x /='\t' && x /='\n') a)

contar :: IO ()
contar = do putStrLn "Digite 1 para contar palavras ou 2 para contar caracteres:"
            putStr "-> "
            option <- getLine
            putStrLn "Digite o caminho do arquivo de texto:"
            putStr "-> "
            caminho <- getLine
            arquivo <- readFile caminho
            if option == "1" then putStr (show (contaPalavras arquivo)) else if option == "2" then putStr (show (contaCarac arquivo)) else putStrLn "Opção inválida"
