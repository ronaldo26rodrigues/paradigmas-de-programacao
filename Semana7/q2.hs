{-
Crie um programa em Haskell que fica pedindo para o usuário 
digitar caminhos para arquivos até o usuário digitar uma linha vazia. 
Em seguida, seu programa deve concatenar o conteúdo de todos os 
arquivos informados na entrada em um único arquivo cujo nome também 
deve ser informado pelo usuário. 
-}

loop :: String -> IO ()
loop t = do putStrLn "Digite um caminho de arquivo"
            x <- getLine
            if x == "" then do putStrLn "Digite o caminho do arquivo de saída"
                               s <- getLine
                               writeFile s t
                               else do r <- readFile x
                                       putStrLn r
                                       loop (t ++ r ++ "\n")

concatenar :: IO ()
concatenar = loop ""
