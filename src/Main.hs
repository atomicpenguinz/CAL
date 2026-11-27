module Main where

import Instancia
import LeituraVRP
import Tabu
import System.FilePath
import System.Directory
import Data.List

main :: IO ()
main = do
    putStrLn "Digite do arquivo .vrp:"
    caminho <- getLine

    if caminho == "todos"
        then testarTodos
        else testarArquivo caminho

    where
    testarTodos = do
        arquivos <- listDirectory "../datasets/"

        let arquivosVRP = sort
                [ "../datasets/" </> arquivo
                | arquivo <- arquivos
                , takeExtension arquivo == ".vrp"
                ]

        mapM_ testarArquivo arquivosVRP
  
    testarArquivo entrada = do
        resultado <- carregarVRP entrada

        case resultado of
            Left erro ->
                putStrLn erro

            Right inst -> do
                putStrLn "Arquivo:"
                print entrada
                putStrLn "Instancia carregada!"

                let solucao = buscaTabu inst 100 5

                putStrLn "Solucao encontrada:"
                print solucao

                -- putStrLn "Cargas das rotas:"
                -- print (map (carga inst) solucao)

                -- putStrLn "Solucao valida:"
                -- print (solucaoValida inst solucao)

                putStrLn "número de veículos:"
                print (length solucao)

                putStrLn "Custo total:"
                print (custoTotal inst solucao)
                putStrLn "\n"
