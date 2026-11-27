{- HLINT ignore "Eta reduce" -}
{- HLINT ignore "Use notElem" -}

module LeituraVRP (lerVRP, carregarVRP) where

import Instancia
import Data.List

trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

valorCabecalho :: String -> [String] -> Either String String
valorCabecalho chave linhas =
    case [trim (dropWhile (`elem` " :") (drop (length chave) linha))
         | linha <- linhas
         , take (length chave) linha == chave] of
        (valor:_) -> Right valor
        [] -> Left ("Cabecalho ausente: " ++ chave)

lerSecao :: String -> [String] -> [String]
lerSecao nome linhas =
    takeWhile (\linha ->
        not (linha == "EOF" || "_SECTION" `isSuffixOf` linha))
    (drop 1 (dropWhile (/= nome) linhas))

lerCoordenadas :: [String] -> Either String [(Double, Double)]
lerCoordenadas linhas =
    mapM lerLinha linhas
    where
        lerLinha linha =
            case words linha of
                [_indice, x, y] -> Right (read x, read y)
                _ -> Left ("Coordenada invalida: " ++ linha)

lerDemandas :: [String] -> Either String [Int]
lerDemandas linhas =
    mapM lerLinha linhas
    where
        lerLinha linha =
            case words linha of
                [_indice, demanda] -> Right (read demanda)
                _ -> Left ("Demanda invalida: " ++ linha)

lerDeposito :: [String] -> Either String Int
lerDeposito linhas =
    case linhas of
        (indice:_) -> Right (read indice)
        [] -> Left "Deposito ausente"

-- Parser principal
lerVRP :: String -> Either String Instancia
lerVRP conteudo = do
    let linhas = lines conteudo

    dimensaoTexto <- valorCabecalho "DIMENSION" linhas
    capacidadeTexto <- valorCabecalho "CAPACITY" linhas

    let dimensao = read dimensaoTexto :: Int
        capacidade = read capacidadeTexto :: Int

        coordenadasTexto = lerSecao "NODE_COORD_SECTION" linhas
        demandasTexto = lerSecao "DEMAND_SECTION" linhas
        depositoTexto = lerSecao "DEPOT_SECTION" linhas

    coordenadas <- lerCoordenadas coordenadasTexto
    demandasLidas <- lerDemandas demandasTexto
    deposito <- lerDeposito depositoTexto

    if length coordenadas /= dimensao
        then Left "Quantidade de coordenadas diferente de DIMENSION"
        else if length demandasLidas /= dimensao
            then Left "Quantidade de demandas diferente de DIMENSION"
            else if deposito /= 1
                then Left "Este parser espera o deposito no indice 1"
                else
                    let pontos = coordenadas
                        demandas = demandasLidas
                    in Right Instancia
                        { capacidadeMaxima = capacidade
                        , demandas = demandas
                        , pontos = pontos
                        , matrizDistancias = constroiMatriz pontos
                        }

-- Lê o arquivo do disco e aplica o parser
carregarVRP :: FilePath -> IO (Either String Instancia)
carregarVRP caminho = do
    conteudo <- readFile caminho
    return (lerVRP conteudo)

