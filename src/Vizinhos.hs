module Vizinhos where

import Instancia
import Data.List

data Movimento 
    = IntraRelocate {
        rota :: Int,
        posicaoOrigem :: Int,
        posicaoDestino :: Int
    }
    | InterShift {
        clienteMovido :: Cliente,
        rotaOrigem :: Int,
        rotaDestino :: Int,
        posOrigem :: Int,
        posDestino :: Int
                
    }
    deriving (Eq,Show)

vizinhos :: Instancia -> Solucao -> [(Solucao, Movimento)]
vizinhos inst sol = vizinhosIntraRelocate sol ++ vizinhosInterShift inst sol

vizinhosIntraRelocate :: Solucao -> [(Solucao, Movimento)]
vizinhosIntraRelocate sol = do
    (indiceRota, rota) <- zip [0..] sol
    (pos, cliente) <- zip [0..] rota
    posDestino <- [0 .. length rota - 1]

    if pos /= posDestino
        then do
            let rotaSemCliente = remover pos rota
                novaRota = inserir posDestino cliente rotaSemCliente
                candidato = atualizar indiceRota novaRota sol

            return
                ( candidato
                , IntraRelocate
                    { rota = indiceRota
                    , posicaoOrigem = pos
                    , posicaoDestino = posDestino
                    }
                )
        else []

vizinhosInterShift :: Instancia -> Solucao -> [(Solucao, Movimento)]
vizinhosInterShift inst sol = do
    (indiceOrigem, rotaOrigem) <- zip [0..] sol
    (posOrigem, cliente) <- zip [0..] rotaOrigem

    (indiceDestino, rotaDestino) <- zip [0..] sol

    if indiceOrigem /= indiceDestino
        && carga inst rotaDestino + demandas inst !! cliente
            <= capacidadeMaxima inst
        then do
            posDestino <- [0 .. length rotaDestino]

            let novaOrigem = remover posOrigem rotaOrigem
                novaDestino = inserir posDestino cliente rotaDestino

                solSemCliente = atualizar indiceOrigem novaOrigem sol
                candidato = atualizar indiceDestino novaDestino solSemCliente

            return
                ( candidato
                , InterShift
                    { clienteMovido = cliente
                    , rotaOrigem = indiceOrigem
                    , rotaDestino = indiceDestino
                    , posOrigem = posOrigem
                    , posDestino = posDestino
                    }
                )
        else []

solucaoInicial :: Instancia -> Solucao
solucaoInicial inst = construir clientes [] []
    where
        clientes = [1 .. length (demandas inst) - 1]

        construir [] rota sol =
            sol ++ [rota]

        construir (c:cs) rota sol
            | carga inst (rota ++ [c]) <= capacidadeMaxima inst =
                construir cs (rota ++ [c]) sol
            | otherwise =
                construir cs [c] (sol ++ [rota])

melhorVizinho :: Instancia -> [(Solucao, Movimento)]-> (Solucao, Movimento)
melhorVizinho inst vizinhos =
    snd (minimumBy comparaCusto todosVizinhos)
    where 
        todosVizinhos = map calculaCusto vizinhos

        calculaCusto (solucao, movimento) =
            (custoTotal inst solucao, (solucao, movimento))

        comparaCusto (custo1, _) (custo2, _) =
            compare custo1 custo2

