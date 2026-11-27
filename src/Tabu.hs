{- HLINT ignore "Eta reduce" -}
module Tabu where

import Instancia
import Vizinhos

type MovimentoTabu = Movimento
type ListaTabu = [(MovimentoTabu, Int)] -- int é a iteração onde o tabu acaba


ehTabu :: Movimento -> Int -> ListaTabu -> Bool
ehTabu movimento iteracao lista =
    any (\(tabu, expira) -> movimento == tabu && iteracao < expira) lista

inverteMovimento :: Movimento -> Movimento

inverteMovimento (IntraRelocate rota pOrigem pDestino) =
    IntraRelocate rota pDestino pOrigem

inverteMovimento (InterShift cliente rotaOrigem rotaDestino pOrig pDestino) =
    InterShift cliente rotaDestino rotaOrigem pDestino pOrig

atualizarListaTabu :: Movimento -> Int -> Int -> ListaTabu -> ListaTabu
atualizarListaTabu movimento iteracao tempoTabu lista =
    (inverteMovimento movimento, iteracao + tempoTabu + 1)
        : filter (\(_, expira) -> iteracao < expira) lista

melhorVizinhoTabu
    :: Instancia
    -> Solucao
    -> Int
    -> ListaTabu
    -> Solucao
    -> Maybe (Solucao, Movimento)

melhorVizinhoTabu inst sol iteracao listaTabu melhorGlobal =
    case vizinhosPermitidos of
        [] -> Nothing
        _  -> Just (melhorVizinho inst vizinhosPermitidos)
    where
        vizinhosPermitidos =
            filter permitido (vizinhos inst sol)

        permitido (solucao, movimento) =
            solucaoValida inst solucao
            &&
            (not (ehTabu movimento iteracao listaTabu)
            || custoTotal inst solucao < custoTotal inst melhorGlobal)

buscaTabu :: Instancia -> Int -> Int -> Solucao
buscaTabu inst maxIteracoes tempoTabu =
    loop 0 solInicial solInicial []
    where
        solInicial = solucaoInicial inst

        loop iteracao solAtual melhorGlobal listaTabu
            | iteracao >= maxIteracoes =
                melhorGlobal

            | otherwise =
                case melhorVizinhoTabu
                    inst solAtual iteracao listaTabu melhorGlobal of

                    Nothing ->
                        melhorGlobal

                    Just (novaSolucao, movimento) ->
                        let novoMelhorGlobal =
                                if custoTotal inst novaSolucao
                                    < custoTotal inst melhorGlobal
                                then novaSolucao
                                else melhorGlobal

                            novaListaTabu =
                                atualizarListaTabu
                                    movimento
                                    iteracao
                                    tempoTabu
                                    listaTabu

                        in loop
                            (iteracao + 1)
                            novaSolucao
                            novoMelhorGlobal
                            novaListaTabu
