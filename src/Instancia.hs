module Instancia where

import Data.Array
import Data.List
import Data.Ord

type Cliente = Int
type Rota = [Cliente]
type Solucao = [Rota]

type MatrizDouble = Array (Int, Int) Double

data Instancia = Instancia {
  capacidadeMaxima :: Int,
  demandas :: [Int],
  pontos :: [(Double, Double)],
  matrizDistancias :: MatrizDouble
}

exemplo =
    let ps = [ (0, 0), (2, 3), (5, 4), (1, 6), (6, 1), (8, 4)]
    in 
    Instancia {
    capacidadeMaxima = 10,
    demandas = [0, 4, 3, 5, 6, 2],
    pontos = ps,
    matrizDistancias = constroiMatriz ps
}

distancia :: [(Double, Double)] -> Int -> Int -> Double
distancia pontos i j =
    let (x1,y1) = pontos !! i
        (x2,y2) = pontos !! j
    in sqrt ((x2 - x1)^2 + (y2 - y1)^2)

carga :: Instancia -> Rota -> Int
carga inst rota =
    sum [demandas inst !! c | c <- rota]

distanciaRota :: Instancia -> Rota -> Double
distanciaRota _ [] = 0
distanciaRota inst rota =
    let caminho = 0 : rota ++ [0]
    in sum [ matrizDistancias inst ! (a, b)
           | (a, b) <- zip caminho (drop 1 caminho) ]

constroiMatriz :: [(Double, Double)] -> MatrizDouble
constroiMatriz pontos =
    listArray ((0,0), (n-1,n-1))
    [ distancia pontos i j
    | i <- [0.. n - 1]
    , j <- [0.. n - 1]]
    where n = length pontos

custoTotal :: Instancia -> Solucao -> Double
custoTotal inst sol =
    sum [distanciaRota inst rota | rota <- sol]

solucaoValida :: Instancia -> Solucao -> Bool
solucaoValida _ [] = True
solucaoValida inst (rota:rotas)
    | carga inst rota <= capacidadeMaxima inst = solucaoValida inst rotas
    | otherwise = False

-- remover e inserir em indices determinados
remover :: Int -> [a] -> [a]
remover i lista = take i lista ++ drop (i+1) lista

inserir :: Int -> a -> [a] -> [a]
inserir i valor lista = take i lista ++ [valor] ++ drop i lista

-- substitui um elemento de uma lista
atualizar :: Int -> a -> [a] -> [a]
atualizar i valor lista = take i lista ++ [valor] ++ drop (i+1) lista

