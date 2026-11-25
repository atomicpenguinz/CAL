{- HLINT ignore "Eta reduce" -}
type Cliente = Int
type Rota = [Cliente]
type Solucao = [Rota]

type Ponto = (Double, Double)

data Instancia = Instancia {
  capacidadeMaxima :: Int,
  demandas :: [Int],
  pontos :: [(Double, Double)]
}

exemplo = Instancia {
    capacidadeMaxima = 10,
    demandas = [0, 4, 3, 5, 6, 2],
    pontos =
        [ (0, 0)
        , (2, 3)
        , (5, 4)
        , (1, 6)
        , (6, 1)
        , (8, 4)
        ]
}

distancia :: Instancia -> Int -> Int -> Double
distancia inst i j =
    let (x1,y1) = pontos inst !! i
        (x2,y2) = pontos inst !! j
    in sqrt ((x2 - x1)^2 + (y2 - y1)^2)

carga :: Instancia -> Rota -> Int
carga inst rota =
    sum [demandas inst !! c | c <- rota]

distanciaRota :: Instancia -> Rota -> Double
distanciaRota _ [] = 0
distanciaRota inst rota =
    let caminho = 0 : rota ++ [0]
    in sum [ distancia inst a b
           | (a, b) <- zip caminho (drop 1 caminho) ]

custoTotal :: Instancia -> Solucao -> Double
custoTotal inst sol =
    sum [distanciaRota inst rota | rota <- sol]

solucaoValida :: Instancia -> Solucao -> Bool
solucaoValida _ [] = True
solucaoValida inst (rota:rotas)
    | carga inst rota <= 10 = solucaoValida inst rotas
    | otherwise = False

data Movimento 
    = IntraRelocate {
        rota :: Int,
        posicaoOrigem :: Int,
        posicaoDestino :: Int
    }

