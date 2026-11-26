{- HLINT ignore "Eta reduce" -}

Import Data.Array

type Cliente = Int
type Rota = [Cliente]
type Solucao = [Rota]

data Instancia = Instancia {
  capacidadeMaxima :: Int,
  demandas :: [Int],
  pontos :: [(Double, Double)],
  matrizDistancias :: Array (Int, Int) Double
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

constroiMatriz :: [(Double, Double)] -> MatrizDouble
constroiMatriz pontos =
    listArray ((0,0), (n-1,n-1))
    [ distancia p1 p2
    | p1 <- pontos
    , p2 <- pontos]
    where n = length pontos

custoTotal :: Instancia -> Solucao -> Double
custoTotal inst sol =
    sum [distanciaRota inst rota | rota <- sol]

solucaoValida :: Instancia -> Solucao -> Bool
solucaoValida _ [] = True
solucaoValida inst (rota:rotas)
    | carga inst rota <= inst capacidadeMaxima = solucaoValida inst rotas
    | otherwise = False

data Movimento 
    = IntraRelocate {
        rota :: Int,
        posicaoOrigem :: Int,
        posicaoDestino :: Int
    }

vizinhosIntraRelocate sol matriz :: Solucao -> MatrizDouble -> Solucao
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
                    { irRota = indiceRota
                    , irPosicaoOrigem = pos
                    , irPosicaoDestino = posDestino
                    }
                )
        else []
