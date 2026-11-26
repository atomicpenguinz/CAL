import Data.Array
{- HLINT ignore "Eta reduce" -}

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
    let ps =
        [ (0, 0)
        , (2, 3)
        , (5, 4)
        , (1, 6)
        , (6, 1)
        , (8, 4)
        ]
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
atualizar indice valor lista = take i lista ++ [valor] ++ drop (i+1) lista

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
        posicaoDestino :: Int
                
    }

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
                    , posicaoDestino = posDestino
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
