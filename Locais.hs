module Locais
  ( Local (..),
    converteNomeECoordenadasECustoDaDiariaEmLocais,
    calculaDistanciasDoLocalAtualParaOsProximosLocais
  ) where

import DistanciaEuclidiana (calculaDistancia)

data Local = Local
  { nome :: String,
    coordenadas :: (Int, Int), -- Coordenadas em duas dimensoes (x, y)
    custoDaDiaria :: Int,
    indice :: Int
  } deriving (Show, Eq)

converteNomeECoordenadasECustoDaDiariaEmLocais :: Int -> [String] -> [String] -> [Local]
converteNomeECoordenadasECustoDaDiariaEmLocais _ [] _ = []
converteNomeECoordenadasECustoDaDiariaEmLocais _ _ [] = []
converteNomeECoordenadasECustoDaDiariaEmLocais _ [_] (_:_) = []
converteNomeECoordenadasECustoDaDiariaEmLocais
  indice
  (nome : coordenadasBrutas : restoDosNomesECordenadas)
  (custoDaDiaria : restoDosCustosDasDiarias) = do

  let arrayCoordenadas = words coordenadasBrutas
  let coordenadas = (read (head arrayCoordenadas) :: Int, read (arrayCoordenadas !! 1) :: Int)
  let inteiroCustoDaDiaria = read custoDaDiaria :: Int

  Local {
    nome = nome,
    coordenadas = coordenadas,
    custoDaDiaria = inteiroCustoDaDiaria,
    indice = indice
  } : converteNomeECoordenadasECustoDaDiariaEmLocais (indice + 1) restoDosNomesECordenadas restoDosCustosDasDiarias


calculaDistanciasDoLocalAtualParaOsProximosLocais :: Local -> [Local] -> [Float]
calculaDistanciasDoLocalAtualParaOsProximosLocais _ [] = []
calculaDistanciasDoLocalAtualParaOsProximosLocais localAtual (possivelProximoLocal : locais) = do
  let distancia = calculaDistancia (coordenadas localAtual) (coordenadas possivelProximoLocal)

  distancia : calculaDistanciasDoLocalAtualParaOsProximosLocais localAtual locais
