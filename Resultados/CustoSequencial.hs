module Resultados.CustoSequencial (calculaOCustoSequencial) where

import DistanciaEuclidiana (calculaDistancia)
import Locais (Local (..))
import Resultados.Data
  ( ResultadoTotal
      ( ResultadoTotal,
        custoTotal,
        distanciaTotal,
        sequenciaDasCidades
      ),
  )

calculaDistanciaEmOrdemSequencial :: [[Float]] -> [Local] -> [(Float, Float)]
calculaDistanciaEmOrdemSequencial _ [] = []
calculaDistanciaEmOrdemSequencial _ [_] = []
calculaDistanciaEmOrdemSequencial matrizDeCustoPorKM (localAtual : proximoLocal : locais) = do
  let distancia = calculaDistancia (coordenadas localAtual) (coordenadas proximoLocal)
  let custo = calculaCustoDaViagem matrizDeCustoPorKM (indice localAtual) (indice proximoLocal) distancia

  (distancia, custo) : calculaDistanciaEmOrdemSequencial matrizDeCustoPorKM (proximoLocal : locais)

calculaCustoDaViagem :: [[Float]] -> Int -> Int -> Float -> Float
calculaCustoDaViagem matrizDeCustoPorKM i j distancia =
  matrizDeCustoPorKM !! i !! j * distancia

pegarDistancias :: [(Float, Float)] -> [Float]
pegarDistancias = map fst

pegarCustoTotalDasDiarias :: [Local] -> [Int]
pegarCustoTotalDasDiarias locais =
  map custoDaDiaria (tail locais)

pegarCustoDaViagem :: [(Float, Float)] -> [Float]
pegarCustoDaViagem = map snd

calculaOCustoSequencial :: [[Float]] -> [Local] -> ResultadoTotal
calculaOCustoSequencial matrizDeCustoPorKM locais = do
  let distanciasECustosPorViagem = calculaDistanciaEmOrdemSequencial matrizDeCustoPorKM locais
  let distanciaTotal = sum $ pegarDistancias distanciasECustosPorViagem
  let custoTotalDaViagem = sum $ pegarCustoDaViagem distanciasECustosPorViagem
  let custoTotalDasDiarias = fromIntegral $ sum $ pegarCustoTotalDasDiarias locais
  let sequenciaDasCidades = map nome locais

  ResultadoTotal
    { distanciaTotal = distanciaTotal,
      custoTotal = custoTotalDaViagem + custoTotalDasDiarias,
      sequenciaDasCidades = sequenciaDasCidades
    }
