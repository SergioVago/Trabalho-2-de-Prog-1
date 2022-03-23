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
  ((matrizDeCustoPorKM !! i !! j) * distancia)

pegarDistancias :: [(Float, Float)] -> [Float]
pegarDistancias distanciasECustoPorKM =
  map (\(distancia, custo) -> distancia) distanciasECustoPorKM

pegarDistanciaTotal :: [Float] -> Float
pegarDistanciaTotal distancias =
  foldl (\acc distancia -> acc + distancia) 0 distancias

pegarCustoTotalDasDiarias :: [Local] -> Int
pegarCustoTotalDasDiarias locais =
  foldl (\acc diaria -> acc + (custoDaDiaria diaria)) 0 (tail locais)

pegarCustoDaViagem :: [(Float, Float)] -> [Float]
pegarCustoDaViagem distanciasECustoPorKM =
  map (\(distancia, custo) -> custo) distanciasECustoPorKM

pegarCustoTotalDaViagem :: [Float] -> Float
pegarCustoTotalDaViagem custosPorViagem =
  foldl (\acc custoPorViagem -> acc + custoPorViagem) 0 custosPorViagem

calculaOCustoSequencial :: [[Float]] -> [Local] -> ResultadoTotal
calculaOCustoSequencial matrizDeCustoPorKM locais = do
  let distanciasECustosPorViagem = calculaDistanciaEmOrdemSequencial matrizDeCustoPorKM locais
  let distanciaTotal = pegarDistanciaTotal $ pegarDistancias distanciasECustosPorViagem
  let custoTotalDaViagem = pegarCustoTotalDaViagem $ pegarCustoDaViagem distanciasECustosPorViagem
  let custoTotalDasDiarias = fromIntegral $ pegarCustoTotalDasDiarias locais
  let sequenciaDasCidades = map (\local -> nome local) locais

  ResultadoTotal
    { distanciaTotal = pegarDistanciaTotal $ pegarDistancias distanciasECustosPorViagem,
      custoTotal = (custoTotalDaViagem + custoTotalDasDiarias),
      sequenciaDasCidades = sequenciaDasCidades
    }
