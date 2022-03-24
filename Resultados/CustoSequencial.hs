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
calculaDistanciaEmOrdemSequencial matrizDeCustoPorKm (localAtual : proximoLocal : locais) = do
  let distancia = calculaDistancia (coordenadas localAtual) (coordenadas proximoLocal)
  let custoPorKm = matrizDeCustoPorKm !! indice localAtual !! indice proximoLocal
  let diaria = custoDaDiaria proximoLocal
  let custo = (custoPorKm * distancia) + fromIntegral diaria

  (distancia, custo) : calculaDistanciaEmOrdemSequencial matrizDeCustoPorKm (proximoLocal : locais)

pegarDistancias :: [(Float, Float)] -> [Float]
pegarDistancias = map fst

pegarCustoDaViagem :: [(Float, Float)] -> [Float]
pegarCustoDaViagem = map snd

calculaOCustoSequencial :: [[Float]] -> [Local] -> ResultadoTotal
calculaOCustoSequencial matrizDeCustoPorKm locais = do
  let distanciasECustosPorViagem = calculaDistanciaEmOrdemSequencial matrizDeCustoPorKm locais
  let distanciaTotal = sum $ pegarDistancias distanciasECustosPorViagem
  let custoTotalDaViagem = sum $ pegarCustoDaViagem distanciasECustosPorViagem
  let sequenciaDasCidades = map nome locais

  ResultadoTotal
    { distanciaTotal = distanciaTotal,
      custoTotal = custoTotalDaViagem,
      sequenciaDasCidades = sequenciaDasCidades
    }
