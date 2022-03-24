module Resultados.MenorDistancia (calculaOCustoPelaMenorDistancia) where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import DistanciaEuclidiana (calculaDistancia)
import Locais
  ( Local
      ( Local,
        coordenadas,
        custoDaDiaria,
        indice,
        nome
      ),
  )
import Resultados.Data
  ( ResultadoTotal
      ( ResultadoTotal,
        custoTotal,
        distanciaTotal,
        sequenciaDasCidades
      ),
  )

calculaDistanciasDoLocalAtualParaOsProximosLocais :: Local -> [Local] -> [Float]
calculaDistanciasDoLocalAtualParaOsProximosLocais _ [] = []
calculaDistanciasDoLocalAtualParaOsProximosLocais localAtual (possivelProximoLocal : locais) = do
  let distancia = calculaDistancia (coordenadas localAtual) (coordenadas possivelProximoLocal)

  distancia : calculaDistanciasDoLocalAtualParaOsProximosLocais localAtual locais

-- A inversao das distancias serve para a funcao minimum encontrar o local de maior ordem
-- Retorna a distancia na primeira posicao e o indice na segunda
encontraAMenorDistaciaESeuIndiceComOIndiceInvertido :: [Float] -> (Float, Int)
encontraAMenorDistaciaESeuIndiceComOIndiceInvertido distancias = do
  let distanciasComOIndexInvertido = zip (reverse distancias) [0 ..]

  minimum distanciasComOIndexInvertido

encontraOProximoLocalEDistanciaMaisPerto :: [Local] -> [Float] -> (Local, Float)
encontraOProximoLocalEDistanciaMaisPerto locais distancias = do
  let distanciaEIndiceInvertido = encontraAMenorDistaciaESeuIndiceComOIndiceInvertido distancias
  let (distancia, indice) = distanciaEIndiceInvertido
  let local = reverse locais !! indice

  (local, distancia)

pegarDistanciasESequenciaDasCidades :: (Local, Int) -> [Local] -> [(Local, Float)]
pegarDistanciasESequenciaDasCidades _ [] = []
pegarDistanciasESequenciaDasCidades (localAtual, indiceDoLocalAtual) locais = do
  let (_, proximosLocais) = splitAt (indiceDoLocalAtual + 1) locais

  if null proximosLocais
    then [(localAtual, 0)]
    else do
      let distancias = calculaDistanciasDoLocalAtualParaOsProximosLocais localAtual proximosLocais
      let (proximoLocal, distancia) = encontraOProximoLocalEDistanciaMaisPerto proximosLocais distancias
      let indice = fromJust $ elemIndex proximoLocal proximosLocais
      (localAtual, distancia) : pegarDistanciasESequenciaDasCidades (proximoLocal, indice) proximosLocais

pegarDistancias :: [(Local, Float)] -> [Float]
pegarDistancias = map snd

pegarNomeDosLocais :: [(Local, Float)] -> [String]
pegarNomeDosLocais = map (\(local, _) -> nome local)

pegarCustosDaDiariaDosLocais :: [(Local, Float)] -> [Int]
pegarCustosDaDiariaDosLocais locaisEDistancias =
  map (\(local, distancia) -> custoDaDiaria local) $ tail locaisEDistancias

pegarCustosDaViagem :: [[Float]] -> [(Local, Float)] -> [Float]
pegarCustosDaViagem _ [] = []
pegarCustosDaViagem _ [_] = []
pegarCustosDaViagem
  matrizDeCustoPorKm
  ((cidadeAtual, distancia) : (proximaCidade, proximaDistancia) : distanciasESequenciaDasCidades) =
    (custoPorKm * distancia)
      + fromIntegral diaria :
    pegarCustosDaViagem matrizDeCustoPorKm ((proximaCidade, proximaDistancia) : distanciasESequenciaDasCidades)
    where
      custoPorKm = matrizDeCustoPorKm !! indice cidadeAtual !! indice proximaCidade
      diaria = custoDaDiaria proximaCidade

calculaOCustoPelaMenorDistancia :: [[Float]] -> [Local] -> ResultadoTotal
calculaOCustoPelaMenorDistancia matrizDeCustoPorKm locais =
  ResultadoTotal
    { distanciaTotal = distanciaTotal,
      custoTotal = custoTotal,
      sequenciaDasCidades = sequenciaDasCidades
    }
  where
    indiceInicial = 0
    locaisEDistancias = pegarDistanciasESequenciaDasCidades (locais !! indiceInicial, indiceInicial) locais
    sequenciaDasCidades = pegarNomeDosLocais locaisEDistancias
    distanciaTotal = sum $ pegarDistancias locaisEDistancias
    custoTotal = sum $ pegarCustosDaViagem matrizDeCustoPorKm locaisEDistancias
