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
  let distancia = fst distanciaEIndiceInvertido
  let indice = snd distanciaEIndiceInvertido
  let local = (reverse locais) !! indice

  (local, distancia)

pegarDistanciasESequenciaDasCidades :: (Local, Int) -> [Local] -> [(Local, Float)]
pegarDistanciasESequenciaDasCidades _ [] = []
pegarDistanciasESequenciaDasCidades _ [ultimoLocal] = [(ultimoLocal, 0)]
pegarDistanciasESequenciaDasCidades (localAtual, indiceDoLocalAtual) locais = do
  let (_, proximosLocais) = splitAt (indiceDoLocalAtual + 1) locais

  if ((length proximosLocais) == 0)
    then (localAtual, 0) : []
    else do
      let distancias = calculaDistanciasDoLocalAtualParaOsProximosLocais localAtual proximosLocais
      let (proximoLocal, distancia) = encontraOProximoLocalEDistanciaMaisPerto proximosLocais distancias
      let indice = fromJust $ elemIndex proximoLocal proximosLocais
      (localAtual, distancia) : pegarDistanciasESequenciaDasCidades (proximoLocal, indice) proximosLocais

pegarDistancias :: [(Local, Float)] -> [Float]
pegarDistancias locaisEDistancias =
  map (\(local, distancia) -> distancia) locaisEDistancias

pegarDistanciaTotal :: [Float] -> Float
pegarDistanciaTotal distancias =
  foldl (\acc x -> acc + x) 0 distancias

pegarNomeDosLocais :: [(Local, Float)] -> [String]
pegarNomeDosLocais locaisEDistancias =
  map (\(local, distancia) -> nome local) locaisEDistancias

pegarCustosDaDiariaDosLocais :: [(Local, Float)] -> [Int]
pegarCustosDaDiariaDosLocais locaisEDistancias =
  map (\(local, distancia) -> custoDaDiaria local) locaisEDistancias

pegarCustosDaDiariaTotal :: [Int] -> Int
pegarCustosDaDiariaTotal custosDaDiaria =
  foldl (\acc x -> acc + x) 0 custosDaDiaria

pegarCustosDaViagem :: [[Float]] -> [(Local, Float)] -> [Float]
pegarCustosDaViagem _ [] = []
pegarCustosDaViagem _ [_] = []
pegarCustosDaViagem
  matrizDeCustoPorKM
  ((cidadeAtual, distancia) : (proximaCidade, _) : distanciasESequenciaDasCidades) =
    (matrizDeCustoPorKM !! indice cidadeAtual !! indice proximaCidade) * distancia :
    pegarCustosDaViagem matrizDeCustoPorKM distanciasESequenciaDasCidades

pegarCustoTotalDaViagem :: [Float] -> Float
pegarCustoTotalDaViagem custosDaViagem =
  foldl (\acc custoDaViagem -> acc + custoDaViagem) 0 custosDaViagem

calculaOCustoPelaMenorDistancia :: [[Float]] -> [Local] -> ResultadoTotal
calculaOCustoPelaMenorDistancia matrizDeCustoPorKM locais = do
  ResultadoTotal
    { distanciaTotal = distanciaTotal,
      custoTotal = custoTotal,
      sequenciaDasCidades = sequenciaDasCidades
    }
  where
    indiceInicial = 0
    locaisEDistancias = pegarDistanciasESequenciaDasCidades (locais !! indiceInicial, indiceInicial) locais
    sequenciaDasCidades = pegarNomeDosLocais locaisEDistancias
    custoDaViagem = pegarCustoTotalDaViagem $ pegarCustosDaViagem matrizDeCustoPorKM locaisEDistancias
    distanciaTotal = pegarDistanciaTotal $ pegarDistancias locaisEDistancias
    custoDasDiarias = pegarCustosDaDiariaTotal $ pegarCustosDaDiariaDosLocais locaisEDistancias
    custoTotal = (fromIntegral custoDasDiarias) + custoDaViagem