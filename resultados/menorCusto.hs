module Resultados.MenorCusto (calculaAViagemPeloMenorCusto) where

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
    calculaDistanciasDoLocalAtualParaOsProximosLocais,
  )
import Resultados.Data
  ( ResultadoTotal
      ( ResultadoTotal,
        custoTotal,
        distanciaTotal,
        sequenciaDasCidades
      ),
  )

-- A inversao das distancias serve para a funcao minimum encontrar o local de maior ordem
-- Retorna a distancia na primeira posicao e o indice na segunda
encontraOMenorCustoESeuIndiceComOIndiceInvertido :: [Float] -> (Float, Int)
encontraOMenorCustoESeuIndiceComOIndiceInvertido custos = do
  let custosComOIndexInvertido = zip (reverse custos) [0 ..]

  minimum custosComOIndexInvertido

encontraOProximoLocalComMenorCusto :: [Local] -> [(Float, Float)] -> (Local, (Float, Float))
encontraOProximoLocalComMenorCusto locais custosEDistancias = do
  let custos = map (\(custo, distancia) -> custo) custosEDistancias
  let distancias = map (\(custo, distancia) -> distancia) custosEDistancias

  let custoEIndiceInvertido = encontraOMenorCustoESeuIndiceComOIndiceInvertido custos
  let custos = fst custoEIndiceInvertido
  let indice = snd custoEIndiceInvertido

  let distancia = (reverse distancias) !! indice
  let local = (reverse locais) !! indice

  (local, (custos, distancia))

pegarCustosEDistanciaESequenciaDasCidades :: [[Float]] -> (Local, Int) -> [Local] -> [(Local, (Float, Float))]
pegarCustosEDistanciaESequenciaDasCidades _ _ [] = []
pegarCustosEDistanciaESequenciaDasCidades _ _ [ultimoLocal] = [(ultimoLocal, (0, 0))]
pegarCustosEDistanciaESequenciaDasCidades matrizDeCustoPorKM (localAtual, indiceDoLocalAtual) locais = do
  let (_, proximosLocais) = splitAt (indiceDoLocalAtual + 1) locais

  if ((length proximosLocais) == 0)
    then (localAtual, (0, 0)) : []
    else do
      let custosEDistanciaDaViagem = calculaCustoDaViagemDoLocalAtualParaOsProximosLocais matrizDeCustoPorKM localAtual proximosLocais
      let (proximoLocal, (custo, distancia)) = encontraOProximoLocalComMenorCusto proximosLocais custosEDistanciaDaViagem
      let indice = fromJust $ elemIndex proximoLocal proximosLocais

      (localAtual, (custo, distancia)) : pegarCustosEDistanciaESequenciaDasCidades matrizDeCustoPorKM (proximoLocal, indice) proximosLocais

calculaCustoDaViagemDoLocalAtualParaOsProximosLocais :: [[Float]] -> Local -> [Local] -> [(Float, Float)]
calculaCustoDaViagemDoLocalAtualParaOsProximosLocais _ _ [] = []
calculaCustoDaViagemDoLocalAtualParaOsProximosLocais matrizDeCustoPorKM localAtual (possivelProximoLocal : locais) = do
  let distancia = calculaDistancia (coordenadas localAtual) (coordenadas possivelProximoLocal)
  let diaria = custoDaDiaria possivelProximoLocal
  let custoTotal = ((matrizDeCustoPorKM !! indice localAtual !! indice possivelProximoLocal) * distancia) + (fromIntegral diaria)

  (custoTotal, distancia) : calculaCustoDaViagemDoLocalAtualParaOsProximosLocais matrizDeCustoPorKM localAtual locais

pegarNomeDosLocais :: [(Local, (Float, Float))] -> [String]
pegarNomeDosLocais locaisCustosEDistancias =
  map (\(local, _) -> nome local) locaisCustosEDistancias

pegarCustosDaViagem :: [(Local, (Float, Float))] -> [Float]
pegarCustosDaViagem locaisCustosEDistancias =
  map (\(_, (custo, _)) -> custo) locaisCustosEDistancias

pegarDistancias :: [(Local, (Float, Float))] -> [Float]
pegarDistancias locaisCustosEDistancias =
  map (\(_, (_, distancia)) -> distancia) locaisCustosEDistancias

pegarDistanciaTotal :: [Float] -> Float
pegarDistanciaTotal distancias =
  foldl (\acc x -> acc + x) 0 distancias

pegarCustoTotalDaViagem :: [Float] -> Float
pegarCustoTotalDaViagem custosDaViagem =
  foldl (\acc custoDaViagem -> acc + custoDaViagem) 0 custosDaViagem

calculaAViagemPeloMenorCusto :: [[Float]] -> [Local] -> ResultadoTotal
calculaAViagemPeloMenorCusto matrizDeCustoPorKM locais = do
  ResultadoTotal
    { distanciaTotal = distanciaTotal,
      custoTotal = custoTotal,
      sequenciaDasCidades = sequenciaDasCidades
    }
  where
    indiceInicial = 0
    locaisECustosEDistancias = pegarCustosEDistanciaESequenciaDasCidades matrizDeCustoPorKM (locais !! indiceInicial, indiceInicial) locais
    sequenciaDasCidades = pegarNomeDosLocais locaisECustosEDistancias
    custoTotal = pegarCustoTotalDaViagem $ pegarCustosDaViagem locaisECustosEDistancias
    distanciaTotal = pegarDistanciaTotal $ pegarDistancias locaisECustosEDistancias