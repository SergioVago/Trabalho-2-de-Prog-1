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
  let custos = map fst custosEDistancias
  let distancias = map snd custosEDistancias

  let custoEIndiceInvertido = encontraOMenorCustoESeuIndiceComOIndiceInvertido custos
  let custos = fst custoEIndiceInvertido
  let indice = snd custoEIndiceInvertido

  let distancia = reverse distancias !! indice
  let local = reverse locais !! indice

  (local, (custos, distancia))

pegarCustosEDistanciaESequenciaDasCidades :: [[Float]] -> (Local, Int) -> [Local] -> [(Local, (Float, Float))]
pegarCustosEDistanciaESequenciaDasCidades _ _ [] = []
pegarCustosEDistanciaESequenciaDasCidades _ _ [ultimoLocal] = [(ultimoLocal, (0, 0))]
pegarCustosEDistanciaESequenciaDasCidades matrizDeCustoPorKm (localAtual, indiceDoLocalAtual) locais = do
  let (_, proximosLocais) = splitAt (indiceDoLocalAtual + 1) locais

  if null proximosLocais
    then [(localAtual, (0, 0))]
    else do
      let custosEDistanciaDaViagem = calculaCustoDaViagemDoLocalAtualParaOsProximosLocais matrizDeCustoPorKm localAtual proximosLocais
      let (proximoLocal, (custo, distancia)) = encontraOProximoLocalComMenorCusto proximosLocais custosEDistanciaDaViagem
      let indice = fromJust $ elemIndex proximoLocal proximosLocais

      (localAtual, (custo, distancia)) : pegarCustosEDistanciaESequenciaDasCidades matrizDeCustoPorKm (proximoLocal, indice) proximosLocais

calculaCustoDaViagemDoLocalAtualParaOsProximosLocais :: [[Float]] -> Local -> [Local] -> [(Float, Float)]
calculaCustoDaViagemDoLocalAtualParaOsProximosLocais _ _ [] = []
calculaCustoDaViagemDoLocalAtualParaOsProximosLocais matrizDeCustoPorKm localAtual (possivelProximoLocal : locais) = do
  let distancia = calculaDistancia (coordenadas localAtual) (coordenadas possivelProximoLocal)
  let diaria = custoDaDiaria possivelProximoLocal
  let custoTotal = ((matrizDeCustoPorKm !! indice localAtual !! indice possivelProximoLocal) * distancia) + fromIntegral diaria

  (custoTotal, distancia) : calculaCustoDaViagemDoLocalAtualParaOsProximosLocais matrizDeCustoPorKm localAtual locais

pegarNomeDosLocais :: [(Local, (Float, Float))] -> [String]
pegarNomeDosLocais = map (\(local, _) -> nome local)

pegarCustosDaViagem :: [(Local, (Float, Float))] -> [Float]
pegarCustosDaViagem = map (\(_, (custo, _)) -> custo)

pegarDistancias :: [(Local, (Float, Float))] -> [Float]
pegarDistancias = map (\(_, (_, distancia)) -> distancia)

calculaAViagemPeloMenorCusto :: [[Float]] -> [Local] -> ResultadoTotal
calculaAViagemPeloMenorCusto matrizDeCustoPorKm locais = do
  ResultadoTotal
    { distanciaTotal = distanciaTotal,
      custoTotal = custoTotal,
      sequenciaDasCidades = sequenciaDasCidades
    }
  where
    indiceInicial = 0
    locaisECustosEDistancias = pegarCustosEDistanciaESequenciaDasCidades matrizDeCustoPorKm (locais !! indiceInicial, indiceInicial) locais
    sequenciaDasCidades = pegarNomeDosLocais locaisECustosEDistancias
    custoTotal = sum $ pegarCustosDaViagem locaisECustosEDistancias
    distanciaTotal = sum $ pegarDistancias locaisECustosEDistancias