module Resultados.Resultados (calculaResultados) where

import Locais (Local)
import Resultados.CustoSequencial (calculaOCustoSequencial)
import Resultados.Data (ResultadoTotal)
import Resultados.MenorCusto (calculaAViagemPeloMenorCusto)
import Resultados.MenorDistancia (calculaOCustoPelaMenorDistancia)

calculaResultados :: [[Float]] -> [Local] -> [ResultadoTotal]
calculaResultados matrizDeCustoPorKm locais =
  [ calculaOCustoSequencial matrizDeCustoPorKm locais,
    calculaOCustoPelaMenorDistancia matrizDeCustoPorKm locais,
    calculaAViagemPeloMenorCusto matrizDeCustoPorKm locais
  ]