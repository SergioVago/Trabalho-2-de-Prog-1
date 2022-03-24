module Resultados.Resultados (calculaResultados) where

import Locais (Local)
import Resultados.CustoSequencial (calculaOCustoSequencial)
import Resultados.Data (ResultadoTotal)
import Resultados.MenorCusto (calculaAViagemPeloMenorCusto)
import Resultados.MenorDistancia (calculaOCustoPelaMenorDistancia)

calculaResultados :: [[Float]] -> [Local] -> [ResultadoTotal]
calculaResultados matrizDeCustoPorKM locais =
  [ calculaOCustoSequencial matrizDeCustoPorKM locais,
    calculaOCustoPelaMenorDistancia matrizDeCustoPorKM locais,
    calculaAViagemPeloMenorCusto matrizDeCustoPorKM locais
  ]