module ProcessaArquivos (processaArquivosERetornaOsResultados) where

import Locais (converteNomeECoordenadasECustoDaDiariaEmLocais)
import Resultados.Data (ResultadoTotal (custoTotal, distanciaTotal, sequenciaDasCidades))
import Resultados.Resultados (calculaResultados)
import Text.Printf (printf)

processaArquivosERetornaOsResultados :: FilePath -> FilePath -> IO ()
processaArquivosERetornaOsResultados
  nomeDoArquivoDeNomeECoordenadas
  nomeDoArquivoDoCustosDasDiarias = do
    nomesECordenadas <- readFile nomeDoArquivoDeNomeECoordenadas
    custosDasDiarias <- readFile nomeDoArquivoDoCustosDasDiarias

    let listaDeNomesECordenadas = lines nomesECordenadas
    let quantidadeDeLocais = length listaDeNomesECordenadas `div` 2 -- Divide por dois, pois cada duas linhas representam um Local
    let (listaDeCustosDasDiarias, matrizDeCustoPorKMBruto) = splitAt quantidadeDeLocais (lines custosDasDiarias)
    let matrizDeCustoPorKM = geraMatrizDeCustoPorKM matrizDeCustoPorKMBruto

    let locais = converteNomeECoordenadasECustoDaDiariaEmLocais 0 listaDeNomesECordenadas listaDeCustosDasDiarias

    let resultados = calculaResultados matrizDeCustoPorKM locais

    writeFile "saida.txt" $ formataASaida resultados

geraMatrizDeCustoPorKM :: [String] -> [[Float]]
geraMatrizDeCustoPorKM =
  map (converteArrayDeStringEmArrayDeFloat . words)

converteArrayDeStringEmArrayDeFloat :: [String] -> [Float]
converteArrayDeStringEmArrayDeFloat [] = []
converteArrayDeStringEmArrayDeFloat arrayDeString =
  map (\linha -> (read linha :: Float)) arrayDeString

imprimeDistanciaECusto :: ResultadoTotal -> String
imprimeDistanciaECusto resultado = printf "Distancia: %f Custo: %f\n" (distanciaTotal resultado) (custoTotal resultado)

imprimeSequenciaDasCidades :: ResultadoTotal -> String
imprimeSequenciaDasCidades resultado = printf "Sequencia de Cidades:%s " (show $ sequenciaDasCidades resultado)

imprimeSequenciaDasCidadesEDistanciaECusto :: ResultadoTotal -> String
imprimeSequenciaDasCidadesEDistanciaECusto resultado = imprimeSequenciaDasCidades resultado ++ imprimeDistanciaECusto resultado

formataASaida :: [ResultadoTotal] -> String
formataASaida resultados =
  imprimeDistanciaECusto custoSequencial ++ imprimeSequenciaDasCidadesEDistanciaECusto menorDistancia ++ imprimeSequenciaDasCidadesEDistanciaECusto menorCusto
  where
    custoSequencial = head resultados
    menorDistancia = resultados !! 1
    menorCusto = resultados !! 2
