module Main where

import ProcessaArquivos (processaArquivosERetornaOsResultados)

main :: IO ()
main = do
  let nomeCord = "bateria1/nome-coord.txt"
  let diariaCusto = "bateria1/diaria-custo.txt"

  processaArquivosERetornaOsResultados nomeCord diariaCusto
