module Main where

import ProcessaArquivos (processaArquivosERetornaOsResultados)

main :: IO ()
main = do
  let nomeCord = "nome-coord.txt"
  let diariaCusto = "diaria-custo.txt"

  processaArquivosERetornaOsResultados nomeCord diariaCusto
