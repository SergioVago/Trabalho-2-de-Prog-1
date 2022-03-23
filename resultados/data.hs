module Resultados.Data
  ( ResultadoTotal (..),
  )
where

data ResultadoTotal = ResultadoTotal
  { distanciaTotal :: Float,
    custoTotal :: Float,
    sequenciaDasCidades :: [String]
  }
  deriving (Show)