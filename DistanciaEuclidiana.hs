module DistanciaEuclidiana (calculaDistancia) where

subtraiEElevaAoQuadrado :: Int -> Int -> Int
subtraiEElevaAoQuadrado i j = (i - j) ^ 2

calculaDistancia :: (Int, Int) -> (Int, Int) -> Float
calculaDistancia (x1, y1) (x2, y2) = sqrt (fromIntegral somaDosQuadrados)
  where
    somaDosQuadrados = subtraiEElevaAoQuadrado x2 x1 + subtraiEElevaAoQuadrado y2 y1
