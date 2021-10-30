-- Local maxima
module Exercise2 where

localMaxima :: [Integer] -> [Integer]
localMaxima (x : xs@(y : z : _))
  | all (y >) [x, z] = y : localMaxima xs
  | otherwise = localMaxima xs
localMaxima _ = []
