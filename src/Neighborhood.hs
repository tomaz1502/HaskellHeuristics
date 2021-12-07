module Neighborhood where

import Data.List ( minimumBy )
import Data.Vector ( singleton, (!), Vector )
import qualified Data.Vector
import qualified Data.Vector as V

import Node ( Node )
import TSPInstance ( Tour, evalT )

newtype Neighborhood = Neighborhood (Tour -> [Tour])

getBestNeighbor :: Neighborhood -> Tour -> Tour
getBestNeighbor (Neighborhood n) p =
  minimumBy (\n1 n2 -> if evalT n1 < evalT n2 then LT else GT) (n p)

twoOpt :: Neighborhood
twoOpt = Neighborhood $ \tour -> go 0 2 tour
  where go :: Int -> Int -> Tour -> [Tour]
        go i k t
          | i     >= length t = []
          | k + 1 >= length t = go (i + 1) (i + 3) t
          | otherwise         = swap i k t : go i (k + 1) t

        swap :: Int -> Int -> Tour -> Tour
        swap i k t = V.take i t V.++
                      singleton (t ! k) V.++
                      V.reverse (V.slice (i + 1) (k - 1) t) V.++
                      V.drop k t

threeOpt :: Neighborhood
threeOpt = Neighborhood $ \tour -> undefined
