module Neighborhood where

import Data.List ( minimumBy )
import Data.Vector ( singleton, (!), Vector )
import qualified Data.Vector
import qualified Data.Vector as V

import Node ( Node )
import TSPInstance ( Tour, evalT )
import Utils ( replace )

newtype Neighborhood = Neighborhood (Tour -> [Tour])

run :: Neighborhood -> Tour -> [Tour]
run (Neighborhood n) = n

getBestNeighbor :: Neighborhood -> Tour -> Tour
getBestNeighbor n p =
  minimumBy (\n1 n2 -> if evalT n1 < evalT n2 then LT else GT) (run n p)

swap :: Int -> Int -> Tour -> Tour
swap i k t = V.take (i + 1) t V.++
             V.reverse (V.slice (i + 1) (k - i) t) V.++
             V.drop (k + 1) t

twoOpt :: Neighborhood
twoOpt = Neighborhood $ \tour -> go tour (allPairs (length tour))
  where go :: Tour -> [(Int, Int)] -> [Tour]
        go _ []           = []
        go t ((i, j) : r) = swap i j t : go t r

        allPairs n = [(i, j) | i <- [0 .. n - 1], j <- [i + 2 .. n - 1]]

threeOpt :: Neighborhood
threeOpt = Neighborhood $ \tour -> go tour (allTriples (length tour))
  where go t [] = []
        go t ((i, j, k) : r) = bestSwap (i, j, k) t : go t r

        bestSwap :: (Int, Int, Int) -> Tour -> Tour
        bestSwap (i, j, k) t =
          let t1 = swap i j t
              t2 = swap i k t
              t3 = swap j k t
              t4 = interleave i j k t
          in minimumBy (\tour1 tour2 -> if evalT tour1 < evalT tour2
                                        then LT else GT) [t1, t2, t3]

        interleave :: Int -> Int -> Int -> Tour -> Tour
        interleave i j k t =
          let tij  = V.slice i (j - i) t
              tjk  = V.slice j (k - j) t
              conc = tjk V.++ tij
          in  replace t conc i

        allTriples n =
          [ (i, j, k)
          | i <- [0 .. n - 1]
          , j <- [i + 2 .. n - 1]
          , k <- [j + 2 .. n - 1]
          ]
