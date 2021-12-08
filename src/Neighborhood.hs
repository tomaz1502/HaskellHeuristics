module Neighborhood where

import Debug.Trace ( trace )

import Data.Vector ( singleton, (!), Vector )
import qualified Data.Vector
import qualified Data.Vector as V

import Node ( Node )
import Tour ( Tour(..), evalT )
import Utils ( replace )

newtype Neighborhood = Neighborhood (Tour -> [Tour])

run :: Neighborhood -> Tour -> [Tour]
run (Neighborhood n) = n

getBestNeighbor' :: Neighborhood -> Tour -> Tour
getBestNeighbor' n t =
  let b = getBestNeighbor n t
      delta = evalT b - evalT t
  in trace ("evalT b = " ++ show (evalT b) ++ "\nevalT n = " ++ show (evalT t) ++ "\n") b

getBestNeighbor :: Neighborhood -> Tour -> Tour
getBestNeighbor n t =
  case run n t of
    [] -> t
    ns -> minimum ns

getLocalBest :: Neighborhood -> Tour -> Tour
getLocalBest n t =
  let b = getBestNeighbor n t
  in if b >= t then t else getLocalBest n b

swap :: Int -> Int -> Tour -> Tour
swap i k (Tour t) = Tour $ V.take (i + 1) t V.++
                           V.reverse (V.slice (i + 1) (k - i) t) V.++
                           V.drop (k + 1) t

twoOpt :: Neighborhood
twoOpt = Neighborhood $ \t@(Tour tour) -> go t (allPairs (length tour))
  where go :: Tour -> [(Int, Int)] -> [Tour]
        go _ []           = []
        go t ((i, j) : r) = swap i j t : go t r

        allPairs n = [(i, j) | i <- [0 .. n - 1], j <- [i + 2 .. n - 1]]

threeOpt :: Neighborhood
threeOpt = Neighborhood $ \t@(Tour tour) -> go t (allTriples (length tour))
  where go t [] = []
        go t ((i, j, k) : r) = bestSwap (i, j, k) t : go t r

        bestSwap :: (Int, Int, Int) -> Tour -> Tour
        bestSwap (i, j, k) t =
          let t1 = swap i j t
              t2 = swap i k t
              t3 = swap j k t
              t4 = interleave i j k t
          in minimum [t1, t2, t3, t4]

        interleave :: Int -> Int -> Int -> Tour -> Tour
        interleave i j k (Tour t) =
          let tij  = V.slice i (j - i) t
              tjk  = V.slice j (k - j) t
              conc = tjk V.++ tij
          in Tour $ replace t conc i

        allTriples n =
          [ (i, j, k)
          | i <- [0 .. n - 1]
          , j <- [i + 2 .. n - 1]
          , k <- [j + 2 .. n - 1]
          ]
