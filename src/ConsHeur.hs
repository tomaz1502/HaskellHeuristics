{-# LANGUAGE RecordWildCards #-}

module ConsHeur where

import           Utils ( applyN, erase )
import           TSPInstance ( eval, PartialSolution, TSPInstance(numNodes, nodes) )
import           Node ( distance )

import           Data.Vector (uncons, singleton)
import qualified Data.Vector as V

-- | A constructive heuristic
data ConsHeur =
  ConsHeur { initSol :: TSPInstance -> PartialSolution
           , step :: PartialSolution -> PartialSolution
           }

-- | Apply the given heuristic in the given tsp instance. Returns the cost of
--   the solution found
solve :: ConsHeur -> TSPInstance -> Double
solve ConsHeur {..} ti = eval $ applyN (numNodes ti - length stNodes) step st
  where st@(stNodes, _) = initSol ti

initNearNeigh :: TSPInstance -> PartialSolution
initNearNeigh ti =
  case V.uncons (nodes ti) of
    Nothing -> error "empty list of nodes is not allowed"
    Just (h, t) -> (singleton h, t)

stepNearNeigh :: PartialSolution -> PartialSolution
stepNearNeigh sol@(path, rem)
  | V.null rem = sol
  | otherwise = let border = V.last path
                    chosen = V.minimumBy (\n1 n2 -> if distance border n1 <
                                                       distance border n2
                                                    then LT else GT) rem
                in (path V.++ singleton chosen, erase rem chosen)

nearestNeighbour :: ConsHeur
nearestNeighbour = ConsHeur initNearNeigh stepNearNeigh
