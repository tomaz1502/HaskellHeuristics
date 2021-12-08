{-# LANGUAGE RecordWildCards #-}

module ConsHeur where

import           Utils ( applyN, erase )
import           TSPInstance ( TSPInstance(numNodes, nodes) )
import           Tour ( eval, PartialSolution, Tour(Tour) )
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
runConsHeur :: ConsHeur -> TSPInstance -> Tour
runConsHeur ConsHeur {..} ti = fst $ applyN (numNodes ti - length stNodes) step st
  where st@(Tour stNodes, _) = initSol ti

initNearNeigh :: TSPInstance -> PartialSolution
initNearNeigh ti =
  case V.uncons (nodes ti) of
    Nothing     -> error "empty list of nodes is not allowed"
    Just (h, t) -> (Tour $ singleton h, Tour t)

stepNearNeigh :: PartialSolution -> PartialSolution
stepNearNeigh sol@(Tour path, Tour rem)
  | V.null rem = sol
  | otherwise = let border = V.last path
                    chosen = V.minimumBy (\n1 n2 -> if distance border n1 <
                                                       distance border n2
                                                    then LT else GT) rem
                in (Tour $ path V.++ singleton chosen, Tour $ erase rem chosen)

nearestNeighbour :: ConsHeur
nearestNeighbour = ConsHeur initNearNeigh stepNearNeigh
