{-# LANGUAGE RecordWildCards #-}

module ConsHeur where

import           Utils
import           TSPInstance
import           Node

import           Data.List          (delete)
import qualified Data.List.NonEmpty as NE

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
initNearNeigh ti = let (h, mt) = NE.uncons (nodes ti)
                   in ([h], maybeNEToList mt)

stepNearNeigh :: PartialSolution -> PartialSolution
stepNearNeigh sol@(_, []) = sol
stepNearNeigh (path, rem@(c1 : cs)) =
        let border = last path
            chosen = foldr (\n1 n2 -> if distance border n1 < distance border n2
                                      then n1 else n2) c1 cs
        in (path ++ [chosen], delete chosen rem)

nearestNeighbour :: ConsHeur
nearestNeighbour = ConsHeur initNearNeigh stepNearNeigh
