{-# LANGUAGE RecordWildCards #-}

module ConsHeur where

import           Utils ( applyN, erase )
import           TSPInstance ( TSPInstance(numNodes, nodes, TSPInstance) )
import           Tour ( eval, PartialSolution, Tour(Tour) )
import           Node ( distance )

import           Control.Monad.State
import           Data.Vector (uncons, singleton)
import qualified Data.Vector as V
import           System.Random


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

-- | Nearest Neighbour

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

-- | Random constructive

type Step = State (StdGen, PartialSolution) ()

data RandCons = RandCons { initSolR :: TSPInstance -> PartialSolution
                         , stepR :: Step
                         }

initRandom :: TSPInstance -> PartialSolution
initRandom = initNearNeigh -- welp, its really the same :P

stepRandom :: Step
stepRandom = state $ \(rng, sol) -> ((), go rng sol)
  where go :: StdGen -> PartialSolution -> (StdGen, PartialSolution)
        go rng sol@(Tour path, Tour rem)
          | V.null rem = (rng, sol)
          | otherwise  =
            let border    = V.last path
                dists     = V.map (distance border) rem
                cmin      = minimum dists
                cmax      = maximum dists
                thres     = cmin + alpha * (cmax - cmin)
                cands     = V.filter (\n -> distance border n < thres) rem
                (i, rng') = random rng
                chosen    = cands V.! i
            in (rng', (Tour $ path V.++ singleton chosen, Tour $ erase rem chosen))
        alpha :: Double
        alpha = 0.1

randomConstructive :: RandCons
randomConstructive = RandCons initRandom stepRandom
