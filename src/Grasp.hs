module Grasp where

import ConsHeur ( randomConstructive, runRandom )
import TSPInstance (TSPInstance)
import Tour (Tour(Tour))
import Neighborhood ( getLocalBest, Neighborhood )

grasp :: Neighborhood -> TSPInstance -> Tour
grasp n ti =
  let initial = runRandom randomConstructive ti
  in getLocalBest n initial
