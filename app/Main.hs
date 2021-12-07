module Main where

import Text.Parsec (parse)

import TSPInstance
import ConsHeur
import Utils

consHeur :: ConsHeur
consHeur = nearestNeighbour

-- | Use consHeur to solve the given tsp instance. For now, it must come from
--   the standard input. We're using the tsplib standard.
main :: IO ()
main =
    interact ((\d -> show d ++ "\n") .
              solve consHeur .
              fromRight' .
              parse parseTSPInstance "")
