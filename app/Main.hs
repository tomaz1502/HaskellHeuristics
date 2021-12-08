module Main where

import Text.Parsec (parse)

import ConsHeur ( runConsHeur, nearestNeighbour, ConsHeur )
import Neighborhood ( getLocalBest, twoOpt, threeOpt )
import Tour ( evalT, Tour (Tour) )
import TSPInstance ( parseTSPInstance )
import Utils ( fromRight' )
import VND ( vnd )

consHeur :: ConsHeur
consHeur = nearestNeighbour

-- | Use consHeur to solve the given tsp instance. For now, it must come from
--   the standard input. We're using the tsplib standard.
main :: IO ()
main =
    interact ( (\d -> show d ++ "\n") .
              evalT .
              vnd [twoOpt, threeOpt] .
              runConsHeur consHeur .
              fromRight' .
              parse parseTSPInstance "")
