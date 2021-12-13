module Main where

import Text.Parsec (parse)

import ConsHeur ( runConsHeur, nearestNeighbour, ConsHeur )
import Neighborhood ( getLocalBest, twoOpt, threeOpt, getBestNeighbor' )
import Tour ( evalT, Tour (Tour) )
import TSPInstance ( parseTSPInstance )
import Utils ( fromRight', applyN, fixpoint )
import VND ( vnd )
import Grasp (grasp)

consHeur :: ConsHeur
consHeur = nearestNeighbour

main :: IO ()
main =
    interact ( (\d -> show d ++ "\n") .
              evalT .
              grasp twoOpt  .
              fromRight' .
              parse parseTSPInstance "")
