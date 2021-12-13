module Main where

import Text.Parsec (parse)

import ConsHeur ( runConsHeur, nearestNeighbour, ConsHeur )
import Neighborhood ( getLocalBest, twoOpt, threeOpt, getBestNeighbor' )
import Tour ( evalT, Tour (Tour) )
import TSPInstance ( parseTSPInstance )
import Utils ( fromRight', applyN, fixpoint )
import VND ( vnd )
import Grasp (grasp)
import Check (check)

consHeur :: ConsHeur
consHeur = nearestNeighbour

main :: IO ()
main =
    interact ( (\d -> show d ++ "\n") .
              evalT .
              grasp twoOpt  .
              fromRight' .
              parse parseTSPInstance "")

-- main :: IO ()
-- main = interact $
--   \s -> let eti = parse parseTSPInstance "" s
--             ti = fromRight' eti
--             initial = runConsHeur consHeur ti
--             tour = vnd [twoOpt, threeOpt] initial
--             b = check tour ti
--         in show b ++ "\n"
