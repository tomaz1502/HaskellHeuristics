module Main where

import Text.Parsec (parse)

import TSPInstance
import ConsHeur
import Utils

consHeur :: ConsHeur
consHeur = nearestNeighbour

main :: IO ()
main =
    interact ((\d -> show d ++ "\n") .
              solve consHeur .
              fromRight' .
              parse parseTSPInstance "")
