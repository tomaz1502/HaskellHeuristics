module Main where

import Text.Parsec (parse)

import TSP
import Node
import ConsHeur
import Utils

main :: IO ()
main = interact ((\d -> show d ++ "\n") . solve nearestNeighbour . fromRight' . parse parseTSPInstance "")
