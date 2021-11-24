module Main where

import Text.Parsec (parse)

import Lib (parseTSPInstance, nearestNeighbour, solve)

fromRight' :: Either a b -> b
fromRight' (Right a) = a
fromRight' (Left  _) = error "parse error"

main :: IO ()
main = interact ((\d -> show d ++ "\n") . solve nearestNeighbour . fromRight' . parse parseTSPInstance "")
