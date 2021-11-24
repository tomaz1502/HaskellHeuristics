module Main where

import Text.Parsec (parse)

import Lib (parseTSPInstance)

fromRight' :: Either a b -> b
fromRight' (Right a) = a
fromRight' (Left  _) = error "parse error"

main :: IO ()
main = interact (show . fromRight' . parse parseTSPInstance "")
