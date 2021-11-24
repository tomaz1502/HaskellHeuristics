module Main where

import Text.Parsec

import Lib (Node(..), distance, parseNode, parseTSPInstance)
import Data.Either (fromRight)

fromRight' :: Either a b -> b
fromRight' (Right a) = a
fromRight' (Left  _) = error "parse error"

main :: IO ()
main = interact (show . parse parseTSPInstance "")
-- main = do line <- getLine
--           print $ fromRight' $ parse parseNode "" line
