{-# LANGUAGE RecordWildCards #-}
module Lib (Node (..), distance, parseNode, parseTSPInstance) where

import Data.Vector hiding ((++))
import Text.Parsec
import Text.Parsec.String

-- | A point in a 2-D plane
data Node =
  Node { xc :: Double
       , yc :: Double
       }

instance Show Node where
  show Node {..} = "(" ++ show xc ++ ", " ++ show yc ++ ")"

distance :: Node -> Node -> Double
distance Node {xc = x1, yc = y1} Node {xc = x2, yc = y2} =
  sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

data TSPInstance =
  TSPInstance { numNodes :: Int
              , instName :: String
              , nodes    :: Vector Node
              }

instance Show TSPInstance where
  show TSPInstance {..} = "name: " ++ instName ++
                          "\nnumNodes: " ++ show numNodes ++
                          "\nnodes: " ++ show nodes

parseNode :: Parser Node
parseNode = do skipMany (noneOf " ")
               skipMany (oneOf " ")
               xc <- toDouble <$> many (noneOf " ")
               skipMany (oneOf " ")
               yc <- toDouble <$> many1 (noneOf " \n")
               return $ Node xc yc
  where toDouble s = read s :: Double

skipLine :: Parser ()
skipLine = do skipMany (noneOf "\n")
              skipMany (oneOf "\n")

parseTSPInstance :: Parser TSPInstance
parseTSPInstance = do skipMany (noneOf " ")
                      skipMany (oneOf " ")
                      nm <- many (noneOf "\n")
                      skipMany (oneOf "\n")
                      skipLine
                      skipLine
                      skipMany (noneOf " ")
                      skipMany (oneOf " ")
                      numNodes <- toInt <$> many (noneOf "\n")
                      skipMany (oneOf "\n")
                      skipLine
                      skipLine
                      nodes <- replicateM numNodes parseNode
                      return $ TSPInstance numNodes nm nodes
  where toInt s = read s :: Int
