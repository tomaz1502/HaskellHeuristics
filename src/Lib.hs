{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Lib (Node (..), distance, parseNode, parseTSPInstance) where

import           Text.Parsec
import           Text.Parsec.String

import           Control.Monad
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

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
              , nodes    :: NonEmpty Node
              }

instance Show TSPInstance where
  show TSPInstance {..} = "\nname: " ++ instName ++
                          "\nnumNodes: " ++ show numNodes ++
                          "\nnodes: " ++ show nodes ++ "\n"

parseNode :: Parser Node
parseNode = do skipMany (noneOf " ")
               skipMany (oneOf " ")
               xc <- toDouble <$> many (noneOf " ")
               skipMany (oneOf " ")
               yc <- toDouble <$> many1 (noneOf " \n")
               return $ Node xc yc
  where toDouble s = read s :: Double

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
        skipLine = skipMany (noneOf "\n") >> skipMany (oneOf "\n")


type PartialSolution = ([Node], [Node])

eval :: PartialSolution -> Double
eval ([], _)    = error "trying to eval the empty list"
eval (ns@(h : t), _) = let pairs = zip ns (t ++ [h])
                       in  foldr (\(a, b) acc -> distance a b + acc) 0 pairs

data ConsHeur =
  ConsHeur { initSol :: TSPInstance -> PartialSolution
           , step :: TSPInstance -> PartialSolution -> PartialSolution
           }

solve :: TSPInstance -> ConsHeur -> Double
solve ti ConsHeur {..} = eval $ applyN (numNodes ti) (step ti) (initSol ti)

nearestNeighbour :: ConsHeur
nearestNeighbour = ConsHeur initSol step
  where initSol ti = let (h, mt) = NE.uncons (nodes ti) in
                         ([h], maybeNEToList mt)
        step ti = undefined

-- utils
-- sadly, doesn't exist in the prelude :(
applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ a = a
applyN n f a = applyN (n - 1) f (f a)

maybeNEToList :: Maybe (NonEmpty a) -> [a]
maybeNEToList Nothing   = []
maybeNEToList (Just xs) = NE.toList xs
